#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(import scheme)
(cond-expand
 (chicken-4
  (use data-structures extras files irregex posix srfi-1 utils)
  (use matchable)
  (define read-list read-file))
 (chicken-5
  (import (chicken condition)
          (chicken file)
          (chicken format)
          (chicken io)
          (chicken irregex)
          (chicken pathname)
          (chicken port)
          (chicken pretty-print)
          (chicken process-context)
          (chicken string))
  (import matchable srfi-1))
 (else
  (error "Unsupported CHICKEN version.")))

(define (module-body data)
  (cddr data))

(define (module-name data)
  (if (eq? (car data) 'module)
      (cadr data)
      (error "Cannot determine module name.")))

(define (load-definition-files files)
  (let loop ((files files))
    (if (null? files)
      '()
      (append (with-input-from-file (car files) read-list) (loop (cdr files))))))

(define (module-data mod-file)
  (let ((data (with-input-from-file mod-file read-list)))
    (let loop ((data data))
      (if (null? data)
          '()
          (let ((form (car data)))
            (cond ((eq? (car form) 'module)
                   (car data))
                  (else (loop (cdr data)))))))))

(define (exported-definitions mod-data exported)
  (let ((toplevel-defs (toplevel-definitions mod-data)))
    (filter (lambda (def)
              (memq (cadr def) exported))
            toplevel-defs)))

(define (module-exported-symbols data)
  (let ((exported (caddr data)))
    (if (eq? exported '*)
        (toplevel-definitions (cdddr data))
        exported)))

(define (definer? op)
  (memq op
        '(define
          define-parameter
          define-macro
          define-syntax
          define-constant)))

(define (toplevel-definitions mod-data)
  (filter-map
   (lambda (form)
     (let ((maybe-definer (car form)))
       (and (definer? maybe-definer)
            (let ((name (cadr form)))
              (if (pair? name)
                  (car name)
                  name)))))
   mod-data))

(define (format-parameter-value value)
  (if (and (pair? value) (eq? (car value) 'lambda))
      "procedure"
      (string-chomp (with-output-to-string (cut pp value)))))

(define (format-definitions mod-data exported)

  (define (exported? symbol)
    (memq (if (pair? symbol) (car symbol) symbol) exported))

  (filter-map
   (lambda (form)
     (if (and (list? form)
              (= (length form) 3)
              (eq? (car form) 'define)
              (symbol? (caddr form))
              (exported? (cadr form)))
         (conc "<procedure>(" (cadr form) " ...)</procedure>")
         (match form
           (('define (proc ...) body ...)
            (and (exported? proc)
                 (conc "<procedure>" proc "</procedure>")))

           (('define (proc . rest) body ...)
            (and (exported? proc)
                 (conc "<procedure>" (cons proc rest) "</procedure>")))

           ((or ('define proc ('lambda args body ...))
                ('define proc ('foreign-lambda args body ...))
                ('define proc ('foreign-lambda* args body ...))
                ('define proc ('foreign-safe-lambda* args body ...)))
            (and (exported? proc)
                 (conc "<procedure>(" proc " "
                       (if (symbol? args)
                           args
                           (irregex-replace "\\)$"
                                            (irregex-replace "^\\("
                                                             (->string args)
                                                             "")
                                            ""))
                       ")</procedure>")))

           (('define param ('make-parameter value))
            (and (exported? param)
                 (conc "<parameter>(" param " [default=" (format-parameter-value value) "])</parameter>")))

           (('define identifier value)
            (and (exported? identifier)
                 (match value
                        (('let _ ('lambda (args) body ...))
                         (conc "<procedure>(" identifier " " args ")</procedure>"))
                        (('let _ ('lambda args body ...))
                         (conc "<procedure>(" identifier " . " args ")</procedure>"))
                        (('let* _ ('lambda (args) body ...))
                         (conc "<procedure>(" identifier " " args ")</procedure>"))
                        (('let* _ ('lambda args body ...))
                         (conc "<procedure>(" identifier " . " args ")</procedure>"))
                        (else (conc "[constant] '''" identifier "''' " value)))))

           (('define-constant const value)
            (and (exported? const)
                 (conc "[constant] '''" const "''' " value)))

           (('define-parameter param value)
            (and (exported? param)
                 (conc "<parameter>(" param " [default=" (format-parameter-value value) "])</parameter>")))

           ((or ('define-syntax identifier (syntax-rules ...))
                ('define-syntax identifier (er-macro-transformer ...)))
            (and (exported? identifier)
                 (conc "<macro>(" identifier " ...)</macro>")))

           (('define-record record args ...)
            (and (exported? record)
                 (conc "[record] ('''" record "''' " (string-intersperse (map ->string args)) ")")))

           (('include files ...)
            (format-definitions (load-definition-files files) exported))

           (else #f))))
   mod-data))

(define (meta-data-obj meta-file)
  (let ((meta-data (with-input-from-file meta-file read)))
    (lambda (field #!optional (default "") as-list?)
      (or (and-let* ((val (alist-ref field meta-data)))
            (if as-list?
                val
                (car val)))
          default))))

(define (format-requirements requirements)
  (string-intersperse
   (map (lambda (req)
          (let ((link-egg
                 (lambda (egg)
                   (conc "[[http://wiki.call-cc.org/egg/" egg "|" egg "]]"))))
            (string-append
             "* "
             (if (list? req)
                 (conc (link-egg (car req)) " version " (cadr req))
                 (link-egg req)))))
        requirements)
   "\n"))

(define (print-api definitions)
  (for-each (lambda (def)
              (if (list? def)
                  (print-api def)
                  (print def "\n")))
            definitions))

(define (initial-wiki-doc module-file meta-file)
  (let* ((data (module-data module-file))
         (meta-data (meta-data-obj meta-file))
         (author (meta-data 'author))
         (description (meta-data 'synopsis))
         (license (meta-data 'license))
         (requirements (cond-expand
                        (chicken-4
                         (or (meta-data 'depends #f #t)
                             (meta-data 'needs #f #t)
                             '()))
                        (chicken-5
                         (or (meta-data 'dependencies #f #t)
                             (meta-data 'build-dependencies #f #t)
                             '())))))
    (print "== " (module-name data) "\n")
    (print "=== Author\n\n" author "\n\n")
    (print "=== Repository\n\n")
    (print "=== Requirements\n\n"
           (if (null? requirements)
               "None"
               (format-requirements requirements))
           "\n\n")
    (print "=== Description\n\n" description "\n\n")
    (print "=== API\n")
    (print-api (format-definitions (module-body data)
                                   (module-exported-symbols data)))
    (print "\n=== License\n\n" license "\n\n")
    (print "=== Version history\n")))

(define (usage #!optional exit-code)
  (let ((port (if (zero? exit-code)
                  (current-output-port)
                  (current-error-port)))
        (program (pathname-strip-directory (program-name))))
    (fprintf port "~a <egg dir>\n" program)
    (when exit-code (exit exit-code))))

(let ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))
  (when (member (car args) '("-h" "-help" "--help"))
    (usage 0))
  (let* ((eggdir (last args))
         (meta-file (handle-exceptions
                     exn
                     (begin
                       (fprintf (current-error-port) "Could not find a ~a file.\n"
                                (cond-expand
                                 (chicken-4 ".meta")
                                 (chicken-5 ".egg")))
                       (exit 1))
                     (car (glob (make-pathname eggdir
                                               (cond-expand
                                                (chicken-4 "*.meta")
                                                (chicken-5 "*.egg")))))))
         (module-file (pathname-replace-extension meta-file "scm")))
    (initial-wiki-doc module-file meta-file)))
