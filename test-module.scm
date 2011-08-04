(module test
  (thing1 thing2 thing3 thing4 thing5 thing6 thing7 thing8
   thing9 thing10 thing11 thing12 thing13 thing14 thing15
   thing16 thing17 thing18 thing19 thing20)
  
(define (thing1 a b)
  (+ a b))

(define thing2
  (lambda (a b)
    (+ a b)))

(define thing3 (make-parameter #f))

(define-syntax thing4
  (syntax-rules ()
    ((_ a b) (+ a b))))

(define (thing5 . more)
  abc
  (apply + rest))

(define (thing6 #!optional a b)
  abc
  (+ a b))

(define (thing7 a b #!key c)
  abc
  (+ a b c))

(define (thing8 a b . rest)
  abc
  (apply + rest))

(define-parameter thing9 #f)

(define-constant thing10 0)

(define thing11
  (lambda args
    (apply + args)))

(define (thing12 a b #!key (c 3) (d 0))
  (+ a b))

(define thing13
  (let ()
    (lambda args
      abc
      (apply + args))))
    
(define thing14
  (let ()
    (lambda (args)
      abc
      (apply + args))))
    
(define-record thing15 a b c)

(define (thing16 a b #!optional c (d 0))
  (+ a b))

(define thing17 another-thing)

(define thing18 'a-symbol)

(define thing19
  (lambda (a c . b)
    abc
    (+ a b)))

(define thing20
  (lambda (a c #!optional b)
    abc
    (+ a b)))

) ; end module
