(define square (lambda (x) (* x x)))

((lambda (x) (* x x)) 100)

(define (f x)
  (lambda (y) (+ x y)))

(define g (f 3))

(define (derive f dx)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
       dx
       )
    )
  )

(define (f x) (+ (* x x) (* 2 x)))

(define (compose f g)
  (lambda (x)
    (f (g x))
    )
  )

(define sq2 (compose square square))

(define (repeated f N)
  (cond ((= N 0) (lambda (x) x))
        ((= N 1) f)
        (else (compose f
                       (repeated f (- N 1)))
        )
  )
)

(define dx 0.0000001)

(define (simple-derive f)
  (derive f dx))

(define (deriveN f n)
  ( (repeated simple-derive n) f)
)

