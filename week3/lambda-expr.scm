(define (constantly c)
  (lambda (x) c))

(define forever-21 (constantly 21))

(define (flip f)
  (lambda (x y) (f y x)))

(define f1 (flip -))

(define (complement p)
  (lambda (x) (not (p x))))

(define (less-than-5? x) (< x 5))

(define f2 (complement less-than-5?))

(define (compose f g)
  (lambda (x) (f (g x))))

(define f3 (compose (lambda (x) (+ x 1)) (lambda (x) (* x x)))) ; ((x^2)+1)

(define (repeat n f)
  (if (= n 0) (lambda (x) x))
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeat (- n 1) f))
      ))




(define dx 0.000001)

(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx )))

(define f-prim (derive (lambda (x) (+ (* x x) (* 2 x)))))

(define (derive-n f n)
  ((repeat n derive) f))

(define f-n (derive-n (lambda (x) (+ (* x x x) (* 2 x))) 2))