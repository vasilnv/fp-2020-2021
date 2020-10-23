(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.000001))

(define (square x)
  (* x x))

(define (sqr x)
  (sqrt-iter 1.0 x))

;improved for smaller numbers
(define (my-square x)
  (define (helper guess x last)
    (if (good-enough? guess (square last))
        guess
        (helper (improve guess x) x guess)))
  (helper 1.0 x 0.0))