(define (fib num)
  (define (helper x x-1 cnt)
    (if (= cnt num)
        x
        (helper (+ x x-1) x (+ cnt 1))))
  (helper 1 1 2))