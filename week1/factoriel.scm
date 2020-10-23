(define (factoriel n)
  (define (helper res curr)
    (if (or (= curr 1) (= curr 0))
        res
        (helper (* res curr) (- curr 1))))
  (helper 1 n)
  )

;using cond
(define (fact n)
  (define (helper res curr)
    (cond ((= curr 1) res)
          ((= curr 0) res)
          (else (helper (* res curr) (- curr 1)))))
  (helper 1 n))
