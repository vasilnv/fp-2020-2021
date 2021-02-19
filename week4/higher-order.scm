(define (accumulate op term init a next b)
  (define (loop i)
    (if (> i b)
      init
      (op (term i) (loop (next i)))))
  (loop a))

(define (sum a b)
  (accumulate +
              (lambda (x) x)
              0
              a
              (lambda (x) (+ x 1))
              b))

(define (!! n)
  (accumulate *
              (lambda (x) x)
              1
              (if (even? n) 2 1)
              (lambda (x) (+ x 2))
              n
              ))

(define (count p? a b)
  (accumulate +
              (lambda (x) (if (p? x) 1 0))
              0
              a
              (lambda (x) (+ x 1))
              b
              ))


(define (all? p? a b)
  (accumulate (lambda (x y) (and x y))
              p?
              #t
              a
              (lambda (x) (+ x 1))
              b
              ))

(define (any? p? a b)
  (accumulate (lambda (x y) (or x y))
              p?
              #f
              a
              (lambda (x) (+ x 1))
              b))

(define (is-1-to-3? n)
  (if (or (= n 3) (= n 2) (= n 1))
      #t
      #f))

;задача 2

(define (fact n)
  (accumulate *
              (lambda (x) x)
              1
              1
              (lambda (x) (+ x 1))
              n))


(define (nchk n k)
  (/ (accumulate *
              (lambda (x) x)
              1
              (+ (- n k) 1)
              (lambda (x) (+ x 1))
              n)
     (accumulate *
                 (lambda (x) x)
                 1
                 1
                 (lambda (x) (+ x 1))
                 k)
     
     ))

(define (nchk* n k)
  (accumulate *
              (lambda (x) (/ (+ (- n x) 1) (+ (- k x) 1)))
              1
              1
              (lambda(x) (+ x 1))
              k
   ))

;задача 3
(define (2^ n)
  (accumulate *
              (lambda (x) 2)
              1
              1
              (lambda (x) (+ x 1))
              n))

(define (2^* n)
  (accumulate +
              (lambda (x) (nchk* n x))
              0
              0
              (lambda (x) (+ x 1))
              n)
  )

;задача 4

(define (divisors-sum n)
  (accumulate +
              (lambda (x) (if (= (remainder n x) 0) x 0))
              0
              1
              (lambda (x) (+ x 1))
              n

   ))

;задача 7
(define (prime? n)
  (accumulate (lambda (x y) (not (= (remainder n x) 0)))
              (lambda (x) x)
              #f
              2
              (lambda (x) (+ x 1))
              (- n 1)
              )
  )
