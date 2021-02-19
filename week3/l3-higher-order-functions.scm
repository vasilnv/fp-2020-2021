(define (1+ n)
  (+ n 1))

(define (prn result i)
  (display i))

(define (accumulate-i init op a next b)
  (define (loop result i)
    (if (<= i b)
        (loop (op result i) (next i))
        result
        )
    )
  (loop init a)
  )

(define (sum-if-even result i)
  (if (even? i) (+ result i) result))

(define (accumulate-rec init op a next b)
  (define (loop i)
    (if (<= i b)
        (op (loop (next i)) i)
        init
        )
    )
  (loop a)
  )

(accumulate-rec 0 prn 1 1+ 10)
(display "goshooo")
(accumulate-i 0 prn 1 1+ 10)
(accumulate-rec 0 - 10 1+ 20)
(accumulate-i 0 - 10 1+ 20)

(define (accumulate-r init op a next b)
  (define (loop i)
    (if (<= i b)
        (op i (loop (next i)))
        init
        )
    )
  (loop a)
  )

(accumulate-r 0 - 10 1+ 20)

;right-associative ----> op i calculated-result 
(define (accumulate op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a)
  )

(define (id x) x)

(define (sum-int a b)
  (accumulate + id 0 a 1+ b))

(define (2+ n) (+ n 2))

(define (sum-even a b)
  (accumulate +
              id
              0
              (if (even? a) a (+ a 1))
              2+
              b)
  )
; left-associative ---> op result i - we want 
(define (accumulate-i op term init a next b)
  (define (loop i result)
    (if (> i b)
        result
        (loop (next i) (op result (term i)))
    )
  )
  (loop a init)
  
  )


(define (prime? a)
  (define (helper num curr)
    (if (= curr num)
        #t
        (if (= (remainder num curr) 0)
            #f
            (helper num (+ curr 1)))))
  (if (= a 1)
      #t
      (helper a 2))
  )

(define (next-prime a)
  (if (prime? (+ a 1))
      (+ a 1)
      (next-prime (+ a 1))
      )
  )

(define (sum-prime a b)
  (accumulate + id 0
              (if (prime? a) a (next-prime a))
              next-prime
              b))