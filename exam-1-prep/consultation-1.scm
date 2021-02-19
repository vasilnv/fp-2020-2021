(define (accumulate op term init a next b)
  (define (loop i)
    (if (> i b)
        init
        (op (term i) (loop (next i))))
    )
  (loop a)
  )

(define (num-length num)
  (define (helper n cnt)
    (if (= n 0)
        cnt
        (helper (quotient n 10) (+ cnt 1))))
  (helper num 0))

(define (^ p q)
  (define (helper res i)
    (if (= i q)
        res
        (helper (* res p) (+ i 1))))
  (helper 1 0)
  )

(define (rev num)

  (accumulate (lambda (i res) (+ (* res 10) (remainder (quotient num (^ 10 (- (num-length num) i))) 10)))
              (lambda (i) i)
              0
              1
              (lambda (i) (+ i 1))
              (num-length num)))


(define (meet-twice f g a b)
  (define (helper i cnt)
    (if (> i b)
        (if (= cnt 2)
            #t
            #f)
        (if (= cnt 2)
            #t
            (if (= (f i) (g i))
                (helper (+ i 1) (+ cnt 1))
                (helper (+ i 1) cnt))))
    )
  (helper a 0)
  )

(define (middle-digit num)
  (let (
        (mid (if ( = (remainder (num-length num) 2) 0)
                 (quotient (num-length num) 2)
                 (quotient (num-length num) 2)))
        )
    (remainder (quotient num (expt 10 mid)) 10)
    )
  )

(define (repeat n f)
  (accumulate (lambda (x y) (f y))
              (lambda (x) x)
              (lambda (x) (f x))
              1
              (lambda (x) (+ x 1))
              n))

(define f (repeat 5 (lambda (x) (+ x 1))))
