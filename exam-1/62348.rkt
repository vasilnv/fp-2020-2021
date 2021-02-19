;1
(define (number-valid? n)
  (define (helper num cnt)
    (if (= num 0)
        #t
        (if (= cnt 2)
            #f
            (helper (quotient num 10) (if (= (remainder num 10) 0)
                                          (+ cnt 1)
                                          0)))))
  (helper n 0))


;2
(define (to-bin num)
  (define (helper n mult res)
    (if (= n 0)
        res
        (helper (quotient n 2)
                (* 10 mult)
                (+ res (* mult (remainder n 2)))))
    )
  (helper num 1 0)
  )
;101
;111
(define (set-add set i)
  (let (
        (bin-num (to-bin set))
        )
    (define (helper num mult res elem)
      (if (and (> elem i)
               (= num 0))
          res
          (helper (quotient num 10)
                  (* 2 mult)
                  (+ res (* mult (if (= elem i) 1 (remainder num 10))))
                  (+ elem 1))))
    (helper bin-num 1 0 0)
    ))


(define (valid->nset n)
  (define (helper num mult set curr)
    (if (= num 0)
        (if (> curr 0) (set-add set curr) set)
        (helper (quotient num 10)
                (if (= (remainder num 10) 0) 1 (* mult 10))
                (if (and (= (remainder num 10) 0) (> curr 0)) (set-add set curr) set)
                (if (= (remainder num 10) 0) 0 (+ curr (* mult (remainder num 10))))
                )))
  (if (number-valid? n)
      (helper n 1 0 0)
      #f
      )
  )

;3
(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

(define (less-than-5? x)
  (< x 5))

(define (make-nset a b pred?)
  (accumulate (lambda (x y) (if (pred? x) (set-add y x) y))
              (lambda (x) x)
              0
              a
              (lambda (x) (+ x 1))
              b)
  )
