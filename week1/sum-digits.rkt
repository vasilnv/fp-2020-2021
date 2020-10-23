(define (count-digits number)
   (define (helper num cnt)
     (if (= (quotient num 10) 0)
         cnt
         (+ cnt 1)
     )

   )
   (helper number 1)
)