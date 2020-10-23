(define yes "yaay")
(define no "naaah")

(define (checkGrade x)
   (if (= x 6)
       (yes)
       (no))
)

(define (abso x)
  (if (>= x 0)
       x
       (- x)
  )
  
)

(define (ge x y)
  (or
   (> x y)
   (= x y)
   )
)

(define (le x y)
  (not (> x y))
)

(define x
   (+
    (-(/
     (+ 2 3/16)
     (* 9 2.78)
    ))
    (
     - 6 (/ 5 2)
    )
   )
)

(define y
   
    (/
     (+ 5 1/4
       (- 2
          (- 3
             (+ 6 1/5))))
     (* 3 (- 6 2) (- 2 7))
     )

)

(define (square x) (* x x))

(define e
  (+ (square (square 16)) 95/2)

)

(define (square-sum-bigger-two a b c)
  (cond
    ((and (> a b) (> c b) ) (+ a c))
    ((and (> a c) (> b c) ) (+ a b))
    ((and (> b a) (> c a) ) (+ c b))
    
  )
)

(define (sum-interval start end)
  (define (helper start end result)
     (define res (+ result start))
     (if (= start end)
         res
         (helper (+ start 1) end res))
     )
  
  (helper start end 0)

)
