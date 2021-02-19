(define (accumulate op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)))
        init))
  (loop a)
  )

(define (to-bin n)
  (define (get-last-bin-digit n) (remainder n 2))

  (define (drop-last-bin-digit n) (quotient n 2 ))

  (define (loop num mult bin)
    (if (= num 0)
        bin
        (loop (drop-last-bin-digit num)
              (* 10 mult)
              (+ bin
                 (* mult (get-last-bin-digit num)))))
    )

  (loop n 1 0)
 )

(define (to-dec n)
  (define (loop bin mult dec)
    (if (= bin 0)
        dec
        (loop (quotient bin 10)
              (* 2 mult)
              (+ dec
                 (* mult (remainder bin 10)))))
    )

  (loop n 1 0)
  )

(define (set-add set elem)
  (let (
        (bin-set (to-bin set))
        )
    (define (loop bin mult res i)
      (if (and (= bin 0) (> i elem))
          res
          (loop (quotient bin 10)
                (* 2 mult)
                (+ res (* mult (if (= i elem) 1 (remainder bin 10))))
                (+ i 1)
                )))
     (loop bin-set 1 0 0)))

(define (set-remove set elem)
    (let (
        (bin-set (to-bin set))
        )
    (define (loop bin mult res i)
      (if (and (= bin 0) (> i elem))
          res
          (loop (quotient bin 10)
                (* 2 mult)
                (+ res (* mult (if (= i elem) 0 (remainder bin 10))))
                (+ i 1)
                )))
     (loop bin-set 1 0 0)))

(define (set-contains? set elem)
    (let (
        (bin-set (to-bin set))
        )
    (define (loop bin i)
      (if (= bin 0)
          #f
          (if (= i elem)
              (if (= (remainder bin 10) 1)
                  #t
                  #f)
              (loop (quotient bin 10)
                        (+ i 1)))))
     (loop bin-set 0)))

(define (set-empty? set)
  (if (= set 0)
      #t
      #f))

(define (set-size set)
  (let (
        (bin-set (to-bin set))
       )

    (define (loop bin cnt)
      (if (= bin 0)
          cnt
          (loop (quotient bin 10) (if (= (remainder bin 10) 1) (+ cnt 1) cnt))
          ))
    (loop bin-set 0)
    ))

(define (set-intersect s1 s2)
  (let (
        (bin-set1 (to-bin s1))
        (bin-set2 (to-bin s2))
        )
    (define (loop set1 set2 intersection mult)
      (if (or (= set1 0) (= set2 0))
          intersection
          (loop (quotient set1 10)
                (quotient set2 10)
                (if (and (= (remainder set1 10) 1) (= (remainder set2 10) 1))
                    (+ intersection mult)
                    intersection)
                (* 2 mult))))
    (loop bin-set1 bin-set2 0 1)))

(define (set-union s1 s2)
  (let (
        (bin-set1 (to-bin s1))
        (bin-set2 (to-bin s2))
        )
    (define (loop set1 set2 union mult)
      (if (and (= set1 0) (= set2 0))
          union
          (loop (quotient set1 10)
                (quotient set2 10)
                (if (or (= (remainder set1 10) 1) (= (remainder set2 10) 1))
                    (+ union mult)
                    union)
                (* 2 mult))))
    (loop bin-set1 bin-set2 0 1)))

(define (set-difference s1 s2)
  (let (
        (bin-set1 (to-bin s1))
        (bin-set2 (to-bin s2))
        )
    (define (loop set1 set2 difference mult)
      (if (= set1 0)
          difference
          (loop (quotient set1 10)
                (quotient set2 10)
                (if (and (= (remainder set1 10) 1) (= (remainder set2 10) 0))
                    (+ difference mult)
                    difference)
                (* 2 mult))))
    (loop bin-set1 bin-set2 0 1)))

;for test purposes
(define (w i)
  (cond ((= i 0) 10)
        ((= i 1) 20)
        ((= i 2) 30)
        (else 1000)))


;for test purposes
(define (p i)
  (cond ((= i 0) 60)
        ((= i 1) 100)
        ((= i 2) 120)
        (else 1)))

(define (max_ a b)
  (if (> a b)
      a
      b))

(define (bigger? a b)
  (if (> a b)
      #t
      #f))


(define (knap c n w p)
  (if (or (= c 0) (= n 0))
      0
      (if (> (w (- n 1)) c)
          (knap c (- n 1) w p)
          (max_ (+ (p (- n 1)) (knap (- c (w (- n 1))) (- n 1) w p)) (knap c (- n 1) w p))))
  
  )

(define (knapsack c n w p)
  (let (
        (prices (knap c n w p))
        )
    (define (helper c n val w p result)
      (if (< val 0)
          0
          (if (= val 0)
              result
              (if (or (= c 0) (= n 0))
                  0
                  (if (> (w (- n 1)) c)
                      (helper c (- n 1) val w p result)
                      (max_ (helper (- c (w (- n 1))) (- n 1) (- val (p (- n 1))) w p (set-add result (- n 1)))
                       (helper c (- n 1) val w p result)))))))

    (helper c n prices w p 0)
    ))

   
(define (generate-set-indexes set)
  (accumulate (lambda (i j) (if (set-contains? set i) (if (= i (set-size set)) (display i) (begin (display ",") (display i)))))
              (lambda (i) i)
    )
    (- (helper c n w p 0) 
              (display "{")
              0
              (lambda (i) (+ i 1))
              (set-size set))
  (display "}"))

