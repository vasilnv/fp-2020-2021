#lang racket/base

(provide (all-defined-out))

(define (is-tree? t)
  (or (null? t)
      (and (pair? t)
           (= (length t) 3))
           (is-tree? (cadr t))
           (is-tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))      
(define (make-leaf root) (make-tree root empty-tree empty-tree)) 
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)



(define (height tree)
  (define (helper t cnt)
    (if (empty-tree? t)
        cnt
        (max (helper (left-tree t) (+ cnt 1))
             (helper (right-tree t) (+ cnt 1))
             )))
  (helper tree 0))

(define (balanced? tree)
  (define (helper t)
    (if (empty-tree? t)
        #t
        (if (< (abs (- (height (left-tree t))
                       (height (right-tree t)))) 2)
            (and (helper (left-tree t)) (helper (right-tree t)))
            #f
            )))
  (helper tree))

(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
            (list (root-tree t))
            (tree->list (right-tree t)))))

(define (ordered? tree)
  (let (
        (l (tree->list tree))
        )
    (define (helper l)
      (if (null? (cdr l))
          #t
          (if (>= (car l) (cadr l))
              #f
              (helper (cdr l)))))
    (if (null? l)
        #t
        (helper l)
        )
    ))

(define (stack-push stack el)
  (string-append stack (number->string el)))

(define (stack-peek stack len)
  (string->number (string (string-ref stack (- len 1)))))

(define (stack-pop stack len)
  (define (helper res i)
    (if (= i (- len 1))
        res
        (helper (string-append res (string (string-ref stack i))) (+ i 1))))
  (helper "" 0)
  )

(define (rm-whitespaces str)
  (let (
        (len (string-length str))
        )
    (define (helper str res i)
      (if (= i len)
          res
          (helper str
                  (if (and (> i 0) (char=? (string-ref str i) #\space) (char=? (string-ref str (- i 1)) #\space))
                      res
                      (string-append res (string (string-ref str i))))
                  (+ i 1))))
    (helper str "" 0)
    )
)

(define (digits-ok str)
  (let (
        (len (string-length str))
        )
    (define (helper i)
      (cond ((>= i (- len 1)) #t)
            ((and (> i 0) (< i len) (char=? (string-ref str i) #\space) (char-numeric? (string-ref str (+ i 1))) (char-numeric? (string-ref str (- i 1)))) #f)
            (else (helper (+ i 1)))
            )
    )
    (helper 1)
    ))

(define (tree? input)
  (let* (
        (str (rm-all-spaces input))
        (len (string-length str))
        )
    (define (helper score i stack stack-len num-scopes )
      (cond ((= i len) (if (and (= score 1) (= stack-len 0) (= num-scopes 0)) #t #f))
            ((and (char=? (string-ref str i) #\{) (char-numeric? (string-ref str (+ i 1)))) (helper 1 (+ i 1) (stack-push stack (+ score 1)) (+ stack-len 1) (+ num-scopes 1)))
            ((char-numeric? (string-ref str i)) (helper score (+ i 1) stack stack-len num-scopes))
            ((char=? (string-ref str i) #\*) (helper (+ score 1) (+ i 1) stack stack-len num-scopes))
            ((char=? (string-ref str i) #\}) (if (= score 3)
                                            (helper (stack-peek stack stack-len) (+ i 1) (stack-pop stack stack-len) (- stack-len 1) (- num-scopes 1))
                                            #f
                                            ))
            (else #f)
            ))
    (if (digits-ok (rm-whitespaces input))
        (helper 0 0 "" 0 0)
        #f)
))

(define (tree-valid? tree)
  (if (null? tree)
      #t
      (if (and (pair? tree) (= (length tree) 3))
          (and (tree-valid? (cadr tree)) (tree-valid? (caddr tree)))
          #f
          )))

(define (tree->string tree)
  (define (helper curr-tree res)
    (if (null? curr-tree)
        (string-append res "*")
        (string-append res "{"
                       (if (pair? (root-tree curr-tree))
                           (helper (root-tree curr-tree) res)
                           (number->string (root-tree curr-tree)))
                       " "
                       (helper (left-tree curr-tree) res)
                       " "
                       (helper (right-tree curr-tree) res) "}")))
  (if (tree-valid? tree)
      (helper tree "")
      #f)
      )

(define (find-end str j st st-len)
        (if (= st-len 0)
            (- j 1)
            (if (char=? (string-ref str j) #\{)
                (find-end str (+ j 1) (stack-push st 1) (+ st-len 1))
                (if (char=? (string-ref str j) #\})
                    (find-end str (+ j 1) (stack-pop st st-len) (- st-len 1))
                    (find-end str (+ j 1) st st-len)
                ))
        
        ))

(define (extract-num str)
  (define (helper res str i)
    (if (char-numeric? (string-ref str i))
        (helper (+ (* res 10) (string->number (string (string-ref str i)))) str (+ i 1))
        res
        )
  )
  (helper 0 str 1)
  )

(define (num-digits num)
  (define (helper num-left l)
  (if (= num-left 0)
      l
      (helper (quotient num-left 10) (+ l 1))))
  (helper num 0))

(define (extract-str str i ei)
  (let (
        (len (string-length str))
        )
    (define (helper j res)
      (if (= j ei)
          res
          (helper (+ j 1) (string-append res (string (string-ref str j))))))
    (helper i "")
    ))

(define (rm-all-spaces str)
  (let (
        (len (string-length str))
        )
    (define (helper str res i)
      (if (= i len)
          res
          (helper str
                  (if (char=? (string-ref str i) #\space) 
                      res
                      (string-append res (string (string-ref str i))))
                  (+ i 1))))
    (helper str "" 0)
    )
)

(define (string->tree input)
    (let* (
        (str (rm-all-spaces input))
        (len (string-length str))
        )

      (define (helper str)
        (let* (
               (root (extract-num str))
               (num-len (num-digits root))
               (s (extract-str str (+ num-len 1) (string-length str)))
               (s-len (string-length s))
               )
          (list root
                (if (char=? (string-ref s 0) #\{)
                    (helper (extract-str s 0 (+ (find-end s 1 "1" 1) 1)))
                    '())
                (if (char=? (string-ref s 0) #\{)
                    (if (and (< (find-end s 1 "1" 1) (- s-len 1)) (char=? (string-ref s (+ (find-end s 1 "1" 1) 1)) #\{))
                        (helper (extract-str s (+ (find-end s 1 "1" 1) 1) s-len))
                        '())
                    (if (char=? (string-ref s 1) #\{)
                        (helper (extract-str s 1 (+ (find-end s 2 "1" 1) 1)))
                        '()
                        )
                
          
                
          ))))
      (if (tree? input)
          (helper str)
          #f
          )
  ))








