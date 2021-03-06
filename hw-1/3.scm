(define (rem-whitespaces expr)
  (define (helper expr i res)
    (if (= i (string-length expr))
        res
        (helper expr (+ i 1) (if (not (char-whitespace? (string-ref expr i)))
                                 (string-append res (string (string-ref expr i)))
                                 (string-append res "")) )
        )
    )
  (helper expr 0 ""))

(define (char-operation? c)
  (cond ((char=? c #\-) #t)
        ((char=? c #\+) #t)
        ((char=? c #\^) #t)
        ((char=? c #\*) #t)
        ((char=? c #\/) #t)
        (else #f)))


;what-was-before = 0 number
;                = 1 number + whitespace
;                = 2 operation
;                = 3 operation + whitespace
(define (expr-valid? expr)
  (define (helper expr i what-was-before result)
    (if (= i (string-length expr))
        result
        (if (= what-was-before 0)
            (cond ((char-numeric? (string-ref expr i)) (helper expr (+ i 1) 0 #t))
                  ((char-whitespace? (string-ref expr i)) (helper expr (+ i 1) 1 #t))
                  ((char-operation? (string-ref expr i)) (helper expr (+ i 1) 2 #t))
                  (else #f)
                  )
        (if (= what-was-before 1)
            (cond ((char-numeric? (string-ref expr i)) #f)
                  ((char-whitespace? (string-ref expr i)) (helper expr (+ i 1) 1 #t))
                  ((char-operation? (string-ref expr i)) (helper expr (+ i 1) 2 #t))
                  (else #f)
                  )
        (if (= what-was-before 2)
            (cond ((char-numeric? (string-ref expr i)) (helper expr (+ i 1) 0 #t))
                  ((char-whitespace? (string-ref expr i)) (helper expr (+ i 1) 3 #t))
                  ((char-operation? (string-ref expr i)) #f)
                  (else #f)
                  )
        (if (= what-was-before 3)
            (cond ((char-numeric? (string-ref expr i)) (helper expr (+ i 1) 0 #t))
                  ((char-whitespace? (string-ref expr i)) (helper expr (+ i 1) 3 #t))
                  ((char-operation? (string-ref expr i)) #f)
                  (else #f)
                  )
            ))))
        )
    )
  (if (and (> (string-length expr) 0)
       (char-operation? (string-ref expr (- (string-length expr) 1))))
      #f
      (helper expr 0 2 #t))
  )


(define (stack-peek stack)
  (if (> (string-length stack) 0)
      (string-ref stack (- (string-length stack) 1))
      #\,)
  )

(define (stack-peek-num stack)
  (let (
        (rev-stack (stack-rev stack))
        )
    (define (helper stack output i)
      (if (or (>= i (string-length stack))
              (char=? (string-ref stack i) #\,)
              )
          output
          (helper stack (string-append output (string (string-ref stack i))) (+ i 1))))
    (stack-rev (helper rev-stack "" 0)))
  )



(define (stack-pop stack)
  (define (helper result i stack)
    (if (= i (- (string-length stack) 1))
        result
        (helper (string-append result (string (string-ref stack i))) (+ i 1) stack )))
  (helper "" 0 stack)
  )

(define (stack-pop-num stack)
  (let (
        (rev-stack (stack-rev stack))
        )
    (define (helper stack output i)
      (if (>= i (string-length stack))
          output
          (helper stack (string-append output (string (string-ref stack i))) (+ i 1))))
    (stack-rev (helper rev-stack "" (+ (string-length (stack-peek-num stack)) 1) )))
  )

(define (stack-pop*/ stack)
  (define (helper result i stack)
    (if (or (= (string-length result) (string-length stack))
            (or (char=? (string-ref stack i) #\+)
                (char=? (string-ref stack i) #\-)))
        result
        (helper (string-append result (string (string-ref stack i))) (+ i 1) stack )
        )
    )
  (helper "" 0 (stack-rev stack) )
  )

(define (stack-pop-left*/ stack)
  (let(
      (start-pos (string-length (stack-pop*/ stack)))
      )
    (define (helper result stack i)
      (if (= i (- (string-length stack) 1))
          (string-append result (string (string-ref stack i)))
          (helper (string-append result (string (string-ref stack i)))
                  stack
                  (+ i 1)
                  ))

      )
    (if (< start-pos (string-length stack))
        (stack-rev (helper "" (stack-rev stack) start-pos))
        ""
        )
    
  )
  )

(define (absolute num)
  (if (< num 0)
      (- num)
      num))

(define (stack-push stack operation)
    (if (char? operation)
        (string-append stack (string operation))
        (string-append stack  "," (number->string operation))
        )
  )


(define (stack-rev stack)
  (define (helper s result i)
    (if (= i 0)
        (string-append result (string (string-ref s i)))
        (helper s
                (string-append result (string (string-ref s i)))
                (- i 1)))
    )
  (if (> (string-length stack) 0)
      (helper stack "" (- (string-length stack) 1))
      "")
  )

(define (expr-rp expr)
  (let (
        (expression (rem-whitespaces expr))
        )
    (define (helper i expr output stack)
      (if (= i (string-length expr))
             (string-append output "," (stack-rev stack))
             (helper (+ i 1)
                     expr
                     (cond((char-numeric? (string-ref expr i)) (string-append output (string (string-ref expr i))))
                          ((char-operation? (string-ref expr i)) (string-append output  "," (if (and (char=? (string-ref expr i) #\^)
                                                                                                    (char=? (stack-peek stack) #\^))
                                                                                             (string (stack-peek stack))
                                                                                             (if(and (or (char=? (string-ref expr i) #\*)
                                                                                                         (char=? (string-ref expr i) #\/))
                                                                                                     (or (char=? (stack-peek stack) #\*)
                                                                                                         (char=? (stack-peek stack) #\/)
                                                                                                         (char=? (stack-peek stack) #\^)))
                                                                                                (stack-pop*/ stack)
                                                                                                (if(or (char=? (string-ref expr i) #\+)
                                                                                                       (char=? (string-ref expr i) #\-))
                                                                                                   (stack-rev stack)
                                                                                                   ""))))))
                     (cond((char-numeric? (string-ref expr i)) stack)
                          ((char-operation? (string-ref expr i)) (if (and (char=? (string-ref expr i) #\^)
                                                                          (char=? (stack-peek stack) #\^))
                                                                                             (stack-push (stack-pop stack) #\^)
                                                                                             (if(and (or (char=? (string-ref expr i) #\*) (char=? (string-ref expr i) #\/))
                                                                                                     (or (char=? (stack-peek stack) #\*)
                                                                                                         (char=? (stack-peek stack) #\/)
                                                                                                         (char=? (stack-peek stack) #\^)))
                                                                                                (stack-push (stack-pop-left*/ stack) (string-ref expr i))
                                                                                                (if(or (char=? (string-ref expr i) #\+)
                                                                                                       (char=? (string-ref expr i) #\-))
                                                                                                   (stack-push "" (string-ref expr i))
                                                                                                   (stack-push stack (string-ref expr i)))))))
                     )
             )
      )
    (if (expr-valid? expr)
        (helper 0 expression "" "")
        #f)
    
      )
    )

(define (convert-to-int c)
  (cond ((char=? c #\0) 0 )
        ((char=? c #\1) 1 )
        ((char=? c #\2) 2 )
        ((char=? c #\3) 3 )
        ((char=? c #\4) 4 )
        ((char=? c #\5) 5 )
        ((char=? c #\6) 6 )
        ((char=? c #\7) 7 )
        ((char=? c #\8) 8 )
        ((char=? c #\9) 9 )))

(define (convert-to-num str)
  (string->number str)

    )

(define (convert-to-char c)
  (cond ((= c 0) #\0 )
        ((= c 1) #\1 )
        ((= c 2) #\2 )
        ((= c 3) #\3 )
        ((= c 4) #\4 )
        ((= c 5) #\5 )
        ((= c 6) #\6 )
        ((= c 7) #\7 )
        ((= c 8) #\8 )
        ((= c 9) #\9 )
        ((= c "/") #\/ )
        ((= c "-") #\- )

        ))


(define (convert-to-operation c)
  (cond ((char=? c #\+) + )
        ((char=? c #\-) - )
        ((char=? c #\*) * )
        ((char=? c #\/) / )
        ((char=? c #\^) ^ )))

(define (^ n1 n2)
  (define (helper n1 n2 i result)
    (if (= n2 i)
        result
        (helper n1 n2 (+ i 1) (* result n1))))
  (helper n1 n2 0 1))

(define (expr-eval expr)
  (let (
        (rpn (expr-rp expr))
        )
  (define (helper rpn num-index num stack i)
    (if (= i (string-length rpn))
           (stack-peek-num stack)
           (cond ((char=? (string-ref rpn i) #\,) (helper rpn 0 0 (stack-push stack num) (+ i 1)))
                 ((char-numeric? (string-ref rpn i)) (helper rpn (+ num-index 1) (+ (convert-to-int (string-ref rpn i)) (* num 10 )) stack (+ i 1)))
                 ((char-operation? (string-ref rpn i)) (helper
                                                        rpn
                                                        0
                                                        0
                                                        (stack-push (stack-pop-num (stack-pop-num stack))
                                                                                   ((convert-to-operation (string-ref rpn i)) (convert-to-num (stack-peek-num (stack-pop-num stack)))
                                                                                                                              (convert-to-num (stack-peek-num stack))))
                                                        (+ i 1)
                                                        )))
           )
    )
    (if (expr-valid? expr) 
        (helper rpn 0 0 "" 0)
        #f)

    )
  )




