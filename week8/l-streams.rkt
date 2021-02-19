#lang racket/base

(require racket/stream)

(define (infinite n)
  (printf "~a\n" n)
  (delay (infinite (+ n 1))))

;(infinite 0)
;(force (infinite 0))

(define (infinite* n)
  (cons (n (delay (infinite* (+ n 1))))))

(define (interval a b)
  (if (> a b)
      empty-stream
      [stream-cons a (interval (+ a 1) b)]))

