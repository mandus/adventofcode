#!/usr/bin/env racket
#lang racket

;; AoC 2019 - day 6 - Åsmund Ødegård


(define (input)
  (map  (λ (x) (string-split x ")")) (file->lines "input.txt")))

(define (hash-orbits)
  (let* 
    ([orbs (make-hash)] 
     [l (input)])
    (define (insert-if-more orbs l)
      (let* ([head (car l)]
             [tail (cdr l)])
        (if (equal? (first head) "COM")
          (begin
            (hash-set! orbs (first head) 0)
            (hash-set! orbs (second head) 1)
            (insert-if-more orbs tail))
          (if (hash-has-key? orbs (first head))
            (begin 
              (hash-set! orbs (second head) (+ (hash-ref orbs (first head)) 1))
              (when (not (empty? tail))
                (insert-if-more orbs tail)))
            (insert-if-more orbs (reverse (cons head (reverse tail))))))))
    (insert-if-more orbs l)
    orbs))

;    (for ([l (input)])
;        (displayln (format "~a: ~a" (car l) (hash-ref orbs (car l) 0)))
;        (displayln (format "~a: ~a" (second l) (hash-ref orbs (second l) 0)))
;        (unless (hash-has-key? orbs (car l))
;          (hash-set! orbs (car l) 0))
;        (hash-set! orbs (second l) (+ (hash-ref orbs (car l)) 1))) 
;    orbs))


(for*/fold ([acc 0])
  ([(k v) (hash-orbits)])
  (+ acc v))
