#!/usr/bin/env racket
#lang racket

;; AoC 2019 - day 2 - Åsmund Ødegård

(define (update-ic pos ic)
  (let ([op (vector-ref ic pos)]
        [left (vector-ref ic (vector-ref ic (+ 1 pos)))]
        [right (vector-ref ic (vector-ref ic (+ 2 pos)))]
        [savepos (vector-ref ic (+ 3 pos))])
    (case op
      [(1) (vector-set! ic savepos (+ left right))]
      [(2) (vector-set! ic savepos (* left right))])
    (values 
      op
      (+ pos 4))))


(define (set-noun-verb ic noun verb)
  (vector-set! ic 1 noun)
  (vector-set! ic 2 verb)
  ic)


(define (str->numb l)
  (for/list ([i l])
    (string->number i)))

(define (part1)
  (let* ([data (str->numb (string-split (file->string "input.txt") ","))]
         [vd (set-noun-verb (list->vector data) 12 2)])
    (define (loop-till-ninenine pos)
      (let-values ([(op nextpos) (update-ic pos vd)])
        (if (equal? op 99)
          (displayln (vector-ref vd 0))
          (loop-till-ninenine nextpos))))
    (loop-till-ninenine 0)))

(part1)
