#!/usr/bin/env racket
#lang racket

; AoC 2019 - åsmund ødegård - after insipiration from net and own python code

(define (next-point cur dir)
  (let ([incr (hash-ref (hash #\R '#(1 0)
                              #\U '#(0 1)
                              #\L '#(-1 0)
                              #\D '#(0 -1)) dir)])
  (vector-map + cur incr)))
   

(define (segment-points wire)
  (for*/fold ([pts '()] #:result (reverse pts))
    ([move (in-list wire)]
     [n (in-range (string->number (substring  move 1)))])
    (cons (next-point (if (null? pts)
                        '#(0 0)
                        (car pts)) (string-ref move 0))
          pts)))


(define (wires)
  (map (lambda (x) (string-split x ",")) (file->lines "input.txt")))

(map segment-points (wires))
