#!/usr/bin/env racket
#lang racket

;; Advent of Code 2019 - day 1 - Åsmund Ødegård

(define (find-fuels weight fuels)
  (let ((f (- (floor (/ weight 3)) 2)))
    (if (> f 0)
      (find-fuels f (cons f fuels))
      (for/sum ([i fuels]) i))))

; the extra for/sum can of course be dropped
(define (find-fuels-II weight fuels)
  (let ([f (- (floor (/ weight 3)) 2)])
    (if (> f 0)
      (find-fuels-II f (+ f fuels))
      fuels) ; just return the number
    ))

(define (run-part2)
  (for/fold
    ([acc 0])
    ([w (file->list "input.txt")])
    (+ acc (find-fuels-II w 0))))


(define (run-part1)
    (for/fold
      ([acc 0])
      ((i (file->list "input.txt")))
      (+ acc (- (floor (/ i 3)) 2))))

(run-part1)
(run-part2)
