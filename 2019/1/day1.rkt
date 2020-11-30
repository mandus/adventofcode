#!/usr/bin/env racket
#lang racket

;; Advent of Code 2019 - day 1 - Åsmund Ødegård

(define (fuel-for-mass mass)
  (- (quotient mass 3) 2))

(define (find-fuels-II mass fuels)
  (let ([f (fuel-for-mass mass)])
    (if (> f 0)
      (find-fuels-II f (+ f fuels))
      fuels)))

; part1
(for/fold
  ([acc 0])
  ((i (file->list "input.txt")))
  (+ acc (fuel-for-mass i)))


; part2
(for/fold
  ([acc 0])
  ([w (file->list "input.txt")])
  (+ acc (find-fuels-II w 0)))
