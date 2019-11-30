#!/usr/bin/env racket
#lang racket

;; Advent of Code 2019 - day 1 - Åsmund Ødegård

;; Assume there is some input-file with lines that needs to be done something
;; with


(define (run)
  (let ((lines (file->lines "input.txt")))
    ;; dummy
    (displayln (string-join lines))))

(run)
