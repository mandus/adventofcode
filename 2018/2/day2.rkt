#!/usr/bin/env racket
#lang racket

;; Advent of Code 2018 - Åsmund Ødegård

(define (check-line line)
  (let* ((counts (for/fold ([counts (hash)])
                   ([chr (string->list line)])
                   (hash-update counts chr (lambda (i) (+ i 1)) 0)))
         (hash-vals (hash-values counts)))
    (values 
      (if (member 2 hash-vals) 1 0)
      (if (member 3 hash-vals) 1 0))))

(define (checksum lines)
  (let-values ([(dupe trip) 
               (for/fold ([dupe 0]
                          [trip 0])
                 ([line lines ])
                 (let-values ([(d t) (check-line line)])
                   (values 
                     (+ dupe d ) 
                     (+ trip t))))])
   (* dupe trip)))

(define (close-lines f s)
  (let ((diff (for/fold ([diff 0])
                ([elf (string->list f)]
                 [els (string->list s)])
                (if (equal? elf els)
                  diff
                  (+ diff 1)))))
    (if (> diff 1) 
      #f 
      #t)))

(define (remove-diff f s) 
  (let ((word (for/fold ([word null])
                ([elf (string->list f)]
                 [els (string->list s)])
                (if (equal? elf els)
                  (cons elf word)
                  word))))
    (list->string (reverse word))))

;; Better differ - from subreddit
(define (deltaword f s)
  (for/fold ([word ""])
    ([elf (in-string f)]
     [els (in-string s)])
    (if (equal? elf els)
      (~a word elf)
      word)))

(define (find-close lines) 
  (let-values ([(first second) 
                (for/fold ([first null]
                           [second null])
                  ([line lines]
                   #:break (and (not (null? first)) (not (null? second))))
                  (if (null? first)
                    (values line null)
                    (if (close-lines first line)
                      (values first line)
                      (values line null))))])
    (deltaword first second)))

(define (run-it)
 (let ((lines (sort (file->lines "input.txt") string<?)))
   ;; Part 1 - day2
   (displayln (checksum lines))
   ;; Part 2 - day 2
   (displayln (find-close lines))
   ))

(run-it)
