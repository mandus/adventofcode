#!/usr/bin/env racket
#lang racket

;; AoC 2019 - day 5 - Åsmund Ødegård

;; add support for opcodes 3 (input) and 4 (outout).
;; also add support for immediate and positional modes, i.e direct access of
;; parameter, or use param as a memory location.
;; Full opcode is now xyzop; 
;;  - xyz are bits for mode of result | right op | left op
;;  - op is a number 00-99

(define (read-oper ic pos mode)
  ; in mode 0, look up memory location
  ; in mode 1, look up parameter immediately
  (case mode
    [(0) (vector-ref ic (vector-ref ic pos))]
    [(1) (vector-ref ic pos)]))

(define (save-oper ic pos mode)
  (case mode
    [(0) (vector-ref ic pos)]
    [(1) pos]))

(define (get-opcode op)
  (- op (* 100 (quotient op 100))))

(define (get-mode op part)
  ;; part is: 0 for result, 1 for right, 2 for left
  (let ([str-op (~r op #:min-width 5 #:pad-string "0")])
    (string-ref str-op part)))

(define (update-ic pos ic input)
  (let* ([op (vector-ref ic pos)]
         [opcode (get-opcode op)]
         [movepos (hash-ref (hash 1 4 2 4 3 2 4 2 99 0) opcode)] ; TODO op 1, 2 move 4, op 3, 4 moves 2
         [readsavepos (hash-ref (hash 1 3 2 3 3 1 4 1) opcode)]
         [left (read-oper ic (+ 1 pos) (get-mode op 2))]
         [right (read-oper ic (+ 2 pos) (get-mode op 1))]
         [savepos (save-oper ic (+ readsavepos pos) (get-mode op 0))]) 
    (case op
      [(1) (vector-set! ic savepos (+ left right))]
      [(2) (vector-set! ic savepos (* left right))]
      [(3) (vector-set! ic savepos input)]
      [(4) ()]
      )

    (values 
      opcode
      (+ pos movepos))))


(define (set-noun-verb ic noun verb)
  ;(vector-set! ic 1 noun)
  ;(vector-set! ic 2 verb)
  ic)


(define (str->numb l)
  (for/list ([i l])
    (string->number i)))

(define (part1)
  (let* ([data (str->numb (string-split (file->string "input.txt") ","))]
         [input 1] ; The Ship's air conditioner id...
         [vd (set-noun-verb (list->vector data) 0 0)]) ; NOTE noun/verb disabled
    (define (loop-till-ninenine pos)
      (let-values ([(op nextpos) (update-ic pos vd input)])
        (if (equal? op 99)
          (displayln (vector-ref vd 0))
          (loop-till-ninenine nextpos))))
    (loop-till-ninenine 0)))

(part1)
