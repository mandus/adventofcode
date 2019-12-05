#!/usr/bin/env racket
#lang racket

;; AoC 2019 - day 5 - Åsmund Ødegård


(define (read-oper ic pos mode)
  ; in mode 0, look up memory location
  ; in mode 1, look up parameter immediately
  (case mode
    [(0) (vector-ref ic (vector-ref ic pos))]
    [(1) (vector-ref ic pos)]))


(define (read-left ic pos op)
  (read-oper ic (+ 1 pos) (get-mode op 2)))

(define (read-right ic pos op) 
  (read-oper ic (+ 2 pos) (get-mode op 1)))

(define (save-oper ic pos mode)
  (case mode
    [(0) (vector-ref ic pos)]
    [(1) pos]))

(define (get-opcode op)
  (- op (* 100 (quotient op 100))))

(define (get-mode op part)
  ;; part is: 0 for result, 1 for right, 2 for left
  (let ([str-op (~r op #:min-width 5 #:pad-string "0")])
    (- (char->integer (string-ref str-op part)) 48)))

(define (update-ic pos ic input)
  (let* ([op (vector-ref ic pos)]
         [opcode (get-opcode op)]
         [movepos (hash-ref (hash 
                              1 4 
                              2 4 
                              3 2 
                              4 2 
                              5 3
                              6 3
                              7 4
                              8 4
                              99 0) opcode)] 
         [readsavepos (hash-ref (hash 
                                  1 3 
                                  2 3 
                                  3 1 
                                  4 1 
                                  5 0
                                  6 0
                                  7 3
                                  8 3
                                  99 0) opcode)]
         [savepos (save-oper ic (+ readsavepos pos) (get-mode op 0))]
         )
    (case opcode
      [(1) (vector-set! ic savepos (+ (read-left ic pos op) (read-right ic pos op)))]
      [(2) (vector-set! ic savepos (* (read-left ic pos op) (read-right ic pos op)))]
      [(3) (vector-set! ic savepos input)]
      [(4) (displayln (format "output from (~a, ~a): ~a" 
                   op 
                   (+ 1 pos)
                   (read-left ic pos op)))]
      [(5) (when (not (equal? 0 (read-left ic pos op)))
             (set! movepos (- (read-right ic pos op) pos)))]
      [(6) (when (equal? 0 (read-left ic pos op))
             (set! movepos (- (read-right ic pos op) pos)))]
      [(7) (if (< (read-left ic pos op) (read-right ic pos op))
             (vector-set! ic savepos 1)
             (vector-set! ic savepos 0))]
      [(8) (if (= (read-left ic pos op) (read-right ic pos op))
             (vector-set! ic savepos 1)
             (vector-set! ic savepos 0))]
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

(define (parts)
  (let* ([data (str->numb (string-split (string-trim (file->string "input.txt")) ","))]
         ;[input 1] ; The Ship's air conditioner id...
         [input 5] ; The Ship's thermal radiator controller...
         [vd (set-noun-verb (list->vector data) 0 0)]) ; NOTE noun/verb disabled
    (define (loop-till-ninenine pos)
      (let-values ([(op nextpos) (update-ic pos vd input)])
        (if (equal? op 99)
          ;(displayln (vector-ref vd 0))
          (format "Done!")
          (loop-till-ninenine nextpos))))
    (loop-till-ninenine 0)))

(parts)
