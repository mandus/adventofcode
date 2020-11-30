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
         [output #f]
         [opcode (get-opcode op)]
         [movepos (+ pos (hash-ref (hash 
                              1 4 
                              2 4 
                              3 2 
                              4 2 
                              5 3
                              6 3
                              7 4
                              8 4
                              99 0) opcode))] 
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
      [(4) (set! output (read-left ic pos op))]
      [(5) (when (not (equal? 0 (read-left ic pos op)))
             (set! movepos (read-right ic pos op)))]
      [(6) (when (equal? 0 (read-left ic pos op))
             (set! movepos (read-right ic pos op)))]
      [(7) (if (< (read-left ic pos op) (read-right ic pos op))
             (vector-set! ic savepos 1)
             (vector-set! ic savepos 0))]
      [(8) (if (= (read-left ic pos op) (read-right ic pos op))
             (vector-set! ic savepos 1)
             (vector-set! ic savepos 0))]
      )
    (values 
      opcode
      movepos
      output)))


(define (set-noun-verb ic noun verb)
  ;(vector-set! ic 1 noun)
  ;(vector-set! ic 2 verb)
  ic)


(define (str->numb l)
  (for/list ([i l])
    (string->number i)))


(define (amp phase input)
  (let* ([data (str->numb (string-split (string-trim (file->string "input.txt")) ","))]
         [icinput phase]  
         [output #f]
         [vd (list->vector data)])

    (define (amp-looper pos)
      (let-values ([(op nextpos getout) (update-ic pos vd icinput)])

        (when (equal? op 3)
          ;; When phase has been read, set regular input
          (set! icinput input))

        (when (equal? op 4)
          (set! output getout))

        (if (equal? op 99)
          output
          (amp-looper nextpos))))
    (amp-looper 0)
  output))

(define (run-amps phases input)
  (let ([output (amp (car phases) input)])
    (if (not (null? (cdr phases)))
      (run-amps (cdr phases) output)
      output)))

(define (find-max perms maxi)
  (let ([output (run-amps (car perms) 0)])
    (if (not (null? (cdr perms)))
      (find-max (cdr perms) (max maxi output))
      (max maxi output))))

(format "Part1: ~a" (find-max (permutations '(0 1 2 3 4)) 0))
