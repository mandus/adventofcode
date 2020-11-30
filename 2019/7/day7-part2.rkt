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


(define (str->numb l)
  (for/list ([i l])
    (string->number i)))

;;
;; Define an amplifier with a phase. We define the amp as a closure which
;; contains data and position, and takes input as an argument.
;; Before we return the closure, we iterate to the first input such that phase 
;; is set - then we will not pause on opcode 3 later
(define (create-amp phase)
  (let* ([data (str->numb (string-split (string-trim (file->string "input.txt")) ","))]
         [pos 0]
         [pause-on-input #t]
         [vd (list->vector data)])
    (define (amp input)
      (let-values ([(op nextpos output) (update-ic pos vd input)])
        ;(displayln (format "~a -> ~a: ~a, ~a" pos nextpos op output))
        (set! pos nextpos)
        (if (and (equal? op 3) pause-on-input)
          (begin
            ;(displayln (format "Input ~a set, do not pause on input later on" phase))
            (set! pause-on-input #f)
            (values op #f))
          (if (equal? op 4)
            (values op output)
            (if (equal? op 99)
              (values op #f)
              (amp input))))))
    (amp phase)
    amp))


(define (run-amps phases input)
  (let*-values ([(amp) (create-amp (car phases))]
                [(op output) (amp input)])
    (if (not (null? (cdr phases)))
      (run-amps (cdr phases) output)
      output)))

(define (create-amps phases amps)
  (if (not (null? (cdr phases)))
    (create-amps (cdr phases) (cons (create-amp (car phases)) amps))
    (reverse (cons (create-amp (car phases)) amps))))

(define (run-amps-once amps input final)
  (let-values ([(op output) ((car amps) input)])
    ;; as soon as one amp emit op 99, we can not continue; set as final
    (when (equal? op 99)
      (set! final #t))
    (if (and (not final) (not (null? (cdr amps))))
      (run-amps-once (cdr amps) output final) ;; feed output from previous Amp into the next
      (values output final))))

(define (loop-till-end phases) 
  (let* ([amps (create-amps phases '())])
    (define (looper input)
      (let-values ([(output final) (run-amps-once amps input #f)])
        (if final
          input ; use last runs output
          (looper output)))) ;; feed output from Amp E back into Amp A.
    (looper 0)))

(define (find-max perms maxi)
  (let ([output (loop-till-end (car perms))])
    (if (not (null? (cdr perms)))
      (find-max (cdr perms) (max maxi output))
      (max maxi output))))

(format "Part2: ~a" (find-max (permutations '(5 6 7 8 9)) 0))
