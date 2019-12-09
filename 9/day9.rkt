#!/usr/bin/env racket
#lang racket

;; AoC 2019 - day 9 - Åsmund Ødegård


;;
;; Now, the intcode computer need to support larger memory.
;; I will thus use hashmap instead of vector for the data
;;

(define (read-mem ic pos)
  (when (< pos 0)
    (raise 'negative-memory-location))
  (hash-ref ic pos 0)) ; if pos is not in ic, return 0


(define (read-oper ic pos mode relbase)
  ; in mode 0, look up memory location
  ; in mode 1, look up parameter immediately
  ; in mode 2, look up relative to the relative base
  (case mode
    [(0) (read-mem ic (read-mem ic pos))]
    [(1) (read-mem ic pos)]
    [(2) (read-mem ic (+ (read-mem ic pos) relbase))]))

(define (read-left ic pos op relbase)
  (read-oper ic (+ 1 pos) (get-mode op 2) relbase))

(define (read-right ic pos op relbase) 
  (read-oper ic (+ 2 pos) (get-mode op 1) relbase))

(define (save-oper ic pos mode relbase)
  (case mode
    [(0) (read-mem ic pos)]
    [(1) pos]
    [(2) (+ (read-mem ic pos) relbase)]))

(define (get-opcode op)
  (- op (* 100 (quotient op 100))))

(define (get-mode op part)
  ;; part is: 0 for result, 1 for right, 2 for left
  (let ([str-op (~r op #:min-width 5 #:pad-string "0")])
    (- (char->integer (string-ref str-op part)) 48)))

(define (update-ic pos ic input relbase)
  (let* ([op (read-mem ic pos)]
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
                              9 2
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
                                  9 0
                                  99 0) opcode)]
         [savepos (save-oper ic (+ readsavepos pos) (get-mode op (- 3 readsavepos)) relbase)]
         [left (read-left ic pos op relbase)]
         [right (read-right ic pos op relbase)]
         )
    ;(displayln (format "~a [~a] (l: ~a, r: ~a) savepos ~a" op opcode left right savepos))
    (case opcode
      [(1) (hash-set! ic savepos (+ left right))]
      [(2) (hash-set! ic savepos (* left right))]
      [(3) (hash-set! ic savepos input)]
      [(4) (set! output left)]
      [(5) (when (not (equal? 0 left))
             (set! movepos right))]
      [(6) (when (equal? 0 left)
             (set! movepos right))]
      [(7) (if (< left right)
             (hash-set! ic savepos 1)
             (hash-set! ic savepos 0))]
      [(8) (if (= left right)
             (hash-set! ic savepos 1)
             (hash-set! ic savepos 0))]
      [(9) (set! relbase (+ relbase left))]
      )
    (values 
      opcode
      movepos
      output
      relbase)))


(define (str->numb->hash l)
  (for/fold ([ic (hash)])
    ([i (range (length l))]
     [v l])
    (hash-set ic i (string->number v))))

;;
;; Define an amplifier with a phase. We define the amp as a closure which
;; contains data and position, and takes input as an argument.
;; is set - then we will not pause on opcode 3 later
(define (create-computer pause-on-first-input)
  (let* ([ic (hash-copy (str->numb->hash (string-split (string-trim (file->string
                                                           "input.txt"))
                                                       ",")))]
         [pos 0]
         [relbase 0]
         [pause-on-input pause-on-first-input]
         )
    (define (computer input)
      (let-values ([(op nextpos output updrelbase) (update-ic pos ic input relbase)])
        ;(displayln (format "~a -> ~a: ~a, ~a, (rb ~a -> ~a)" pos nextpos op output relbase updrelbase))
        ;(displayln ic)
        (set! pos nextpos)
        (set! relbase updrelbase)
        (if (and (equal? op 3) pause-on-input)
          (begin
            (set! pause-on-input #f)
            (values op #f))
          (if (equal? op 4)
            (values op output)
            (if (equal? op 99)
              (values op #f)
              (computer input))))))
    computer))


(define (run-amps phases input)
  (let*-values ([(amp) (create-computer (car phases))]
                [(op output) (amp input)])
    (if (not (null? (cdr phases)))
      (run-amps (cdr phases) output)
      output)))

(define (create-amps phases amps)
  (if (not (null? (cdr phases)))
    (create-amps (cdr phases) (cons (create-computer (car phases)) amps))
    (reverse (cons (create-computer (car phases)) amps))))

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

(define (run-computer comp outputs)
  (let-values([(op output) (comp 2)])
    (if (equal? op 99)
      (reverse outputs)
      (run-computer comp (cons output outputs)))))

(let ([comp (create-computer #f)])
  (displayln (run-computer comp '())))

