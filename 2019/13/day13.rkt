#!/usr/bin/env racket
#lang racket

;; AoC 2019 - day 13 - Åsmund Ødegård


;;
;; Use IntCode computer to run a robot
;;

(define (read-mem ic pos)
  (when (< pos 0)
    (begin 
      ;(displayln ic)
      (displayln pos)
      (raise 'negative-memory-location)))
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
;; Define an computer with an input. We define the computer as a closure which
;; contains data and memory-position, and takes input as an argument.
;; is set - then we will not pause on opcode 3 later
(define (create-computer pause-on-first-input pause-on-all-input)
  (let* ([ic (hash-copy (str->numb->hash (string-split (string-trim (file->string
                                                           "input.txt"))
                                                       ",")))]
         [pos 0]
         [relbase 0]
         [pause-on-input pause-on-first-input]
         )
    (define (computer input)
      (let-values ([(op nextpos output updrelbase) (update-ic pos ic input relbase)])
        ;(displayln (format "~a -> ~a: ~a, ~a, (rb ~a -> ~a) (inp: ~a)" pos nextpos op output relbase updrelbase input))
        ;(displayln ic)
        (set! pos nextpos)
        (set! relbase updrelbase)
        (if (and (equal? op 3) pause-on-input)
          (begin
            (set! pause-on-input pause-on-all-input)
            (values op #f))
          (if (equal? op 4)
            (values op output)
            (if (equal? op 99)
              (values op #f)
              (computer input))))))
 	(hash-set! ic 0 2) ;; hack 2 quarters to keep running
    computer))

;
; arcade game.
;

(define (find-blocks tiles block)
  (map (λ (x) (car x)) 
       (filter (λ (x) (equal? (cdr x) block)) (hash->list tiles))))

; find ball closest to the paddle, if more than one
; ignore balls at same y-coord as the paddle as those are lost
(define (closest-ball balls ball paddle) 
  (let* ([nextball (car balls)]
         [updball (if 
                    (and (> (second nextball) (second ball)) 
                         (< (second nextball) (second paddle))) 
                    nextball 
                    ball)]
         )
    (if (empty? (cdr balls))
      updball
      (closest-ball (cdr balls) updball paddle))))

;; a in ax + b!
(define (a x y)
  (/ (- (first x) (first y)) (- (second x) (second y))))

;; b in ax + b - given a and a point p
(define (b a p)
  (let* ([x (first p)]
         [y (second p)]
         )
    (- y (* a x))))

;; find x where the line cross y = N
(define (x a b y)
  (* (/ 1 a) (- y b)))

(define (find-sign newball oldball paddle)
  (let* ([A (a newball oldball)]
         [B (b A newball)]
         [X (x A B (second paddle))]
         [sign (if (> X (first paddle)) 
                 1 
                 (if (< X (first paddle)) 
                   -1 
                   0))]
         )
    sign))

(define (sign x y) 
  (if (equal? x y) 0 (/ (- x y) (abs (- x y)))))

;(define (move-paddle paddle balls prevballs newball)
;  (if (or (empty? paddle) 
;          (and (empty? balls) (empty? prevballs) (empty?  newball)))
;    0
;    (let* ([ball (closest-ball balls (car balls) (first paddle))]
;           [prevball (closest-ball prevballs (car prevballs) (first paddle))]
;           )
;      ;(displayln (format "newball: ~a" newball))
;      (displayln (format "ball: ~a" ball))
;      (displayln (format "paddle: ~a" paddle))
;      (if (not (empty? newball))
;        (if (> (second ball) (second newball))
;          (if (> (first (first paddle)) (first newball)) ; move up - move paddle towards x of ball
;            -1 
;            (if (< (first (first paddle)) (first newball))
;              1
;              0))
;          (find-sign newball ball (first paddle)))
;        0))))

(define (move-paddle paddle balls prevballs newball)
  (if (and (not (empty? paddle))
           (not (empty? newball)))
    (sign (first newball) (first (first paddle)))
    0))

(define (run-arcade comp tiles input prevballs)
  (let*-values ([(op x) (comp input)]
                [(op y) (if (or (equal? op 3) (equal? op 99)) 
                          (values op #f) 
                          (comp input))]
                [(op id) (if (or (equal? op 3) (equal? op 99)) 
                           (values op #f)
                           (comp input))])
    ;(when (equal? op 3)
    ;  (displayln (format "~a: (~a,~a) ~a" op x y id)))
    ;(displayln (format "balls: ~a" (find-blocks tiles 4)))
    (let* ([paddle (find-blocks tiles 3)]
           [presentballs (find-blocks tiles 4)]
           [balls (if (empty? presentballs) prevballs presentballs)]
           [newball (if (and (equal? op 4) (equal? id 4)) (list x y) '())]
           [nextinput (move-paddle paddle balls prevballs newball)])

      ;(displayln (format "next input is: ~a" nextinput))
      (case op
        [(3) (run-arcade comp tiles nextinput balls)]
        [(4) (if (and (equal? x -1) (equal? y 0))
               (begin
                 (displayln (format "current score: ~a" id))
                 (run-arcade comp tiles nextinput balls))
               (run-arcade comp (hash-set tiles (list x y) id) nextinput balls))]
        [(99) tiles]))))
      
(define (count-blocks tiles block)
  (length (filter (λ (x) (equal? (cdr x) block)) (hash->list tiles))))

(let ([comp (create-computer #t #t)]
      [tiles (hash)])
  (count-blocks (run-arcade comp tiles 0 '()) 2))
; (displayln (format "part1: ~a" 
;                    (count-blocks  2))))


;
; The robot is run like this:
; the hull is a map consisting of the tiles that have been painted
; if not painted (position not present in map), default to "0" (black)
; maintain position as (x,y)
; maintain direction as count of π/2
; get update for opsition (after trun) as 
;   ((exact-round (cos dir)), (exact-round (sin dir))

(define (direction->move dir)
  (let ([pi-half (/ pi 2)])
    (list (exact-round (cos (* dir pi-half)))
          (exact-round (sin (* dir pi-half))))))

(define (move position direction)
  (let ([update (direction->move direction)])
    (list (+ (first position) (first update)) (+ (second position) (second update)))))

;
; direction = 0: Turn left, i.e add π/2 to the direction
;           = 1: Turn right, i.e subtract π/2 to the direction
(define (run-robot comp hull position direction)
  (let*-values([(input) (hash-ref hull position 0)]   ; default to black
               [(op color) (comp input)]              ; first get the color
               [(op changedirection) (if (equal? op 99) 
                                       (values op 0)  ; keep op; we will stop  
                                       (comp #f))] ; else, get new direction
               [(newdirection) (+ direction (if (> changedirection 0) -1 1))]
               [(newposition) (move position newdirection)])

    (if (equal? op 99)
      hull
      (run-robot comp (hash-set hull position color) newposition newdirection))))

(define (bbox coords minx miny maxx maxy)
  (let* ([chk (car coords)]
         [x (first chk)]
         [y (second chk)]
         [updminx (min minx x)]
         [updminy (min miny y)]
         [updmaxx (max maxx x)]
         [updmaxy (max maxy y)])
    (if (empty? (cdr coords))
      (list (list updminx updminy) (list updmaxx updmaxy))
      (bbox (cdr coords) updminx updminy updmaxx updmaxy))))

(define (draw hull bbox)
  (let ([minx (first (first bbox))]
        [miny (second (first bbox))]
        [maxx (first (second bbox))]
        [maxy (second (second bbox))])
    (define (drawinner x y str)
      ; blank if not in hull or value is 0
      ; # if value is 1
      ; output str when decreasing y; then reset x to minx
      (let* ([color (if (> (hash-ref hull (list x y) 0) 0) "█" " ")]
             [updstr (format "~a~a" str color)])
        (if (< y miny)
          #t ; we're done.
          (if (> x maxx)
            ; output str and move on
            (begin 
              (displayln updstr)
              (drawinner minx (- y 1) ""))
            (drawinner (+ 1 x) y updstr)))))
    (drawinner minx maxy "")))

;(let* ([comp (create-computer #f)]
;       [bigint 100000000]
;       [smallint (- bigint)]
;       [hull (hash-set (hash) (list 0 0) 1)]
;       [paintedhull (run-robot comp hull (list 0 0) 1)]
;       [bb (bbox (hash-keys paintedhull) bigint bigint smallint smallint)])
;
;  (displayln (format "part2:"))
;  (draw paintedhull bb))


