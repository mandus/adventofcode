#lang racket
(require racket/set)
(require threading)

(struct point (x y) #:transparent)
(struct instruction (direction distance) #:transparent)

(define (parse-line line)
  (map (λ (instr) (instruction (substring instr 0 1) (string->number (substring instr 1)) ))(string-split line ",")))

(define (get-input)
  (map parse-line (file->lines "input.txt")))

(define test-a "R8,U5,L5,D3")
(define test-b "U7,R6,D4,L4")
(define get-test (list (parse-line test-a) (parse-line test-b)))

(define (points-from-instruction instr poss)
    (let ([pos (car poss)])
    (flatten (list*
     (match instr
      [(instruction "R" len) (for/list ([step (range len 0 -1)]) (point (point-x pos) (+ (point-y pos) step)))]
      [(instruction "L" len) (for/list ([step (range len 0 -1)]) (point (point-x pos) (- (point-y pos) step)))]
      [(instruction "U" len) (for/list ([step (range len 0 -1)]) (point (+ (point-x pos) step) (point-y pos)))]
      [(instruction "D" len) (for/list ([step (range len 0 -1)]) (point (- (point-x pos) step) (point-y pos)))])
     poss))))

(define (create-wire instructions)
  (define (create-recur instructions positions)
    (if (empty? instructions)
        positions
        (create-recur (cdr instructions) (points-from-instruction (car instructions) positions))))
  (create-recur instructions (list (point 0 0))))

(define (get-wires input)
  (map create-wire input))

(define (part1 input)
  (let ([wires (map list->set (get-wires input))])
  (~>> (set-intersect (first wires) (second wires))
      (set->list)
      (map (λ (point) (+ (abs (point-x point)) (abs (point-y point)))))
      (filter (λ (val) (not (= val 0))))
      (apply min))))

(writeln "Part1: ")
(writeln (part1 (get-input)))

(define (part2 input)
  (let* ([wirepaths (map reverse (get-wires input))]
         [wires (map list->set wirepaths)])
    (~>> (set-intersect (first wires) (second wires))
         (set->list)
         (map (λ (point) (+ (index-of (first wirepaths) point) (index-of (second wirepaths) point))))
         (filter (λ (val) (not (= 0 val))))
         (apply min)
         )))

(writeln "Part2: ")
(writeln (part2 (get-input)))
