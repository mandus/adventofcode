#!/usr/bin/env racket
#lang racket

(require racket/set)

;; AoC 2019 - day 6 - Åsmund Ødegård


(define (input)
  (map  (λ (x) (string-split x ")")) (file->lines "input.txt")))

(define (hash-orbits)
  (let* 
    ([orbs (make-hash)] 
     [conns (make-hash)]
     [l (input)])
    (define (insert-if-more orbs l)
      (let* ([head (car l)]
             [tail (cdr l)])
        (if (equal? (first head) "COM")
          ;; base-case / root-node
          (begin
            (hash-set! orbs (first head) 0)
            (hash-set! orbs (second head) 1)
            (hash-set! conns (second head) (first head))
            (insert-if-more orbs tail))

          (if (hash-has-key? orbs (first head))
            (begin 
              (hash-set! orbs (second head) (+ (hash-ref orbs (first head)) 1))
              (hash-set! conns (second head) (first head))
              (when (not (empty? tail))
                (insert-if-more orbs tail)))
            (insert-if-more orbs (reverse (cons head (reverse tail))))))))

    (insert-if-more orbs l)
    (values 
      orbs 
      conns)))

(define (path conns from to)
  ;; Find the keys of the path from - to
  (define (looper from nodes)
    (let ([parent (hash-ref conns from)])
      (if (equal? parent to)
        nodes
        (looper parent (cons parent nodes)))))
  (looper from '()))


(let*-values ([(orbs conns) (hash-orbits)]
              [(youpath) (path conns "YOU" "COM")]
              [(santapath) (path conns "SAN" "COM")]
              [(commonpath) (set-intersect youpath santapath)]
              [(maxcommonorb) (for*/fold ([maxorb 0])
                             ([path commonpath])
                             (max maxorb (hash-ref orbs path)))]
              [(youorb) (hash-ref orbs "YOU")]
              [(sanorb) (hash-ref orbs "SAN")]
              [(part2) (+ (- youorb 1 maxcommonorb) (- sanorb 1 maxcommonorb))])
  ;; part1
  (displayln (format "part1: ~a" 
                     (for*/fold ([acc 0])
                       ([(k v) orbs])
                       (+ acc v))))
  (displayln (format "part2: ~a" part2)))
