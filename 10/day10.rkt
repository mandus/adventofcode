#!/usr/bin/env racket
#lang racket

;; AoC 2019 - day 10 - Åsmund Ødegård

(define (enum-ast ast x y res)
  (let* ([el (car ast)]
         ;[newres (if (equal? el "#") (cons (list (+ x 0.5) (+ y 0.5)) res) res)])
         [newres (if (equal? el "#") (cons (list x y) res) res)])
    ;(displayln (format "(~a,~a): ~a" x y el))
    (if (null? (cdr ast))
      newres
      (enum-ast (cdr ast) (+ x 1) y newres))))

(define (enum-ast-lines lines y res)
  (let* ([line (cdr (string-split (car lines) ""))] ; need to chop off the empty "" after split
         [newres (enum-ast line 0 y res)])
    ;(displayln (format " ~a " (car lines)))
    ;(displayln (format " ~a " line))
    (if (null? (cdr lines))
      newres
      (enum-ast-lines (cdr lines) (+ y 1) newres))))

(define (input)
  (let ([lines (file->lines "input.txt")])
    (enum-ast-lines lines 0 '())))

(define (polar o p)
  (let* ([decimals 10000]
         [x (- (first p) (first o))]
         [y (- (second o) (second p))]
         [r (sqrt (+ (sqr x) (sqr y)))]
         [acost (/ (round (* decimals (acos (/ x r)))) decimals)]
         [t (if (< 0 y) (- acost) acost)])
    (values t r)))

;; seen should be an immutable hash
(define (in-sight ast data seen)
  (let* ([check (car data)]
         [nomore (null? (cdr data))])

    (if (equal? ast check)
      (if nomore
        seen
        (in-sight ast (cdr data) seen))

      (let-values ([(t r) (polar ast check)])
        (if (hash-ref seen t #f)
          ;; check if closer
          (if (< r (first (hash-ref seen t)))
            (if nomore
              (hash-set seen t (list r check))
              (in-sight ast (cdr data) (hash-set seen t (list r check))))
            (if nomore
              seen
              (in-sight ast (cdr data) seen)))
          (if nomore
            (hash-set seen t (list r check))
            (in-sight ast (cdr data) (hash-set seen t (list r check)))))))))


(define (in-sight-counts checkdata data counts)
  (let* ([check (car checkdata)]
         [newcounts (hash-set counts check (hash-count (in-sight check data (hash))))])
    (if (null? (cdr checkdata))
      newcounts
      (in-sight-counts (cdr checkdata) data newcounts))))

(define (find-max data coor maxi)
  (let* ([d (car data)]
         [dsee (second d)]
         [newmax (if (> dsee maxi) dsee maxi)]
         [newcoor (if (> dsee maxi) (first d) coor)])
    (if (null? (cdr data))
      (values newcoor newmax)
      (find-max (cdr data) newcoor newmax))))

; part 1
(let* ([data (input)]
       [counts (in-sight-counts data data (hash))]
       [countslist (for/list ([(k v) (in-hash counts)]) (list k v))])
  (let-values ([(coor maxi) (find-max countslist #f -1)])
    (displayln (format "From ~a we see ~a others" coor maxi)) 

    ;; part 2
    (let* (;[data (input)]
           [monsta coor]
           [seen (in-sight monsta data (hash))]
           [seenkeys (hash-keys seen)]
           [sortkeys (sort seenkeys (λ (x y) (< x y)))]
           [filtkeys (filter (λ (x) (> x (- (/ pi 2)))) sortkeys)]
           [numfilt (length filtkeys)]
           )
      (displayln sortkeys)
      (displayln filtkeys)
      (displayln numfilt)
      (if (>= 200 numfilt)
        ;; I'm not really sure why I have to subtract 2...
        (displayln (hash-ref seen (list-ref sortkeys (- (- 200 numfilt) 2))))
        (displayln (hash-ref seen (list-ref filtkeys 199)))
        ))
    ))

