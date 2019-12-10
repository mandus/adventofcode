#!/usr/bin/env racket
#lang racket

;; AoC 2019 - day 10 - Åsmund Ødegård

(define (enum-ast ast x y res)
  (let* ([el (car ast)]
         [newres (if (equal? el "#") (cons (list x y) res) res)])
    (if (null? (cdr ast))
      newres
      (enum-ast (cdr ast) (+ x 1) y newres))))

(define (enum-ast-lines lines y res)
  (let* ([line (cdr (string-split (car lines) ""))] ; need to chop off the empty "" after split
         [newres (enum-ast line 0 y res)])
    (if (null? (cdr lines))
      newres
      (enum-ast-lines (cdr lines) (+ y 1) newres))))

(define (input)
  (let ([lines (file->lines "input.txt")])
    (enum-ast-lines lines 0 '())))

;; asteriods has some size, so cut some decimals
(define (approx x)
  (let ([precision 10000])
    (/ (round (* precision x)) precision)))

(define (polar o p)
  (let* ([decimals 10000]
         [x (- (first p) (first o))]
         [y (- (second o) (second p))]
         [r (sqrt (+ (sqr x) (sqr y)))]
         [acost (approx (acos (/ x r)))]
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
          ;; check if closer; if so replace.
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
           [filtkeys (filter (λ (x) (>= x (approx (- (/ pi 2))))) sortkeys)] ;; use same roundoff as when calculating the angles
           [numfilt (length filtkeys)]
           [blastnum 200]
           )
      (if (> blastnum numfilt)
        (displayln (format "blast ~ath: ~a (angle ~a)" blastnum 
                           (second (hash-ref seen (list-ref sortkeys (- (- blastnum numfilt) 1))))
                           (list-ref sortkeys (- (- blastnum numfilt) 1))))
        (displayln (format "blast ~ath: ~a (angle ~a)" blastnum 
                           (second (hash-ref seen (list-ref filtkeys (- blastnum 1))))
                           (list-ref filtkeys (- blastnum 1))))
        ))
    ))

