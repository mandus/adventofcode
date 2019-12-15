#!/usr/bin/env racket
#lang racket

;; h -> immutable hashmap
;(define (lines->hash lines h)
;  (let* ([line (car lines)]
;         [part (string-split line "=>")]
;         [produce (second part)]
;         [consume (first part)]
;         [updh (hash-set h produce consume)]
;         [linesleft (cdr lines)])
;    
;    (displayln (format "~a from (~a)" produce consume))
;    (if (empty? linesleft)
;      updh
;      (lines->hash linesleft updh))))

; create an hash map with the following format:
; Chemical -> (num, ((chemical num), (chemical num)...))
; The key is the chemical produced
; The first part, num, in value is the amount of chemical produced
; The second part of the value is a list of pairs (chemical num) that is needed
; in the reaction
(define (lines->map lines)
  (for/hash ([line lines])
    (let* ([part (string-split line "=>")]
           [produce (string-split (second part) " ")]
           [consume (map 
                      (λ (x) 
                         (let* ([matter (string-split x)]
                                [mass (string->number (first matter))]
                                [elem (second matter)])
                           (list elem mass))) 
                      (map string-trim (string-split (first part) ",")))])
      (values (second produce) (list (string->number (first produce)) consume)))))


(define (run-reaction chemicals ore stack make multiple)
  (for/fold ([oreacc ore]
             [leftovers stack])
    ([r make])
    (begin
      (displayln (format "fold with ~a, mul ~a " r multiple))
      (displayln (format "acc.ore ~a (left ~a)" oreacc leftovers))
      ; consume available leftovers first
      (let* ([has-leftovers (hash-ref leftovers (first r) 0)]
             [use-leftovers (min has-leftovers 
                                 (* multiple (second r)))]
             [adjusted-leftovers 
               (hash-set leftovers (first r) (- has-leftovers use-leftovers))]
             [adjusted-ask-num (- (* multiple (second r)) use-leftovers)])
        (displayln 
          (format "Use ~a of ~a from leftovers~%" use-leftovers (first r)))
        (reaction chemicals 
                  (first r) 
                  (hash-ref chemicals (first r)) 
                  adjusted-ask-num
                  adjusted-leftovers
                  oreacc)))))

(define (reaction chemicals prod recipe num stack ore)
  ; iterate through the parts of the recipe, carry out reactions all the way 
  ; to ORE - count ORE needed as we go. We need to create num times the recipe
  ; for the "prod" product,
  ; so we have to run the recipe enough times to get at least num.
  ; if required parts are on the stack and use them; else 
  ; if recipe is just ORE, add to ore and move on
  ; or finally, use recipes in the chemicals to do further reactions
  ; put left-overs on the stack
  ; in the end, return ore-count
  (let* ([produce (first recipe)]
         [make (second recipe)]
         [multiple (ceiling (/ num produce))]
         [leftover (- (* multiple produce) num)]  ; left-over multiplier
         )
    (displayln (format "~a ~a with recipe ~a (produce: ~a, mul: ~a, left: ~a) (have left ~a)" num prod
                       recipe produce multiple leftover (hash-ref stack prod 0)))
    ;(displayln (format "recipe length ~a" (length make)))
    ; (displayln (format "first to make ~a" (first make)))

    ;    (displayln (format "bool exp: ~a" 
    ;                       (and (equal? 1 (length make)) 
    ;                            (equal? "ORE" (first (first make))))))
    ;(displayln ore)
    (displayln stack)
    (displayln 
      (format "prod ~a left on stack: ~a" prod (hash-ref stack prod 0)))

    ;; consume stack of prod if available
    (if (<= num (hash-ref stack prod 0))
      (values ore
              (hash-set stack prod (- (hash-ref stack prod) num)))

      ;; else check if we can make with just ORE
      (if (and (equal? 1 (length make)) 
               (equal? "ORE" (first (first make))))
        (begin 
          (displayln (format ">>> add ~a ore for prod ~a (mul ~a, recipe ~a)" 
                             (* multiple (second (first make)))
                             prod
                             multiple
                             (first make)))
          (values (+ ore (* multiple (second (first make))))
                  (hash-set stack prod (+ (hash-ref stack prod 0) leftover))))


        ;; else run full recipe
        ;; we need to run all permutations of the recipe, since they may
        ;; require different amount of ORE
        (run-reaction chemicals ore stack make multiple)))))

(define (solve-data data)
  (let*-values ([(lines) (map string-trim (string-split data "\n"))]
                [(chemicals) (lines->map lines)]
                [(ore leftovers)
                 (reaction chemicals "FUEL" (hash-ref chemicals "FUEL") 1 (hash) 0)])
    ;(displayln lines)
    ;(displayln chemicals)
    (displayln 
      (format "used ~a ORE to produce FUEL (leftovers: ~a" ore leftovers))))

; with test-data
;(let* ([data " 10 ORE => 10 A 
;             1 ORE => 1 B
;             7 A, 1 B => 1 C 
;             7 A, 1 C => 1 D 
;             7 A, 1 D => 1 E
;             7 A, 1 E => 1 FUEL"])
;             (solve-data data))
;
;(let* ([data "9 ORE => 2 A
;             8 ORE => 3 B
;             7 ORE => 5 C
;             3 A, 4 B => 1 AB
;             5 B, 7 C => 1 BC
;             4 C, 1 A => 1 CA
;             2 AB, 3 BC, 4 CA => 1 FUEL"])
;             (solve-data data))
;
(let ([data "157 ORE => 5 NZVS
            165 ORE => 6 DCFZ
            44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
            12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
            179 ORE => 7 PSHF
            177 ORE => 5 HKGWZ
            7 DCFZ, 7 PSHF => 2 XJWVT
            165 ORE => 2 GPVTF
            3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"])
            (solve-data data))

;(let ([data "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
;            17 NVRVD, 3 JNWZP => 8 VPVL
;            53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
;            22 VJHF, 37 MNCFX => 5 FWMGM
;            139 ORE => 4 NVRVD
;            144 ORE => 7 JNWZP
;            5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
;            5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
;            145 ORE => 6 MNCFX
;            1 NVRVD => 8 CXFTF
;            1 VJHF, 6 MNCFX => 4 RFSQX
;            176 ORE => 6 VJHF"])
;            (solve-data data))

;(let ([data "171 ORE => 8 CNZTR
;            7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
;            114 ORE => 4 BHXH
;            14 VRPVC => 6 BMBT
;            6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
;            6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
;            15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
;            13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
;            5 BMBT => 4 WPTQ
;            189 ORE => 9 KTJDG
;            1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
;            12 VRPVC, 27 CNZTR => 2 XDBXC
;            15 KTJDG, 12 BHXH => 5 XCVML
;            3 BHXH, 2 VRPVC => 7 MZWV
;            121 ORE => 7 VRPVC
;            7 XCVML => 6 RJRHP
;            5 BHXH, 4 VRPVC => 5 LTCX"])
;            (solve-data data))

;(let* ([data (file->string "input.txt")])
;  (solve-data  data))
