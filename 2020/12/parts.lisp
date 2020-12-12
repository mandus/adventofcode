; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)

(uiop:define-package 
  :aoc
  (:use :cl)
  (:export :run)
  )

(unless swank::*connections* 
  (swank:create-server :port 4005 :dont-close t))

(in-package :aoc)

(defparameter *debug* nil)
;(defparameter *inp* "input_test.txt")
(defparameter *inp* (if *debug* "input_test.txt" "input.txt"))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

;; general utils
;;
(defun parse (l)
  (cons (subseq l 0 1) (parse-integer (subseq l 1))))
 
(defun alist-val (lst key) 
  "return value in alist for given string key"
  (cdr (assoc key lst :test #'string=)))

(defun in (val lst)
  "return member list of string value in list"
  (member val lst :test #'string=))

(defun case-str (str)
  (intern (string-upcase str)))

;; move in compass direction, pt is a complex
(defun compass ()  
  (list (cons "N" (complex 0 1)) 
        (cons "S" (complex 0 -1))
        (cons "E" (complex 1 0))
        (cons "W" (complex -1 0))))

(defun movept (pt dir len)
  (let* ((dirs (compass))) 
    (+ pt (* (alist-val dirs dir) len))))

;; part 1
(defun upd-pos (pos dir instr move)
  (if (in instr '("L" "R")) 
      pos
      (cond
        ((in instr '("N" "S" "E" "W")) (movept pos instr move))
        ((string= instr "F") (+ pos (* dir move))))))

(defun upd-dir (dir instr move)
  (let* ((dirs (compass))
         (pwr (/ move 90))
         (ldir (* dir (expt (alist-val dirs "N") pwr)))
         (rdir (* dir (expt (alist-val dirs "S") pwr))))
    (ecase (case-str instr)
           ((N S E W F) dir)
           (L ldir)
           (R rdir))))

;; part 2

(defun move-pos (pos way instr move)
  (if (string/= instr "F") pos
      (+ pos (* way move))))

(defun upd-way (way instr move)
  (let* ((dirs (compass))
         (pwr (/ move 90))
         (ldir (* way (expt (alist-val dirs "N") pwr)))
         (rdir (* way (expt (alist-val dirs "S") pwr))))
    (ecase (case-str instr)
           (F way)
           ((N S E W) (movept way instr move))
           (L ldir)
           (R rdir))))

;; simulation
(defun upd (lst posfn dirfn dir &optional (pos (complex 0 0)))
 (let* ((upd (car lst))
        (instr (car upd))
        (move (cdr upd))
        (nxtpos (funcall posfn pos dir instr move))
        (nxtdir (funcall dirfn dir instr move))
        (nxt (cdr lst)))
   (when *debug*
     (format t "~a (~a): ~a|~a -> ~a (~a) ~%" pos dir instr move nxtpos nxtdir))
   (if nxt (upd nxt posfn dirfn nxtdir nxtpos)
       nxtpos)))

(defun manhattan (pos)
  (+ (abs (realpart pos)) (abs (imagpart pos))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'parse))
         (end (upd data #'upd-pos #'upd-dir (complex 1 0))))

    (format t "Part 1~%")
    (format t "Manhattan: ~a~%" (manhattan end))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "end ~a~%" end)
      )))

(defun part2 (fn)
  (let* ((data (read-input fn #'parse))
         (end (upd data #'move-pos #'upd-way (complex 10 1))))
    
     (format t "Part 2~%")
     (format t "Manhattan: ~a~%" (manhattan end))

    (when *debug*
      (format t "data ~a~%" data)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
