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

(defun parse (l)
  (cons (subseq l 0 1) (parse-integer (subseq l 1))))

(defun upd-pos (pos dir instr move)
  (cond
    ((member instr '("L" "R") :test #'string=) pos)
    (t (cond
         ((string= instr "N") (cons (car pos) (+ (cdr pos) move)))
         ((string= instr "S") (cons (car pos) (- (cdr pos) move)))
         ((string= instr "E") (cons (+ (car pos) move) (cdr pos)))
         ((string= instr "W") (cons (- (car pos) move) (cdr pos)))
         ((string= instr "F") (cons (+ (car pos) (* (car dir) move)) 
                                    (+ (cdr pos) (* (cdr dir) move))))))))

(defun upd-dir (dir instr move)
  (let* ((left (complex 0 1))
         (right (complex 0 -1))
         (pwr (/ move 90))
         (cdir (complex (car dir) (cdr dir)))
         (ldir (* cdir (expt left pwr)))
         (rdir (* cdir (expt right pwr))))

    (cond
      ((member instr '("N" "S" "E" "W" "F") :test #'string=) dir)
      (t (cond
           ((string= instr "L") (cons (realpart ldir) (imagpart ldir)))
           ((string= instr "R") (cons (realpart rdir) (imagpart rdir)))
           (t nil)
           )))))

(defun upd (lst &optional (pos (cons 0 0)) (dir (cons 1 0)))
 (let* ((upd (car lst))
        (instr (car upd))
        (move (cdr upd))
        (nxtpos (upd-pos pos dir instr move))
        (nxtdir (upd-dir dir instr move))
        (nxt (cdr lst))
        )
   (if nxt
       (upd nxt nxtpos nxtdir)
       (values nxtpos nxtdir))))

(defun manhattan (pos)
  (+ (abs (car pos)) (abs (cdr pos))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'parse))
         (end (upd data))
         )

    (format t "Part 1~%")
    (format t "Manhattan: ~a~%" (manhattan end))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "end ~a~%" end)
      )))

(defun part2 (fn)
  (let* ((data (read-input fn ))
         )
    
     (format t "Part 2~%")

    (when *debug*
      (format t "data ~a~%" data)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
