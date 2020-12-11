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

(defparameter *debug* t)
;(defparameter *inp* "input_test.txt")
(defparameter *inp* (if *debug* "input_test.txt" "input.txt"))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))


;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn ))
         )

    (format t "Part 1~%")

    (when *debug*
      (format t "data: ~a~%" data)
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
