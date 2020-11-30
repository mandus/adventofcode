; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --load parts.lisp

(ql:quickload :swank)

(uiop:define-package 
  :aoc
  (:use :cl))

(unless swank::*connections* 
  (swank:create-server :port 4005 :dont-close t))

(in-package :aoc)

(defparameter *debug* t)

(defparameter *inp* "input_test.txt")

(defun read-input (fn)
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect line)))

(defun transform (elm)
  (parse-integer elm))


(defun part1 (fn)
  (let* ((data (read-input fn))
         (items (loop for d in data collect (transform d)))
         (processed (reduce '+ items))
         )
    (format t "Part 1~%")
    (when *debug*
      (format t "~a~%" data))
      (format t "~a~%" items) 
      (format t "~a~%" processed) 
    )
  )

(defun part2 (fn)
  (let* ((data (read-input fn)))
    (format t "Part 2~%")
    t
    )
  )


(defun run ()
  (part1 *inp*)
  (part2 *inp*))
