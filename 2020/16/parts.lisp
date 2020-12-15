; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)

(uiop:define-package 
  :aoc
  (:use :cl)
  (:export :run))

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
(defun part1 (fn &optional inpdata)
  (let* ((data (or inpdata (read-input fn)))
         )

    (format t "Part 1~%")

    (when *debug*
      (format t "data: ~a~%" data)
      )

    0 ; return for test
    ))

(defun part2 (fn &optional inpdata)
  (let* ((data (or inpdata (read-input fn)))
         )

    (format t "Part 2~%")

    (when *debug*
      (format t "data ~a~%" data)
      )

    0 ; return for test
    ))

(defun test-p1 ()
  (format t "Part 1 tests~%")
  (format t "~a~%" (= (part1 *inp* '(1 2 3)) ))
  )

(defun test-p2 ()
  (format t "Part 2 tests~%")
  (format t "~a~%" (= (part2 *inp* '(1 2 2)) ))
  )

(defun run ()
  (when *debug*
    (test-p1))
  (part1 *inp*)
  (when *debug*
    (test-p2))
  (part2 *inp*))
