; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --load parts.lisp

(ql:quickload :swank)
(ql:quickload :cl-ppcre)

(uiop:define-package 
  :aoc
  (:use :cl)
  (:export :run)
  )

(unless swank::*connections* 
  (swank:create-server :port 4005 :dont-close t))

(in-package :aoc)

(defparameter *debug* nil)
;(defparameter *inp* "test_input.txt")
(defparameter *inp* "input.txt")

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun counter (l cmp &optional (acc 0))
  (if (= cmp (length l)) 
      acc
      (let* ((x (first l)) 
             (y (nth cmp l))
             (nxt (rest l))
             (nxtacc (if (< x y) (1+ acc) acc)))
        (counter nxt cmp nxtacc))))

;; drivers
;;
(defun part1 (fn)
  (let* ((items (read-input fn #'parse-integer)))
    (format t "Part 1~%")
    (format t "~a~%" (counter items 1))

    (when *debug*
      (format t "~a~%" items)) ))

(defun part2 (fn)
  (let* ((items (read-input fn #'parse-integer)))
    (format t "Part 2~%")
    (format t "~a~%" (counter items 3))

    (when *debug*
      (format t "checks: ~a~%" items))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
