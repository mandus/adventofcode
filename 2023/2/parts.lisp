; AoC 2023 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)
(ql:quickload :str)

(uiop:define-package
  :aoc
  (:use :cl)
  (:export :run))


(in-package :aoc)

(defparameter *debug* t)
;(defparameter *debug* nil)

(defparameter *inp* "t1.txt")
;(defparameter *inp* "input.txt")

(when *debug*
  (unless swank::*connections*
  (swank:create-server :port 4015 :dont-close t)))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))


(defun solve (data)
  data)

;; drivers
;;
(defun part1 (fn)
  (let* ((items (read-input fn)))
    (format t "Part 1~%")
    (format t "~a~%" (solve items))

    (when *debug*
      (format t "~a~%" items)) ))

(defun part2 (fn)
  (let* ((items (read-input fn)))
    (format t "Part 2~%")
    (format t "~a~%" (solve items))

    (when *debug*
      (format t "~a~%" items))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
