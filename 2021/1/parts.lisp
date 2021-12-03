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

; default "transform" for reader
(defun noop (x) x)

(defun read-input (fn &optional (trans 'noop))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun counter (l &optional (acc 0))
  (if (= 1 (length l)) 
      acc
      (let ((x (first l)) 
            (nxt (rest l)))
        (if (< x (first nxt)) 
            (counter nxt (1+ acc))
            (counter nxt acc)))))

(defun summer (l idx &optional (acc 0))
  (when *debug* 
    (format t "s ~a (acc ~a) ~a~%" idx acc l))
  (if (zerop idx)
      acc
      (summer (rest l) (1- idx) (+ acc (first l)))))

(defun countslices (l len &optional (acc 0))
  (when *debug* 
    (format t "cs ~a ~a >= ~a~%" acc (length l) len))
  (if (<= (length l) len)
      acc
      (let* ((nxt (rest l))
            (x (summer l len))
            (y (summer nxt len)))
        (when *debug* 
          (format t "cmp ~a ~a ~%" x y))
        (if (< x y)
            (countslices nxt len (1+ acc))
            (countslices nxt len acc)))))

;; drivers
;;
(defun part1 (fn)
  (let* ((items (read-input fn #'parse-integer))
         ;(checks (loop for d in items collect (check-p1 d)))
         ;(numtrue (count-if-not #'not checks))
         )
    (format t "Part 1~%")
    (format t "~a~%" (counter items))

    (when *debug*
      (format t "~a~%" items)) ))

(defun part2 (fn)
  (let* ((items (read-input fn #'parse-integer))
         ;(checks (loop for d in items collect (check-p2 d)))
         ;(numtrue (count-if-not #'not checks))
         )
    (format t "Part 2~%")
    (format t "~a~%" (countslices items 3))

    (when *debug*
      (format t "checks: ~a~%" items))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
