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
;(defparameter *inp* "input_test.txt")
(defparameter *inp* "input.txt")


(defun read-input (fn)
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect line)))

(defun transform (elm)
  (cl-ppcre:split " " elm))

(defun checktree (elm)
  (when *debug*
    (format t "~a~%" elm))
  (string= elm "#"))

(defun counttrees (data right down) 
  (count-if 'checktree 
            (loop for line in data 
                  for x = 0 then (mod (+ x right) (length line))
                  for y = 0 then (mod (1+ y) down)
                  collect (when (= y 0) (char line x)))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn)))
    (format t "Part 1~%")

	(format t "num trees: ~a~%" (counttrees data 3 1))

    (when *debug*
      (format t "~a~%" data)
      ) 
    ))

(defun part2 (fn)
  (let* ((data (read-input fn))
         (slopes `((1 1) (3 1) (5 1) (7 1) (,(/ 1 2) 2)))
         (slopetrees (loop for trav in slopes
                        collect (counttrees data (first trav) (second trav))))
         )
    (format t "Part 2~%")
    (format t "trace ~a~%" (reduce '* slopetrees))

    (when *debug*
      (format t "data ~a~%" data)
      (format t "checks ~a~%" slopes)
      )
    ))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
