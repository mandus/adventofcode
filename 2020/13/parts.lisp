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

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'parse))
         )

    (format t "Part 1~%")

    (when *debug*
      (format t "data: ~a~%" data)
      )))

(defun part2 (fn)
  (let* ((data (read-input fn #'parse))
         )
    
     (format t "Part 2~%")

    (when *debug*
      (format t "data ~a~%" data)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
