; AoC 2022 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)
(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(uiop:define-package 
  :aoc
  (:use :cl)
  (:export :run))

(unless swank::*connections* 
  (swank:create-server :port 5005 :dont-close t))

(in-package :aoc)

(defparameter *debug* nil)
;(defparameter *debug* t)

(defparameter *inp* "test_input.txt")
;(defparameter *inp* "input.txt")
(defun getinp ()  (if *debug* "test_input.txt" "input.txt"))


(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun spl-char (line)
  (when *debug*
    (format t "->[~a] ~a~%" (length line) line))
  (cl-ppcre:split "," line))

(defun parse-only-integer (val)
  (handler-case (parse-integer val)
    (t (c) (values val c))))

(defun parse (l)
  (remove nil (mapcar #'parse-only-integer (cl-ppcre:split "-" l))))

(defun solve-p1 (entry)
  (let* (
         (left (parse (first entry)))
         (right (parse (second entry)))
         (l0 (first left)) (l1 (second left))
         (r0 (first right)) (r1 (second right)))
    (when *debug*
      (format t "~a  -- ~a -> [~a ~a] [~a ~a] ~%" left right l0 l1 r0 r1))

    ; find overlap:
    ; left[0] <= right[0] && right[1] <= left[1]
    ; or
    ; right[0] <= left[0] && left[1] <= right[1]
    (or
      (and 
        (<= l0 r0) (<= r1 l1))
      (and
        (<= r0 l0) (<= l1 r1)))))

(defun range (left right &optional (step 1))
  (loop for n from left below right by step collect n))

(defun solve-p2 (entry)
  (let* ((left (parse (first entry)))
         (right (parse (second entry)))
         (l0 (first left)) (l1 (second left))
         (r0 (first right)) (r1 (second right))
         (left-set (range l0 (1+ l1)))
         (right-set (range r0 (1+ r1))))
    (when *debug*
      (format t "~a  -- ~a -> [~a] [~a] ~%" left right left-set right-set))

    ; can just check if left-set and right-set intersect
    (intersection left-set right-set)))

;; drivers
;;
(defun part1 (fn)
  (let* ((items (read-input fn #'spl-char)))
    (format t "Part 1~%")
    (format t "~a~%" (count-if #'solve-p1 items))

    (when *debug*
      (format t "~a~%" items)) ))

(defun part2 (fn)
  (let* ((items (read-input fn #'spl-char)))
    (format t "Part 2~%")
    (format t "~a~%" (count-if #'solve-p2 items))

    (when *debug*
      (format t "~a~%" items))
    ))

(defun run ()
  (part1 (getinp))
  (part2 (getinp)))

