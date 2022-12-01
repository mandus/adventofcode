; AoC 2022 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

;(ql:quickload :swank)
(ql:quickload :split-sequence)

(uiop:define-package 
  :aoc
  (:use :cl)
  (:export :run))

; (unless swank::*connections* 
;   (swank:create-server :port 4005 :dont-close t))

(in-package :aoc)

(defparameter *debug* nil)
;(defparameter *debug* t)

;(defparameter *inp* "test_input.txt")
(defparameter *inp* "input.txt")

(defun read-by-para (fn &optional (split "") (trans #'identity))
  (mapcar trans (split-sequence:split-sequence split (uiop:read-file-lines fn) :test #'equalp)))

(defun split-lines (data)
  (when *debug*
    (format t " - ~a ~%" data))
  (mapcan (lambda (line) (split-sequence:split-sequence #\Newline line)) data))


(defun solve (data)
  (sort (loop for l in data
              collect (loop for num in l
                            summing (parse-integer num))) '>))

;; drivers
;;
(defun part1 (fn)
  (let* ((items (read-by-para fn "" #'split-lines)))
    (format t "Part 1~%")
    (format t "~a~%" (first (solve items)))

    (when *debug*
      (format t "~a~%" items)) ))

(defun part2 (fn)
  (let* ((items (read-by-para fn "" #'split-lines))
         (agglist (solve items))
         (vals (subseq agglist 0 3)))
    (format t "Part 2~%")
    (format t "~a~%" (reduce '+ vals))

    (when *debug*
      (format t "checks: ~a~%" vals))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
