; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)
(ql:quickload :alexandria)

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

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun char-bit (chr)
  (let ((chrmap (pairlis 
                  '("F" "B" "L" "R") 
                  '(#\0 #\1 #\0 #\1)))) 
    (cdr (assoc chr chrmap :test #'string=))))

(defun dec-row (seat) 
  (parse-integer (subseq seat 0 7) :radix 2))

(defun dec-column (seat) 
  ; my first attempt
  ; (parse-integer (coerce (mapcar #'char-bit (coerce (subseq seat 7 10) 'list)) 'string) :radix 2)]
  ; better to just map directly; as suggested from landimatte@reddit 
  ; and then also map all bits in one go (in seat-to-id), saves a lot of map'ing!
  (parse-integer (subseq seat 7 10) :radix 2))

(defun seat-to-id (binseat) 
  (let* ((seat (map 'string #'char-bit binseat))
         (row (dec-row seat))
         (column (dec-column seat)))
    (+ (* row 8) column)))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'seat-to-id))
         )
    (format t "Part 1~%")
    (format t "max id: ~a~%" (apply #'max data))

    (when *debug*
      (format t "data: ~a~%" data))))

(defun part2 (fn)
  (let* ((data (read-input fn #'seat-to-id))
         (minid (apply #'min data))
         (maxid (apply #'max data))
         (allids (alexandria:iota (- maxid minid) :start minid))
         (candidates (remove-if (lambda (x) (member x data)) allids)))
    (format t "Part 2~%")
    (format t "Candidate(s): ~a~%" candidates)

    (when *debug*
      (format t "data ~a~%" data)
      (format t "min ~a~%" minid)
      (format t "max ~a~%" maxid)
      (format t "cands ~a~%" candidates))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
