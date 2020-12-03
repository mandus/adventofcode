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

; default "transform" for reader
(defun noop (x) x)

(defun read-input (fn &optional (trans 'noop))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun transform (elm)
  (cl-ppcre:split " " elm))

(defun check-p1 (entry)
  "entry is ('min-max', 'str:', 'pw') - check if number of 'str' is within min-max in pw"
  (let* ((minmax (mapcar 'parse-integer (cl-ppcre:split "-" (car entry))))
         (chr (string-trim ":" (second entry)))
         (pw (third entry))
         ;(matches (cl-ppcre:all-matches-as-strings chr pw))
         ;(nummatches (length matches))
         ; can count directly actually:
         (nummatches (count chr pw :test 'string=))
         )
    (when *debug*
      (format t "~a ~a ~a: ~a [~a]~%" minmax chr pw matches nummatches))
    (if (and (<= (first minmax) nummatches)
             (>= (second minmax) nummatches))
        t
        nil)))

(defun pwcheck (pos pw chr &optional (cnt 0))
  (let* ((curpos (car pos))
         ;(match (string= pw chr :start1 (1- curpos) :end1 curpos))
         ; can also look up char and compare
         (match (string= (char pw (1- curpos)) chr))
         (newcnt (if match (1+ cnt) cnt))
         )
    (if (rest pos)
        (pwcheck (rest pos) pw chr newcnt)
        newcnt
        )))

(defun check-p2 (entry)
  "entry is ('pos1-pos2', 'str:', 'pw') - check if str is in exactly one of the positions"
  (let* ((pos (mapcar 'parse-integer (cl-ppcre:split "-" (car entry))))
         (chr (string-trim ":" (second entry)))
         (pw (third entry))
         (matches (pwcheck pos pw chr))
         )
    (when *debug* 
      (format t "~a ~a ~a: [~a]~%" pos chr pw matches))
    (if (= 1 matches) t nil)))


;; drivers
;;
(defun part1 (fn)
  (let* ((items (read-input fn 'transform))
         ;(checks (loop for d in items collect (check-p1 d)))
         ;(numtrue (count-if-not #'not checks))
         (directcount (count-if 'check-p1 items))
         )
    (format t "Part 1~%")
    ;(format t "true: ~a~%" numtrue)
    (format t "true: ~a~%" directcount)

    (when *debug*
      (format t "~a~%" items)
      ;(format t "~a~%" checks)
      ) 
    ))

(defun part2 (fn)
  (let* ((items (read-input fn 'transform))
         ;(checks (loop for d in items collect (check-p2 d)))
         ;(numtrue (count-if-not #'not checks))
         (directcount (count-if 'check-p2 items))
         )
    (format t "Part 2~%")
    ;(format t "true: ~a~%" numtrue)
    (format t "true: ~a~%" directcount)

    (when *debug*
      ;(format t "checks: ~a~%" checks)
      )
    ))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
