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

(defparameter *debug* nil)
;(defparameter *inp* "input_test.txt")
(defparameter *inp* (if *debug* "input_test.txt" "input.txt"))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun reducediff (lst diffs prev)
  (let* ((next (car lst))
         (nxtlst (cdr lst))
         (diff (- next prev))
         )
    (when *debug*
      (format t "~a - ~a = ~a; [~a] ~a~%" next prev diff nxtlst diffs))
    (setf (gethash diff diffs) (1+ (gethash diff diffs 0)))
    (when nxtlst (reducediff nxtlst diffs next))))

(defun add-fact (fact cnt)
  (cond ((< cnt 3) (cons 1 fact))
        ((= cnt 3) (cons 2 fact))
        ((= cnt 4) (cons 4 fact))
        ((= cnt 5) (cons 7 fact))))

(defun trav (lst &optional fact (cnt 0) (prev 0))
  (let* ((cur (car lst))
         (next (cdr lst))
         (contig-p (>= (1+ prev) cur))
         )
    (when *debug*
      (format t "~a ~a ~a [~a] cnt ~a, fact ~a~%" prev cur next contig-p cnt fact))
    (if (not next)
        (if contig-p
            (add-fact fact (1+ cnt))
            (add-fact fact cnt)) 
        (if contig-p
            (trav next fact (1+ cnt) cur)
            (trav next (add-fact fact cnt) 1 cur)
            ))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'parse-integer))
         (adapt (sort (copy-seq data) #'<))
         (diffs (make-hash-table :test #'equalp))
         )
    (format t "Part 1~%")
    (reducediff adapt diffs 0)
    ; add an extra entry for step=3 at the end
    (setf (gethash 3 diffs) (1+ (gethash 3 diffs 0)))
    (format t "result: ~a~%" 
            (reduce #'* (loop for value being the hash-values of diffs collect value)))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "data: ~a~%" adapt)
      )))

(defun part2 (fn)
  (let* ((data (read-input fn #'parse-integer))
         ; add the initial state as head as a start with 1,2.. then may give more options
         (adapt (cons 0 (sort (copy-seq data) #'<)))
         (factors (trav adapt)))
    
     (format t "Part 2~%")
     (format t "result: ~a~%" (reduce #'* factors))


    (when *debug*
      (format t "data ~a~%" data)
      (format t "sorted ~a~%" adapt)
      (format t "factors ~a~%" factors)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
