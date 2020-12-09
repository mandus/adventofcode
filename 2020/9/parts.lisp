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

(defun preamble (data size &optional window)
  (let ((next (pop data))) 
    (when *debug* 
      (format t "[~a] window: ~a, next: ~a, data: ~a~%" size window next data)
      )
    (if (= 0 size)
      (values window next data)
      (preamble data (1- size) (append window (list next))))))

(defun findsum (lst val)
  (let ((left (car lst))
        (searchlst (rest lst)))
    (labels ((inner (left searchlst)
               (when *debug*
                 (format t "search ~a-~a in ~a~%" val left searchlst))
               (cond 
                 ((not searchlst) nil)
                 ((member (- val left) searchlst) t)
                 (t (inner (pop searchlst) searchlst)))))
      (inner left searchlst)))) 

(defun first-not (pre next data)
  (if (findsum pre next)
      (multiple-value-bind (pre next data) (preamble data 0 (append (rest pre) (list next)))
        (first-not pre next data))
      next))

(defun search-part-1 (data window-size)
  (multiple-value-bind (pre next restdata) (preamble data window-size)
    (first-not pre next restdata)))

(defun search-contig  (num data)
  (labels ((inner (data &optional lst)
             (let ((lstsum (reduce #'+ lst))
                   (next (pop data))) 
               (when *debug* 
                 (format t "data: ~a, candidates: ~a, sum: ~a, next: ~a~%" data lst lstsum next))
               (cond 
                 ((= num lstsum) lst)
                 ((< num lstsum) (inner (push next data) (cdr lst))) ; now sliding window. Originally just nil here, throw away and start over
                 (t (inner data (append lst (list next))))
                 ))))
    (let ((inner-sum (inner data)))
      (if (not inner-sum) ; with sliding window, we should never get here.
        (search-contig num (rest data))
        inner-sum))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'parse-integer))
         (window-size (if *debug* 5 25)) ; different window-size for test-input and real input!
         )
    (format t "Part 1~%")
    (format t "First not: ~a~%" (search-part-1 data window-size))

    (when *debug*
      (format t "data: ~a~%" data)
      (multiple-value-bind (pre next restdata) (preamble data window-size)
        (format t "pre: ~a, next: ~a, data: ~a~%" pre next restdata)
        (multiple-value-bind (pre next restdata) (preamble restdata 0 (append (rest pre) (list next)) )
          (format t "pre: ~a, next: ~a, data: ~a~%" pre next restdata)
          )
        )
      )))

(defun part2 (fn)
  (let* ((data (read-input fn #'parse-integer))
         (window-size (if *debug* 5 25))
         (search-num (search-part-1 data window-size))
         (contig-lst (search-contig search-num data))
         (min-contig (reduce #'min contig-lst))
         (max-contig (reduce #'max contig-lst))
         )
    
     (format t "Part 2~%")
     (format t "encryption weakness: ~a~%" (+ min-contig max-contig))


    (when *debug*
      (format t "data ~a~%" data)
      (format t "search-num: ~a~%" search-num)
      (format t "search contigous: ~a~%" contig-lst)
      (format t "min contigous: ~a~%" min-contig)
      (format t "max contigous: ~a~%" max-contig)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
