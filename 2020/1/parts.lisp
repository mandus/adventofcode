; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --load parts.lisp

(ql:quickload :swank)

(uiop:define-package 
  :aoc
  (:use :cl))

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
  (parse-integer elm))

(defun searcher (elm lst)
  (let* ((nxt (car lst))
         (tst (+ elm nxt))
         ) 
    (when *debug*
      (format t "~a ~a ~a~%" elm nxt tst))
    (cond 
      ((< 2020 tst )(values nil nil))
      ((> 2020 tst )
       (if (rest lst) (searcher elm (rest lst))
           (values nil nil)))
      ((= 2020 tst )(values elm nxt))
      ))
  )

(defun trisearch (e1 e2 lst)
  (let* ((e3 (car lst))
         (tst (+ e1 e2 e3)))
    (when *debug*
      (format t "~a ~a ~a: ~a~%" e1 e2 e3 tst))
    (cond
      ((< 2020 tst) (values nil nil nil))
      ((> 2020 tst)
       (if (rest lst) (trisearch e1 e2 (rest lst))
           (values nil nil nil)))
      ((= 2020 tst) (values e1 e2 e3))
      ))
  )

(defun traverse (lst)
  (let ((elm (car lst))
        (nxtlst (rest lst))) 
    (when *debug* 
      (format t "search on ~a~%" elm))
    (multiple-value-bind (l r) (searcher elm nxtlst)
      (if (not (and l r)) 
          (traverse nxtlst)
          (values l r)))))


(defun twotrav (e1 lst)
  (let ((e2 (car lst))
        (nxtlst (rest lst)))
    (when *debug* 
      (format t "search on ~a and ~a (~a)~%" e1 e2 nxtlst))
    (multiple-value-bind (r1 r2 r3) (trisearch e1 e2 nxtlst)
      (if (and (rest nxtlst) (not (and r1 r2 r3)))
          (twotrav e1 nxtlst)
          (values r1 r2 r3)))))

(defun tritrav (lst)
  (let ((elm (car lst))
        (nxtlst (rest lst)))
    (when *debug*
      (format t "search on ~a~%" elm))
    (multiple-value-bind (e1 e2 e3) (twotrav elm nxtlst)
      (if (and nxtlst (not (and e1 e2 e3)))
          (tritrav nxtlst)
          (values e1 e2 e3)))))

(defun part1 (fn)
  (let* ((data (read-input fn))
         (items (sort (loop for d in data collect (transform d)) '<))
         ;(processed (reduce '+ items))
         )
    (format t "Part 1~%")
    (multiple-value-bind (l r) (traverse items)
      (format t "~a and ~a: ~a~%" l r (* l r))
      )
    (when *debug*
      (format t "~a~%" data)
      (format t "~a~%" items)) 
      ;(format t "~a~%" processed) 
    )
  )

(defun part2 (fn)
  (let* ((data (read-input fn))
         (items (sort (loop for d in data collect (transform d)) '<))
         )
    (format t "Part 2~%")
    (multiple-value-bind (e1 e2 e3) (tritrav items)
      (format t "~a, ~a, ~a: ~a~%" e1 e2 e3 (* e1 e2 e3)))
    )
  )


(defun run ()
  (part1 *inp*)
  (part2 *inp*))
