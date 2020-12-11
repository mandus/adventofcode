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

(defun parse (lst seats)
  (loop for line in lst
        for row = 0 then (1+ row)
        do (loop for pos in (coerce line 'list)
                 for col = 0 then (1+ col)
                 for seat = (cons row col)
                 do (setf (gethash (cons row col) seats) pos))))

(defun adj-seat (row col dx dy seats)
  (declare 
    (ignore seats))
  (cons (+  row dx) (+ col dy)))

(defun view-seat (row col dx dy seats)
  "find first non-floor position in grid in direction (dx,dy)"
  (let* ((x (+ row dx)) (y (+ col dy))
        (seat (cons x y))
        (pos (gethash seat seats)))
    (when *debug* 
      (format t "look at (~a,~a): ~a~%" x y pos))
    (cond ((not pos) nil) ; outside grid
          ((string= pos ".") (view-seat x y dx dy seats))
          (t seat))))

(defun adjacents (seat seats fn)
  (let ((row (car seat))
        (col (cdr seat)))
    (loop for i from -1 to 1
          append (loop for j from -1 to 1
                       when (or (/= i 0) (/= j 0))
                       collect (funcall fn row col i j seats)))))

(defun empty-p (lst seats)
  (let ((pos (gethash (car lst) seats))
        (nxt (cdr lst)))
   (if (or (not pos) (string= pos "L") (string= pos "."))
       (if nxt
           (empty-p nxt seats)
           t)
       nil)))

(defun occup-p (lst seats limit &optional (cnt 0))
  (let* ((pos (gethash (car lst) seats))
         (nxtcnt (if (string= pos "#") (1+ cnt) cnt))
         (nxt (cdr lst)))
    (cond 
      ((>= nxtcnt limit) t)
      (nxt (occup-p nxt seats limit nxtcnt))
      (t (>= nxtcnt limit)))))

(defun update-seat (seat seats limit fn)
  (let ((pos (gethash seat seats)))
    (cond 
      ((string= pos "L") (if (empty-p (adjacents seat seats fn) seats) "#" "L"))
      ((string= pos "#") (if (occup-p (adjacents seat seats fn) seats limit) "L" "#"))
      (t "."))))

(defun seat-updates (seats limit fn)
  (loop for seat being the hash-key
        using (hash-value pos) of seats
        for newpos = (update-seat seat seats limit fn)
        when (string/= pos newpos) 
        collect (cons seat newpos)))

(defun update-seats (seats updates)
  (dolist (upd updates)
    (let ((seat (car upd))
          (pos (cdr upd)))
      (setf (gethash seat seats) pos))))

(defun simulate (seats limit fn)
  (let ((updates (seat-updates seats limit fn)))
    (when *debug*
      (format t "updates: ~a~%" updates))
    (dolist (upd updates)
      (let ((seat (car upd))
            (pos (cdr upd)))
        (setf (gethash seat seats) pos)))
    (when updates
      (simulate seats limit fn))))

(defun count-occup (seats)
  (loop for pos being the hash-value of seats
        count (string= pos "#")))

;; debug helper
;; 
(defun print-seats (seats rows cols)
  (loop for i from 0 to (1- rows)
        do (progn
             (loop for j from 0 to (1- cols)
                 for pos = (gethash (cons i j) seats)
                 do (format t "~a" pos))i
             (format t "~%"))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn ))
         (seats (make-hash-table :test #'equalp)))

    (format t "Part 1~%")
    (parse data seats)
    (simulate seats 4 #'adj-seat)
    (format t "after simulation, ~a occupied~%" (count-occup seats))

    (when *debug*
      (format t "data: ~a~%" data)
      )))

(defun part2 (fn)
  (let* ((data (read-input fn ))
         (seats (make-hash-table :test #'equalp)))
    
     (format t "Part 2~%")
     (parse data seats)
     (simulate seats 5 #'view-seat)
     (format t "after simulation, ~a occupied~%" (count-occup seats))

    (when *debug*
      (format t "data ~a~%" data)
      (format t "adjacent-view (4,5): ~a ~%" (adjacents (cons 4 5) seats #'view-seat))
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
