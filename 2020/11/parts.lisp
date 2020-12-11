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

(defun parse-line (line seats row &optional (col 0))
  (let ((pos (car line))
        (nxt (cdr line))
        (seat (cons row col)))
    (when *debug*
      (format t "(~a,~a): ~a " row col pos))
    (setf (gethash seat seats) pos)
    (when nxt
      (parse-line nxt seats row (1+ col)))))

(defun parse (lst seats &optional (row 0))
  (let ((line (car lst))
        (nxt (cdr lst)))
    (parse-line (coerce line 'list) seats row)
    (when *debug* 
      (format t "~%"))
    (when nxt
      (parse nxt seats (1+ row)))))

(defun adjacents (seat seats) 
  (declare
    (ignore seats))
  (let ((row (car seat))
        (col (cdr seat)))
    (loop for i from -1 to 1
          append (loop for j from -1 to 1
                   when (or (/= i 0) (/= j 0))
                   collect (cons (+ row i) (+ col j))))))

(defun view-seat (row col dx dy seats)
  "find first non-floor position in grid in direction (dx,dy)"
  (let* ((x (+ row dx))
        (y (+ col dy))
        (seat (cons x y))
        (pos (gethash seat seats)))
    (when *debug* 
      (format t "look at (~a,~a): ~a~%" x y pos))
    (cond ((not pos) nil) ; outside grid
          ((string= pos ".") (view-seat x y dx dy seats))
          (t seat))))

(defun adjacent-view (seat seats)
  (let ((row (car seat))
        (col (cdr seat)))
    (loop for i from -1 to 1
          append (loop for j from -1 to 1
                       when (or (/= i 0) (/= j 0))
                       collect (view-seat row col i j seats)))))

(defun all-empty (lst seats)
  (let ((pos (gethash (car lst) seats))
        (nxt (cdr lst)))
   (if (or (not pos) (string= pos "L") (string= pos "."))
       (if nxt
           (all-empty nxt seats)
           t)
       nil)))

(defun four-occup (lst seats limit &optional (cnt 0))
  (let* ((pos (gethash (car lst) seats))
         (nxtcnt (if (string= pos "#") (1+ cnt) cnt))
         (nxt (cdr lst)))
    (cond 
      ((>= nxtcnt limit) t)
      (nxt (four-occup nxt seats limit nxtcnt))
      (t (>= nxtcnt limit)))))

(defun update-seat (seat seats limit adjfn)
  (let ((pos (gethash seat seats)))
    (cond 
      ((string= pos "L") (if (all-empty (funcall adjfn seat seats) seats) "#" "L"))
      ((string= pos "#") (if (four-occup (funcall adjfn seat seats) seats limit) "L" "#"))
      (t "."))))

(defun seat-updates (seats limit adjfn)
  (loop for seat being the hash-key
        using (hash-value pos) of seats
        for newpos = (update-seat seat seats limit adjfn)
        when (string/= pos newpos)
        collect (cons seat newpos)))

(defun update-seats (seats updates)
  (dolist (upd updates)
    (let ((seat (car upd))
          (pos (cdr upd)))
      (setf (gethash seat seats) pos))))

(defun simulate (seats limit adjfn)
  (let ((updates (seat-updates seats limit adjfn)))
    (when *debug*
      (format t "updates: ~a~%" updates))
    (dolist (upd updates)
      (let ((seat (car upd))
            (pos (cdr upd)))
        (setf (gethash seat seats) pos)))
    (when updates
      (simulate seats limit adjfn))))

(defun count-occup (seats)
  (loop for pos being the hash-value of seats
        count (string= pos "#")))

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
    (simulate seats 4 #'adjacents)
    (format t "after simulation, ~a occupied~%" (count-occup seats))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "(2,4) ~a~%" (gethash (cons 2 4) seats))
      ;(format t "update: ~a~%" (update-seat (cons 2 4) seats))
      ;(format t "occupied: ~a~%" (count-occup seats))
      )))

(defun part2 (fn)
  (let* ((data (read-input fn ))
         (seats (make-hash-table :test #'equalp)))
    
     (format t "Part 2~%")
     (parse data seats)
     (simulate seats 5 #'adjacent-view)
     (format t "after simulation, ~a occupied~%" (count-occup seats))

    (when *debug*
      (format t "data ~a~%" data)
      (format t "adjacent-view (4,5): ~a ~%" (adjacent-view (cons 4 5) seats))
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
