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

(defun parse (lst mem)
  (loop for line in lst
        for x = 0 then (1+ x)
        do (loop for state in (coerce line 'list)
                 for y = 0 then (1+ y)
                 for cell = (list x y 0)
                 when (char= #\# state)
                 do (setf (gethash cell mem) state))))

;; 3d game of life. - need to generalize to N dimensions for part-2.

(defun adj (cell)
  (let ((x (first cell))
        (y (second cell))
        (z (third cell)))
    (loop for i from -1 to 1
          append (loop for j from -1 to 1
                       append (loop for k from -1 to 1
                                    when (or (/= i 0) (/= j 0) (/= k 0))
                                    collect (list (+ x i) (+ y j) (+ z k)))))))

(defun active-adj (cell mem)
  (loop for c in (adj cell) 
        for state = (gethash c mem)
        count (and state (char= state #\#))))

(defun update-cell (cell mem)
  (let ((state (gethash cell mem)))
    (cond 
      ((and state (char= state #\#)) (if (<= 2 (active-adj cell mem) 3) #\# #\.))
      (t (if (= 3 (active-adj cell mem)) #\# #\.)) ; handle either state #\. or nil
    )))


(defun mem-updates (mem minx maxx miny maxy minz maxz)
  (loop for i from (1- minx) to (1+ maxx)
        append (loop for j from (1- miny) to (1+ maxy)
                 append (loop for k from (1- minz) to (1+ maxz)
                          for cell = (list i j k)
                          for state = (gethash cell mem)
                          for newstate = (update-cell (list i j k) mem)
                          ;do (when *debug* (format t "m-u: (~a,~a,~a) ~a -> ~a ~%" i j k state newstate))
                          when (not (equalp state newstate))
                          collect (cons cell newstate)))))


(defun simulate (mem iter rows cols &optional (layers 1) (minx 0) (miny 0) (minz 0))
  (let ((updates (mem-updates mem minx rows miny cols minz layers)))
    ; (when *debug*
    ;   (format t "updates: ~a~%" updates))
    (dolist (upd updates)
      (let* ((cell (car upd))
             (state (cdr upd))
             (curstate (gethash cell mem)))
        (when (or curstate (char= state #\#)) ; only update if already in hash or activating
         (setf (gethash cell mem) state))))

    ; (when updates  ; this loops until steady-state, which will never happen..
    ;   (simulate mem))
    ;
    (multiple-value-bind (minx rows miny cols minz layers) (mem-range mem) 
      (when *debug*
        (print-mem mem minx rows miny cols minz layers))
      (when (< 0 iter)
        (simulate mem (1- iter) rows cols layers minx miny minz)))))



(defun count-active (mem)
  (loop for state being the hash-value of mem
        count (char= state #\#)))

(defun mem-range (mem)
  (loop for cell being the hash-key of mem
        for x = (first cell)
        for y = (second cell)
        for z = (third cell)
        for minx = x then (min minx x)
        for maxx = x then (max maxx x)
        for miny = y then (min miny y)
        for maxy = y then (max maxy y)
        for minz = z then (min minz z)
        for maxz = z then (max maxz z)
        finally (return (values minx maxx miny maxy minz maxz))))

;; debug helper
;; 
(defun print-mem (mem minrow rows mincol cols minlayer layers)
  (loop for k from minlayer to layers
        do (progn
             (format t "~% layer ~a ~%~%" k)
             (loop for i from minrow to rows
                   do (progn
                        (loop for j from mincol to cols
                              for state = (gethash (list i j k) mem)
                              do (format t "~a" (or state #\.)))
                        (format t "~%")))
             )))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn ))
         (rows (length data))
         (cols (length (car data)))
         (mem (make-hash-table :test #'equalp)))

    (format t "Part 1~%")
    (parse data mem)
    (simulate mem 5 rows cols)
    (format t "after simulation, ~a active ~%" (count-active mem))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "range: ~a~%" (multiple-value-list (mem-range mem)))
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
