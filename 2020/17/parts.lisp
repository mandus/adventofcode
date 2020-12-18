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
(defparameter *inp* (if *debug* "input_test.txt" "input.txt"))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(append (list 1 3) '(0 0))

(defun parse (lst mem &optional (extradim '(0)))
  (loop for line in lst
        for x = 0 then (1+ x)
        do (loop for state in (coerce line 'list)
                 for y = 0 then (1+ y)
                 for cell = (append (list x y) extradim)
                 when (char= #\# state)
                 do (setf (gethash cell mem) state))))

;; Nd game of life. 

(defun adj-inner (dim &optional (lst (list '(-1) '(0) '(1)))) 
  (let ((nxtd (1- dim))
        (nxtl (mapcan #'(lambda (d) (mapcar #'(lambda (x) (append `(,d) x)) lst)) '(-1 0 1))))
    (if (< 1 nxtd)
      (adj-inner nxtd nxtl)
      (remove-if #'(lambda (l) (zerop (reduce #'+ (mapcar #'abs l)))) nxtl))))

(defun adj (cell)
  (mapcar #'(lambda (l) (mapcar #'+ cell l)) (adj-inner (length cell))))

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

(defun coords (mins maxs &key (extend 0))
  (labels ((inner (lst &optional (dim 1))
             (let* ((dmin (nth dim mins))
                    (dmax (nth dim maxs))
                    (len (+ 1 (- dmax dmin) (* 2 extend)))
                    (start (- (nth dim mins) extend))
                    (nxtlst (mapcan #'(lambda (x) (mapcar #'(lambda (b) (append `(,x) b)) lst)) (alexandria:iota len :start start))))
               (if (>= (1+ dim) (length mins))
                   (mapcar #'reverse nxtlst)
                   (inner nxtlst (1+ dim))))))
    (let* ((len (+ 1 (- (first maxs) (first mins)) (* 2 extend)))
           (start (- (first mins) extend)))
      (inner (mapcar #'list (alexandria:iota len :start start))))))

(defun mem-updates (mem mins maxs)
  (loop for cell in (coords mins maxs :extend 1)
        for state = (gethash cell mem)
        for newstate = (update-cell cell mem)
        when (not (equalp state newstate))
        collect (cons cell newstate)))

(defun simulate (mem iter mins maxs)
  (let ((updates (mem-updates mem mins maxs)))
    (dolist (upd updates)
      (let* ((cell (car upd))
             (state (cdr upd))
             (curstate (gethash cell mem)))
        (when (or curstate (char= state #\#)) ; only update if already in hash or activating
         (setf (gethash cell mem) state))))

    (multiple-value-bind (mins maxs) (mem-range mem) 
      (when (< 0 iter)
        (simulate mem (1- iter) mins maxs)))))

(defun count-active (mem)
  (loop for state being the hash-value of mem
        count (char= state #\#)))

(defun mem-range (mem)
  (let* ((cells (loop for cell being the hash-key of mem collect cell))
         (mincoord (reduce #'(lambda (x y) (mapcar #'min x y)) cells))
         (maxcoord (reduce #'(lambda (x y) (mapcar #'max x y)) cells))
         )
    (values mincoord maxcoord)))

;; debug helper
;; 
(defun print-mem (mem mins maxs)
  ; only implemented for 3d
  (labels ((innerpr (&optional w) 
             (loop for k from (third mins) to (third maxs)
                   do (progn
                        (format t "~% z ~a ~%~%" k)
                        (loop for i from (first mins) to (first maxs)
                              do (progn
                                   (loop for j from (second mins) to (second maxs)
                                         for cell = (append (list i j k) w)
                                         for state = (gethash cell mem)
                                         do (format t "~a" (or state #\.)))
                                   (format t "~%")))))))

    (when (= (length mins) 3)
      (innerpr)
      (format t "~%----- next it -----~%"))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn ))
         (rows (length data))
         (cols (length (car data)))
         (mem (make-hash-table :test #'equalp)))

    (format t "Part 1~%")
    (parse data mem)
    (simulate mem 5 '(0 0 0) `(,rows ,cols 0))
    (format t "after simulation, ~a active ~%" (count-active mem))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "range: ~a~%" (multiple-value-list (mem-range mem)))
      )))

(defun part2 (fn)
  (let* ((data (read-input fn ))
         (rows (length data))
         (cols (length (car data)))
         (mem (make-hash-table :test #'equalp)))
    
     (format t "Part 2~%")
     (parse data mem '(0 0))
     (simulate mem 5 '(0 0 0 0) `(,rows ,cols 0 0))
     (format t "after simulation, ~a active~%" (count-active mem))

    (when *debug*
      (format t "data ~a~%" data)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
