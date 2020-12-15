; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

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
(defparameter *inp* (if *debug* "input_test2.txt" "input.txt"))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun dpb-pos (val pos &optional (bt 1))
  "update bit-position pos in value with given bit (default 1)"
  (dpb bt (byte 1 pos) val))

(defun parse-mask-p1 (mask) 
  (loop for i in (reverse (coerce mask 'list))
        for j = 0 then (1+ j) 
        when (char/= #\X i) collect (cons j (digit-char-p i))))

(defun convert-mask-bit (i)
  (if (char= #\X i) 0
      (digit-char-p i)))

(defun parse-mask-p2 (mask) 
  ; returns mask converted into a list of ((pos . bit)...)
  (loop for i in (reverse (coerce mask 'list))
        for j = 0 then (1+ j)
        when (char/= #\0 i) collect (cons j (convert-mask-bit i))))


(defun buildlist-p2 (m lst) 
  (if (not m) nil ; if we got a mem-line, and thus nil, we exit early
      (if (= (cdr m) 1)
          ; bit is 1 - always apply
          (if lst
              (loop for lm in lst collect (cons m lm))
              (list (list m)))
          ; else bit is X ("0" here) - create both variants
          (if lst
              (append (loop for lm in lst collect (cons (cons (car m) 0) lm))
                      (loop for lm in lst collect (cons (cons (car m) 1) lm)))
              (list (list (cons (car m) 0)) (list (cons (car m) 1)))))))

(defun masklists-p2 (mask &optional lst)
  (let* ((m (car mask))
        (nxt (cdr mask))
        (nxtlst (buildlist-p2 m lst)))

    (when *debug*
      (format t "part: ~a [~a] <- ~a~%" m nxt lst))

    (if (not nxt)
        nxtlst
        (masklists-p2 nxt nxtlst))))

(defun mask-val (mask val)
  "mask is a parsed mask, apply to val."
  (reduce #'(lambda (v m) (dpb-pos v (car m) (cdr m))) mask :initial-value val))


(defun re-mask (line &optional (parsefn #'parse-mask-p1)) 
   (cl-ppcre:register-groups-bind (mask) ("mask = (.*)" line)
     (funcall parsefn mask)))

(defun re-mem (line)
  (cl-ppcre:register-groups-bind (mem val) ("mem\\[(.*)\\] = (.*)" line)
    (when (and mem val)
      (cons (parse-integer mem) (parse-integer val)))))

(defun iter-p1 (data h &optional mask)
  (let* ((nxt (cdr data))
         (line (car data))
         (mask (or (re-mask line) mask))
         (mem (re-mem line)))
    (when *debug*
      (format t "mask: ~a, mem: ~a~%" mask mem))
    (when mem
      (setf (gethash (car mem) h) (mask-val mask (cdr mem))))
    (when nxt
      (iter-p1 nxt h mask))))

(defun iter-p2 (data h &optional masks)
  (let* ((nxt (cdr data))
         (line (car data))
         (masks (or (masklists-p2 (re-mask line #'parse-mask-p2)) masks))
         (mem (re-mem line)))
  (when *debug*
    (format t "masks: ~a, mem: ~a~%" masks mem)) 

  (when mem
    (dolist (mask masks)
      (setf (gethash (mask-val mask (car mem)) h) (cdr mem))))
  
  (when nxt
    (iter-p2 nxt h masks))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn ))
         (mem (make-hash-table :test #'equalp))
         )

    (format t "Part 1~%")
    (iter-p1 data mem)
    (format t "Answer ~a~%" (reduce #'+ (loop for value being the hash-values of mem collect value)))

    (when *debug*
      (format t "data: ~a~%" data)
      )))

(defun part2 (fn)
  (let* ((data (read-input fn))
         (mem (make-hash-table :test #'equalp)))
    
     (format t "Part 2~%")
     (iter-p2 data mem)
     (format t "Answer ~a~%" (reduce #'+ (loop for value being the hash-values of mem collect value)))

    (when *debug*
      (format t "data ~a~%" data)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
