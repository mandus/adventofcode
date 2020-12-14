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
(defparameter *inp* (if *debug* "input_test.txt" "input.txt"))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

;(logbitp 1 (parse-integer "11"))
; (parse-integer "101001" :radix 2)
; (= 1 (ldb (byte 1 0) 41)) ; 101001  = 41
; (= 0 (ldb (byte 1 1) 41)) ; 101001  = 41
; (= 0 (ldb (byte 1 2) 41)) ; 101001  = 41
; (= 1 (ldb (byte 1 3) 41)) ; 101001  = 41
; (= 0 (ldb (byte 1 4) 41)) ; 101001  = 41
; (= 1 (ldb (byte 1 5) 41)) ; 101001  = 41
; (= 0 (ldb (byte 1 6) 41)) ; 101001  = 41 - non-existing bit, implicit 0
; (= 40 (dpb 0 (byte 1 0) 41)) ; 101000 = 40
; (= 42 (dpb 1 (byte 1 1) (dpb 0 (byte 1 0) 41))) ; 101010 = 42 - update 2 bits
; (= 43 (dpb 1 (byte 1 1) 41)) ; 101011 = 43

; (= 42 (dpb-pos (dpb-pos 41 0 0) 1))
(defun dpb-pos (val pos &optional (bt 1))
  (dpb bt (byte 1 pos) val))

(defun parse-mask (mask) 
  (loop for i in (reverse (coerce mask 'list))
        for j = 0 then (1+ j) ; turn sign for chinese remainder func
        when (char/= #\X i) collect (cons j (digit-char-p i))))

(defun mask-val (mask val)
  "mask is a parsed mask, apply to val."
  (reduce #'(lambda (v m) (dpb-pos v (car m) (cdr m))) mask :initial-value val))

(defun re-mask (line) 
   (cl-ppcre:register-groups-bind (mask) ("mask = (.*)" line)
     (parse-mask mask)))

(defun re-mem (line)
  (cl-ppcre:register-groups-bind (mem val) ("mem\\[(.*)\\] = (.*)" line)
    (when (and mem val)
      (cons (parse-integer mem) (parse-integer val)))))

(defun iter (data h &optional mask)
  (let* ((nxt (cdr data))
         (line (car data))
         (mask (or (re-mask line) mask))
         (mem (re-mem line)))
    (when *debug*
      (format t "mask: ~a, mem: ~a~%" mask mem))
    (when mem
      (setf (gethash (car mem) h) (mask-val mask (cdr mem))))
    (when nxt
      (iter nxt h mask))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn ))
         (mem (make-hash-table :test #'equalp))
         )

    (format t "Part 1~%")
    (iter data mem)
    (format t "Answer ~a~%" (reduce #'+ (loop for value being the hash-values of mem collect value)))

    (when *debug*
      (format t "data: ~a~%" data)
      )))

(defun part2 (fn)
  (let* ((data (read-input fn))
         )
    
     (format t "Part 2~%")

    (when *debug*
      (format t "data ~a~%" data)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
