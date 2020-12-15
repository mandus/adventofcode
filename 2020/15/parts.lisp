; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)
(ql:quickload :cl-ppcre)

(uiop:define-package 
  :aoc
  (:use :cl)
  (:export :run))

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

(defun parse (l)
  (mapcar #'parse-integer (cl-ppcre:split "," l)))

(defun enumerate-list (lst &optional (start 0))
  (loop for i in lst
        for j = start then (1+ j) 
        collect (cons j i)))

(defun read-out-input (data mem)
  (let* ((vals (enumerate-list data 1))) 
    (when *debug*
      (format t "enum-vals: ~a~%" vals))
    (dolist (val vals)
      (setf (gethash (cdr val) mem ) (cons (car val) nil)))
     (length data)))

(defun take-turns-until (until turn spoken mem)
  (let* ((mem-val (gethash spoken mem))
        (nxtturn (1+ turn))
        speak) 

    (when *debug*
      (format t "spoke ~a at turn ~a (before: ~a)~%" spoken turn mem-val))

    (if (= turn until) 
      spoken
      (progn
        (if (not (cdr mem-val)) ; not spoken before
          (setf speak 0)
          (setf speak (- (car mem-val) (cdr mem-val))))

        (setf (gethash speak mem) (cons nxtturn (car (gethash speak mem))))
        (take-turns-until until nxtturn speak mem)))))

;; drivers
;;
(defun part1 (fn &optional inpdata)
  (let* ((data (or inpdata (car (read-input fn #'parse))))
         (mem (make-hash-table :test #'equalp))
         (turn (read-out-input data mem))
         (prev-spoken (elt data (1- turn))))

    (format t "Part 1~%")
    (format t "Spoken at 2020: ~a~%" (take-turns-until 2020 turn prev-spoken mem))

    (when *debug*
      (format t "data: ~a~%" data)
      )))

(defun part2 (fn &optional inpdata)
  (let* ((data (or inpdata (car (read-input fn #'parse))))
         (mem (make-hash-table :test #'equalp))
         (turn (read-out-input data mem))
         (prev-spoken (elt data (1- turn)))
         (last-spoken (take-turns-until 30000000 turn prev-spoken mem)))

    (format t "Part 2~%")
    (format t "Spoken at 30000000: ~a~%" last-spoken)

    (when *debug*
      (format t "data ~a~%" data))

    ; output value for testing
    last-spoken))

(defun test-p2 ()
  (format t "~a~%" (= (part2 *inp* '(0 3 6)) 175594))
  (format t "~a~%" (= (part2 *inp* '(1 3 2)) 2578))
  (format t "~a~%" (= (part2 *inp* '(2 1 3)) 3544142))
  (format t "~a~%" (= (part2 *inp* '(1 2 3)) 261214))
  (format t "~a~%" (= (part2 *inp* '(2 3 1)) 6895259))
  (format t "~a~%" (= (part2 *inp* '(3 2 1)) 18))
  (format t "~a~%" (= (part2 *inp* '(3 1 2)) 362))
  (format t "~a~%" (= (part2 *inp* '(18 8 0 5 4 1 20)) 13710))) 

(defun run ()
  (part1 *inp*)
  ;(test-p2)
  (part2 *inp*))
