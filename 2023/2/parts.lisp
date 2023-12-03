; AoC 2023 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)
(ql:quickload :str)
(ql:quickload :cl-ppcre)

(uiop:define-package
  :aoc
  (:use :cl)
  (:export :run))


; problem when interning symbols (e.g. red vs. aoc::red) -
; need to use other keys (string?) in asocciation list
;(in-package :aoc)

; (defparameter *debug* t)
(defparameter *debug* nil)

; (defparameter *inp* "t1.txt")
(defparameter *inp* "input.txt")

(when *debug*
  (unless swank::*connections*
  (swank:create-server :port 4015 :dont-close t)))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun parse (l)
  (cdr (cl-ppcre:split "[,:;]" l)))

(defun check (g)
  (let ((goal '(12 13 14))
        (m (loop for k in '(red green blue)
                 collect (cdr (assoc k g)))))
    (every #'identity (mapcar #'>= goal m))))

(defun game (l &optional (al (pairlis '(red green blue) '(0 0 0))))
  (let* ((f (cl-ppcre:split " " (str:trim (car l))))
         (c (read-from-string (car (cdr f))))
         (v (parse-integer (car f)))
         (cur_v (cdr (assoc c al)))
         (r (cdr l)))
    (progn
      (when *debug*
        (format t "~a -> ~a|~a [~a]~%" c cur_v v al))
      (setf (cdr (assoc c al)) (max v cur_v))
      (if r
          (game r al)
          (values (check al) al)))))

(defun power (g)
  (let ((pwr (reduce #'* (loop for k in '(red green blue)
                               collect (cdr (assoc k g))))))
    (progn
      (when *debug*
        (format t "power of ~a: ~a~%" g pwr))
      pwr)))

(defun solve (data &optional (c 1) (e nil) (pwr nil))
  (let* ((f (car data))
         (r (cdr data)))
    (multiple-value-bind (upd al) (game f)
      (let ((c_inc (if upd c 0))
            (g_pwr (power al)))
        (progn
          (when *debug*
            (format t "upd: ~a: ~a~%" upd al))
          (if r
              (solve r (1+ c) (cons c_inc e) (cons g_pwr pwr))
              (values (reduce #'+ (cons c_inc e)) (reduce #'+ (cons g_pwr pwr)))))))))

;; drivers
;;
(defun part1 (fn)
  (let* ((items (read-input fn #'parse)))
    (format t "Part 1~%")
    (multiple-value-bind (g pwr) (solve items)
      (declare (ignore pwr))
      (format t "~a~%" g)

      (when *debug*
        (format t "~a~%" items)))))

(defun part2 (fn)
  (let* ((items (read-input fn #'parse)))
    (format t "Part 2~%")
    (multiple-value-bind (g pwr) (solve items)
      (declare (ignore g))
      (format t "~a~%" pwr)

      (when *debug*
        (format t "~a~%" pwr)))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
