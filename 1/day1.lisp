; AoC 2019 day 1 - Åsmund Ødegård

;;
;; run: sbcl --noinform --load <file.lisp> --quit
;; compile and run: sbcl --noinform --eval "(compile-file \"<file.lisp>\")" --quit
;;                  sbcl --load <file.fasl> --quit


(unless (find-package :iterate)
  (ql:quickload "iterate"))

(defpackage :aoc-2019-01
  (:use :common-lisp))

(in-package :aoc-2019-01)
(use-package :iterate)

(defvar *debug* nil)

(defun read-input (fn)
  (iter (for line in-file fn using #'read-line)
        (collect (parse-integer line))))

(defun fuel-for-mass (mass)
  (- (truncate  mass 3) 2))

(defun fuel-for-mass-rec (mass fuels)
  (let ((f (fuel-for-mass mass)))
    (if (> f 0)
      (fuel-for-mass-rec f (+ f fuels))
      fuels)))

(defun main (fn)
  (let* ((data (read-input fn))
         (part1 (iter (for m in data) (collect (fuel-for-mass m))))
         (part2 (iter (for m in part1) (collect (fuel-for-mass-rec m m))))
         )

    (when *debug* 
      (progn 
        (format t "data: ~{ ~a ~} " data)
        (format t "~%fuels1: ~{ ~a ~} " part1)
        (format t "~%fuels2: ~{ ~a ~} " part2)))
    
    (format t "~%part1: ~a~%" (reduce '+ part1))
    (format t "part2: ~a~%" (reduce '+ part2))))

(main "input.txt")

;(defun init()
;    (let ((fn "input.txt"))
;      (if (probe-file fn)
;        (main fn)
;        (when *debug*
;          (format t "File ~A does not exists~%" fn)))))
;
;(init)
