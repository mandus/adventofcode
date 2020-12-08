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
(defparameter *inp* "input.txt")

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun dispatch ()
  (let ((tbl (make-hash-table :test #'equalp)))
    (setf (gethash "nop" tbl) (lambda (pos val reg) (values (1+ pos) reg)))
    (setf (gethash "jmp" tbl) (lambda (pos val reg) (values (+ pos val) reg)))
    (setf (gethash "acc" tbl) (lambda (pos val reg) (values (1+ pos) (+ reg val))))
    tbl))

(defun mapop (op)
  (cdr (assoc op (pairlis '("jmp" "nop") '("nop" "jmp")) :test #'string=)))

(defun fix (op pos dofix fixhash)
  (cond ((not dofix) (values op dofix))
        ((string= "acc" op) (values op dofix))
        ((gethash pos fixhash) (values op dofix))
        (t (progn
             (setf (gethash pos fixhash) t)
             (values (mapop op) (not dofix))))))

(defun runops (instr &optional dofix fixhash)
  (let ((mark (make-hash-table :test #'equalp))
        (ops (dispatch)))
    (labels ((loopit (pos accu dofix)
               (setf (gethash pos mark) t)
               (destructuring-bind (op data) (nth pos instr)
                 (multiple-value-bind (op dofix) (fix op pos dofix fixhash)
                  (multiple-value-bind (pos accu) (funcall (gethash op ops) pos data accu)
                   (if (and (< pos (length instr))
                            (not (gethash pos mark)))
                       (loopit pos accu dofix)
                       (values accu (>= pos (length instr)))))))))
      (loopit 0 0 dofix))))

(defun split (l)
  (let ((sp (search " " l)))
    (list (subseq l 0 sp) (parse-integer (subseq l (1+ sp))))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'split))
         )
    (format t "Part 1~%")
    (multiple-value-bind (accu end) (runops data)
      (declare 
        (ignore end))
      (format t "Accu: ~a~%" accu))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "first: ~a~%" (car data))
      )))

(defun part2 (fn)
  (let* ((data (read-input fn #'split))
         (fixhash (make-hash-table :test #'equalp)))
    (labels ((loopit ()
               (multiple-value-bind (accu end) (runops data t fixhash)
                 (when *debug*
                   (format t "accu: ~a, end ~a~%" accu end))
                 (if end
                     accu
                     (loopit)))))
     (format t "Part 2~%")
     (format t "Accu: ~a~%" (loopit)))

    (when *debug*
      (format t "data ~a~%" data))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
