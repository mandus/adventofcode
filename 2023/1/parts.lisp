; AoC 2023 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)
(ql:quickload :str)

(uiop:define-package
  :aoc
  (:use :cl)
  (:export :run))


(in-package :aoc)

(defparameter *debug* t)
(defparameter *debug* nil)

(defparameter *inp* "t1.txt")
(defparameter *inp* "input.txt")

(when *debug*
  (unless swank::*connections*
  (swank:create-server :port 4015 :dont-close t)))

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun get-num (val)
    (let* ((nums (remove-if-not #'digit-char-p val))
           (l (length nums))
           (fi (subseq nums 0 1))
           (la (subseq nums (1- l) l)))
      (parse-integer (concatenate 'string fi la))))

(defun check-replace (s k v)
  "if s starts with k, replace with v"
  (if (str:starts-with-p k s)
      (str:replace-first k v s)
      s))

(defun run-match (s p)
  (let* ((tst (car p))
         (np (cdr p))
         (k (car tst))
         (v (cdr tst))
         (ns (check-replace s k v)))
    (if np
        (run-match ns np)
        ns)))

(defun matcher (s)
  (let ((p '(("one" . "1ne")
             ("two" . "2wo")
             ("three" . "3hree")
             ("four" . "4our")
             ("five" . "5ive")
             ("six" . "6ix")
             ("seven" . "7even")
             ("eight" . "8ight")
             ("nine" . "9ine")
             )))
    (run-match s p)))

(defun traverse (data &optional (r ""))
  (let* ((s (matcher data))
         (strl (length s)))
    (progn
      (when *debug*
        (format t "len: ~a for ~a~%" strl s))
      (if (> strl 0)
          (traverse (subseq s 1) (concatenate 'string r (subseq s 0 1)))
          (progn
            (when *debug*
              (format t "~% --> will return: ~a~%" r))
            r)))))

(defun get-num-p2 (val)
  (let* ((nums (remove-if-not #'digit-char-p (traverse val)))
         (l (length nums))
         (fi (subseq nums 0 1))
         (la (subseq nums (1- l) l)))
    (progn
      (when *debug*
        (format t "~a: ~a (l ~a) [~a : ~a]~%" val nums l fi la))
      (parse-integer (concatenate 'string fi la)))))

(defun solve (data)
  (reduce #'+ data))

;; drivers
;;
(defun part1 (fn)
  (let* ((items (read-input fn #'get-num)))
    (format t "Part 1~%")
    (format t "~a~%" (solve items))

    (when *debug*
      (format t "~a~%" items)) ))

(defun part2 (fn)
  (let* ((items (read-input fn #'get-num-p2)))
    (format t "Part 2~%")
    (format t "~a~%" (solve items))

    (when *debug*
      (format t "~a~%" items))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
