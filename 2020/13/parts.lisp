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

;; general utils
;;

(defun parse-only-integer (val)
  (handler-case (parse-integer val)
    (t (c) (values nil c))))

(defun parse (l)
  (remove nil (mapcar #'parse-only-integer (cl-ppcre:split "," l))))
 
(defun alist-val (lst key) 
  "return value in alist for given string key"
  (cdr (assoc key lst :test #'string=)))

(defun in (val lst)
  "return member list of string value in list"
  (member val lst :test #'string=))

(defun case-str (str)
  (intern (string-upcase str)))


(defun first-route (ts routes &optional id wait)
  (let* ((route (car routes))
        (nxt (cdr routes))
        (routewait (- route (mod ts route)))
        (nxtwait (if wait (min wait routewait) routewait))
        (nxtid (cond ((not wait) route) ((< routewait wait) route) (t id)))
        )
    (if nxt
        (first-route ts nxt nxtid nxtwait)
        (values id wait))))

(< 1 2)
(mod 939 7)
(min 7 8)

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'parse))
         (ts (car (car data)))
         (routes (car (cdr data))))

    (format t "Part 1~%")
    (multiple-value-bind (id wait) (first-route ts routes)
      (format t "answer: ~a~%" (* id wait)))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "ts ~a~%" ts)
      (format t "routes ~a~%" routes)
      )))

(defun part2 (fn)
  (let* ((data (read-input fn #'parse))
         )
    
     (format t "Part 2~%")

    (when *debug*
      (format t "data ~a~%" data)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
