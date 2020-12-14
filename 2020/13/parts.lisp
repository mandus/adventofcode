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

(defun parse-p1 (l)
  (remove nil (mapcar #'parse-only-integer (cl-ppcre:split "," l))))

(defun parse-p2 (l)
  (mapcar #'parse-only-integer (cl-ppcre:split "," l)))

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


(defun enumerate-clean (lst)
  (loop for i in lst
       for j = 0 then (1- j) ; turn sign for chinese remainder func
      when i collect (cons j i)))

;; from rosetta code
;;
;; extended euclidean algorithm for gcd
(defun egcd (a b)
  (do ((r (cons b a) 
          (cons (- (cdr r) (* (car r) q)) 
                (car r)))
       (s (cons 0 1) 
          (cons (- (cdr s) (* (car s) q))
                (car s)))
       (u (cons 1 0) 
          (cons (- (cdr u) (* (car u) q))
                (car u)))
       (q nil))
     ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))
     (setq q (floor (/ (cdr r) (car r))))))

(defun invmod (a m)
  (multiple-value-bind (r s k) (egcd a m)
    (declare (ignore k))
    (unless (= 1 r) (error "invmode: Values ~a and ~a are not coprimes." a m))
    s))

(defun chinese-reminder (am)
  "am = '((a1 . m1) (a2 . m2)...) a's reminders, m's coprimes"
  (loop for (a . m) in am
        with mtot = (reduce #'* (mapcar #'(lambda (X) (cdr X)) am))
        with sum = 0
        finally (return (mod sum mtot))
        do (incf sum (* a (invmod (/ mtot m) m) (/ mtot m)))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'parse-p1))
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
  (let* ((data (read-input fn #'parse-p2))
         (factors (enumerate-clean (cadr data))))
    
     (format t "Part 2~%")
     (format t "Answer: ~a~%" (chinese-reminder factors))

    (when *debug*
      (format t "data ~a~%" data)
      (format t "factors ~a~%" (cdr factors))
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
