; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)
(ql:quickload :cl-ppcre)
(ql:quickload :split-sequence)

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

(defun parse-only-integer (val)
  (handler-case (parse-integer val)
    (t (c) (values val c))))

(defun tokenize (str &optional lst val)
  (let ((nxt (subseq str 0 (max 0 (1- (length str)))))
        (e (char str (1- (length str))))
        (intval (if val (parse-integer (coerce val 'string)))))
    ;(when *debug* (format t "tokenize: ~a and ~a [~a] -> ~a (~a)~%" nxt e str lst intval))
    (if (= 0 (length nxt))
        (cond ((char= e #\)) (cons e lst))
              ((digit-char-p e) (cons (parse-integer (coerce (cons e val) 'string)) lst))
              ((char= e #\() (remove nil (cons e (cons intval lst))))
              (t lst))
        (cond ((char= #\) e) (tokenize nxt (cons e lst) val))
              ((digit-char-p e) (tokenize nxt lst (cons e val)))
              ((char= #\( e) (tokenize nxt (cons e (cons intval lst)) nil))))))

(defun tokenize-string (part)
  (let ((val (parse-only-integer part))) 
    (cond ((numberp val) (list val))
          ((= 1 (length part)) (list (char part 0)))
          ((stringp part) (tokenize part)))))

(defun parse (l)
  (mapcan #'tokenize-string (cl-ppcre:split " " l)))

(defun operate (op val)
  (cond 
    ((char= (second op) #\+) (+ val (first op))) 
    ((char= (second op) #\*) (* val (first op)))))

(defun handle-lvl (stack val lvl &optional (add-prec nil))
  (let* ((op (car stack))
         (nxtop (second stack))
         (nxtstack (cdr stack))
         (nxtval (operate op val)))

     (if (not add-prec)
      (if (and nxtop (>= (third nxtop) lvl))
         (handle-lvl nxtstack nxtval lvl)
         (values nxtstack nxtval))

      (cond
        ((and nxtop (or (> (third op) lvl) 
                        (and (= (third op) lvl) (char= (second op) #\+)))) 
         (handle-lvl nxtstack nxtval lvl add-prec))

        ((or (> (third op) lvl) 
             (and (= (third op) lvl) (char= (second op) #\+))) 
         (values nxtstack nxtval))

        (t (values stack val))))))

(defun nxt-op-lvl (lst lvl)
  ; find first value not a closing paren, count down levels if needed
  (cond ((not lst) (values lst lvl))
        ((char= #\) (car lst)) (nxt-op-lvl (cdr lst) (1- lvl)))
        (t (values lst lvl))))


(defun process (lst add-prec &optional (stack nil) (lvl 0))
  (let* ((x (car lst))
         (nxt (cdr lst))
         (has-lvl-stack (and (car stack) (= (third (car stack)) lvl)))
         )
    (cond 
      ((and (numberp x) has-lvl-stack)
       (multiple-value-bind (nxt nxt-lvl) (nxt-op-lvl nxt lvl) 
         (multiple-value-bind (stack nxt-val) (handle-lvl stack x nxt-lvl add-prec)
           (if nxt
               (process (cdr nxt) add-prec (cons (list nxt-val (car nxt) nxt-lvl) stack) nxt-lvl)
               (if stack  ; safe for part-1, but only needed for part-2
                   (multiple-value-bind (stack final-val) (handle-lvl stack nxt-val 0)
                     (declare (ignore stack))
                     final-val)
                   nxt-val)
               )))) ; finish the stack at the end
      ((and (numberp x) (not has-lvl-stack)) 
       (process (cdr nxt) add-prec (cons (list x (car nxt) lvl) stack) lvl))
      ((char= x #\()
       (process nxt add-prec stack (1+ lvl))))))

(defun process-default (lst)
  (process lst nil))

(defun process-with-prec (lst)
  (process lst t))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn #'parse)))

    (format t "Part 1~%")
    (format t "Answer: ~a~%" (reduce #'+ (mapcar #'process-default data)))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "values: ~a~%" (mapcar #'process-default data))
      (format t "values match: ~a~%" (equal (mapcar #'process-default data) '(71 51 26 437 12240 13632))) 
      )))

(defun part2 (fn)
  (let* ((data (read-input fn #'parse)))

    (format t "Part 2~%")
    (format t "Answer: ~a~%" (reduce #'+ (mapcar #'process-with-prec data)))

    (when *debug*
      (format t "data ~a~%" data)
      (format t "values: ~a~%" (mapcar #'process-with-prec data))
      (format t "values match: ~a~%" (equal (mapcar #'process-with-prec data) '(231 51 46 1445 669060 23340))) 
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
