; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)

(uiop:define-package 
  :aoc
  (:use :cl)
  (:export :run)
  )

(unless swank::*connections* 
  (swank:create-server :port 4005 :dont-close t))

(in-package :aoc)

(defparameter *debug* nil)
(defparameter *inp* "input_test.txt")
;(defparameter *inp* "input.txt")

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))


(defun empty-line (line)
  (= 0 (length line)))
 
(defun data-to-groups (data fn)
  (loop for line in data
        for group = (funcall fn group line)
        for groups = nil then groups
        if (empty-line line)
        do (progn
             (setf groups (cons group groups))
             (setf group nil))
        finally (return (cons group groups)))) 

(defun upd-quiz (l line)
  (mapcar (lambda (x) (pushnew x l)) (coerce line 'list)) 
  l)  

(defun cons-if-line (gr l) 
  (when *debug*
    (format t "l: ~a, gr: ~a~%" l gr))
  (if (empty-line l)
      gr
      (cons l gr)))


(defun quiz-intersection (lst line)
  (when *debug* 
    (format t "~a - ~a~%" lst line))
  (let ((ll (coerce line 'list))) 
    (if lst
      (intersection lst ll :test #'string=) 
      ll))) 

(defun reduce-group (gr &optional acc notfirst)
  (if (and notfirst (not acc)) 
      nil ; acc is empty but not first - result must be nil
      (if (not (cdr gr)) ; last entry
          (quiz-intersection acc (car gr))
          (reduce-group (cdr gr) (quiz-intersection acc (car gr)) t))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn))
         (groups (data-to-groups data #'upd-quiz))
         (grouptrace (reduce #'+ (mapcar #'length groups)))
         )
    (format t "Part 1~%")
    (format t "group counts ~a~%" grouptrace)

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "groups ~a~%" groups)
      (format t "groups ~a~%" grouptrace)
      ) 
    ))

(defun part2 (fn)
  (let* ((data (read-input fn))
         (groups (data-to-groups data #'cons-if-line))
         (grouptrace (reduce #'+ (mapcar #'length (mapcar #'reduce-group groups))))
         )
    (format t "Part 2~%")
    (format t "group counts ~a~%" grouptrace)

    (when *debug*
      (format t "data ~a~%" data)
      (format t "groups ~a~%" groups)
      (format t "grouptrace ~a~%" grouptrace)
      )
    ))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))

; gather paragraphs more easy
;  - taken from reddit; I need to learn curry/compose - good stuff!
(ql:quickload :split-sequence)
(ql:quickload :alexandria)
(defparameter *file* (split-sequence:split-sequence "" (uiop:read-file-lines *inp*) :test #'equalp))
(defparameter *data* (mapcar (lambda (list) (mapcar (lambda (string) (coerce string 'list)) list)) *file*))

(defun count-responses (data set-operation)
  (reduce #'+ (mapcar (alexandria:compose #'length (alexandria:curry  #'reduce set-operation)) data)))

(mapcar (alexandria:curry #'count-responses *data*) (list #'union #'intersection))
