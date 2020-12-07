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
;(defparameter *inp* "input_test2.txt")
(defparameter *inp* "input.txt")

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun content-list (lst &optional (found nil))
  (let* ((bag (car (cl-ppcre:split " bag" (string-trim " " (car lst)))))
         (first-space (search " " bag))
         (bag-count (subseq bag 0 first-space))
         (bag-type (subseq bag  (1+ first-space)))
         (nxtfound (append (list bag-type bag-count) found))
         (more (cdr lst))
         ) 
    (cond
      ((string= bag "no other") found)
      ((not more) nxtfound)
      (t (content-list more nxtfound)))))

(defun add-contained (hash name content)
  (let ((bag (car content))
        (nxtcontent (cddr content)))
    (when *debug* 
      (format t "~a is contained in ~a~%" bag name))
    (setf (gethash bag hash) (cons name (gethash bag hash)))
    (if nxtcontent 
        (add-contained hash name nxtcontent)
        hash)))

(defun hashdata (data) 
  (let  ((bags (make-hash-table :test #'equalp))
         (contained (make-hash-table :test #'equalp))
         )
  (dolist (line data) 
    (let* ((bag (cl-ppcre:split " bags contain " line))
           (name (car bag))
           (content (content-list (cl-ppcre:split "," (cadr bag))))) 
      (when *debug*
        (format t "~a contains: ~a~%" name content))
      (setf (gethash name bags) content)
      (when content
        (add-contained contained name content)))) 
  (values bags contained)))

(defun contain-shiny-gold (contained candidates &optional found)
  (let (newcandidates) 
    (dolist (bag candidates)
      (dolist (check (gethash bag contained))
        (when (not (member check found :test #'string=)) 
          (pushnew check newcandidates :test #'string=)
          (pushnew check found :test #'string=))
        (when *debug*
          (format t "check ~a: cands ~a, found ~a~%" check newcandidates found))))
    (if newcandidates
        (contain-shiny-gold contained newcandidates found)
        found)))

(defun shiny-count (bags content)
  (let* ((bag (first content))
         (cnt (parse-integer (second content)))
         (morebags (cddr content)))
    (when *debug*
      (format t "count ~a of ~a which contains ~a and more bags ~a~%" cnt bag (gethash bag bags) morebags))
    (+ cnt 
       (if (gethash bag bags)
           (* cnt (shiny-count bags (gethash bag bags)))
           0)
       (if morebags
           (shiny-count bags morebags)
           0))))

;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn))
         )
    (format t "Part 1~%")

    (multiple-value-bind (bags contained) (hashdata data)
      (format t "Shiny gold can be in ~a bags~%" (length (contain-shiny-gold contained '("shiny gold"))))

      (when *debug*
        (format t "data: ~a~%" data)
        (maphash (lambda (key val) (format t "bag: ~a -> ~a~%" key val)) bags)
        (maphash (lambda (key val) (format t "contain: ~a -> ~a~%" key val)) contained)))))

(defun part2 (fn)
  (let* ((data (read-input fn)))
    (format t "Part 2~%")

    (multiple-value-bind (bags contained) (hashdata data) 
      (declare (ignorable contained))
      
      ; need to subtract the shiny gold bag as we only care about the content
      (format t "Shiny gold bag count: ~a~%" (- (shiny-count bags '("shiny gold" "1")) 1)))

      (when *debug*
        (format t "data ~a~%" data))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
