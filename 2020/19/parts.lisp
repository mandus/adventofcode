; AoC 2020 - Åsmund Ødegård
;
; run: sbcl --quit --load parts.lisp --eval '(aoc:run)'

(ql:quickload :swank)
(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(uiop:define-package 
  :aoc
  (:use :cl)
  (:export :run))

(unless swank::*connections* 
  (swank:create-server :port 4005 :dont-close t))

(in-package :aoc)

(defparameter *debug* t)
;(defparameter *inp* "input_test.txt")
(defparameter *inp* (if *debug* "input_test.txt" "input.txt"))

(defun read-by-para (fn &optional (split "") (trans #'identity))
  (mapcar trans (split-sequence:split-sequence split (uiop:read-file-lines fn) :test #'equalp)))

(defun split-lines (data)
  (when *debug*
    (format t " - ~a - ~%" data))
  (mapcan (lambda (line)(split-sequence:split-sequence #\Newline line)) data))

(defun trim-space (s)
  (string-trim " " s))

(defun split-space (s)
  (split-sequence:split-sequence #\Space s))

(defun parse-rules (lst h &optional a b)
  (let* ((rule (car lst))
         (spl (search ":" rule))
         (key (subseq rule 0 spl))
         (defs (mapcar #'split-space (mapcar #'trim-space (split-sequence:split-sequence #\| (subseq rule (+ 2 spl))))))
        (nxt (cdr lst))
        )
    ;(when *debug* (format t "~a [a: ~a, b: ~a] -> ~{ ~a ~}~%" key a b defs))

    (cond 
      ((and (= 1 (length (car defs))) (string= "\"a\"" (car (car defs)))) (setf a key))
      ((and (= 1 (length (car defs))) (string= "\"b\"" (car (car defs)))) (setf b key))
      (t (setf (gethash key h) defs)))

    (if nxt
      (parse-rules nxt h a b)
      (values a b))))

(defun direct-match (chr val a b)
  (or (and (char= #\a chr) (string= val a))
      (and (char= #\b chr) (string= val b))))

(let ((str "aba"))
  (direct-match (car (coerce str 'list)) "3" "4" "5"))

(defun match-pos (chr pos def rules a b)
  (cond 
    
    ((direct-match chr (nth pos def) a b) t)
    )

  )

(defun match-rules (str rules a b &optional matched lst)
  (let ((c (car str))
        (nxt (cdr str))
        )
   (cond 
    ((not lst ) (loop for key being the hash-key
                      using (def hash-value) of rules
                      for match = (match-pos c 0 def a b) 
                      when match collect match))
    
    ))
  
  )


(defun gen-rule (lst rules a b &optional res)
  ; assume lst reversed so we can build the list from the end 
  (let ((val (car lst))
        (nxt (cdr lst))
        (rule (gethash val rules))
        )

    (if (or (= val a) (= val b))
        (mapcar #'(lambda (x) (cons val x)) lst)
        )

    )
  )




(mapcan #'(lambda (d) (mapcar #'(lambda (x) (cons d x)) (mapcar #'(lambda (x) (list 2 x)) (mapcan #'(lambda (x) (list 3 x)) (list 4))))) `(1 2))

;; drivers
;;
(defun part1 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         (rules (make-hash-table :test #'equalp))
         )

    (format t "Part 1~%")
    (multiple-value-bind (a b ) (parse-rules (car data) rules)


      (when *debug*
        (format t "data: ~a~%" data)

      (format t "a is ~a and b is ~a~%" a b )
      (maphash #'(lambda (key val) (format t "~a => ~a ~%" key val)) rules)

 
        ))))

(defun part2 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         )

    (format t "Part 2~%")

    (when *debug*
      (format t "data ~a~%" data)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
