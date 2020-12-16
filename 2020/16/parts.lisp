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

(defparameter *debug* nil)
;(defparameter *inp* "input_test.txt")
(defparameter *inp* (if *debug* "input_test2.txt" "input.txt"))

(defun read-by-para (fn &optional (split "") (trans #'identity))
  (mapcar trans (split-sequence:split-sequence split (uiop:read-file-lines fn) :test #'equalp)))

(defun split-lines (data)
  (when *debug*
    (format t " - ~a - ~%" data))
  (mapcan (lambda (line)(split-sequence:split-sequence #\Newline line)) data))

(defun re-field (line)
  (cl-ppcre:register-groups-bind (field s1 e1 s2 e2) ("(.*): (.*)-(.*) or (.*)-(.*)" line)
    (cons field (list (list (parse-integer s1) (parse-integer e1)) (list (parse-integer s2) (parse-integer e2))))))

(defun fields (lines &optional ranges parsed-fields)
  ; line: "text: from-to or from-to"
  (let* ((line (car lines))
         (field (re-field line))
         (linerange (cdr field))
         (nxtranges (append linerange ranges))
         (nxtparsed (append (list field) parsed-fields))
         (nxt (cdr lines)))
    (when *debug*
      (format t "field: ~a~%" field)
      (format t "ranges: ~a~%" linerange)
      (format t "parsed fields: ~a~%" nxtparsed))
    (if (not nxt) (values nxtparsed nxtranges)
        (fields nxt nxtranges nxtparsed))))

(defun in-some-range (val ranges)
  (when *debug*
    (format t "i-s-r: ~a in ~a~%" val ranges))
  (loop for r in ranges 
        when (<= (first r) val (second r))
        collect (list val r)))

(defun parse-tkts (tkts ranges &optional outsiders valids)
  (let* ((tkt (car tkts))
         (lst (mapcar #'parse-integer (cl-ppcre:split "," tkt)))
         (outs (loop for val in lst when (not (in-some-range val ranges)) collect val))
         (nxtouts (append outs outsiders))
         (nxtvalids (if outs valids (append (list lst) valids)))
         (nxt (cdr tkts)))
    (when *debug*
      (format t "tkt: ~a [~a] -> ~a | ~a ~%" tkt lst outs nxtvalids))
    (if (not nxt) (values nxtvalids nxtouts)
        (parse-tkts nxt ranges nxtouts nxtvalids))))

(defun in (val lst)
  "return member list of string value in list"
  (member val lst :test #'string=))

(defun find-unique (options &optional unique)
  (let* ((opt (car options))
         (nxt (cdr options))
         (nxtuniq (if (= 1 (length opt)) (cons (car opt) unique) unique)))
    (when *debug*
      (format t "f-u: ~a ===> ~a~%" opt nxtuniq))
    (if (not nxt) nxtuniq 
        (find-unique nxt nxtuniq))))

(defun check-tkt (tkt fields options &optional checked)
  (let* ((val (car tkt))
         (nxt (cdr tkt))
         (opts (car options))
         (nxtopts (cdr options))
         (cand (loop for f in fields
                     for name = (car f)
                     when (and (in-some-range val (rest f)) (in name opts))
                     collect name))
         (nxtchecked (cons cand checked)))
    (when *debug*
      (format t "c-t: ~a ~a -> ~a [~a]~%" val opts cand nxtchecked))
    (if (not nxt) (reverse nxtchecked)
        (check-tkt nxt fields nxtopts nxtchecked))))

(defun assign-fields (tkts fields options)
  (let* ((tkt (car tkts))
         (nxt (cdr tkts))
         (chkd (check-tkt tkt fields options)))
    (when *debug*
      (format t "a-f tkt: ~a (~a) -> ~a~%" tkt options chkd))
    (if (not nxt) chkd
        (assign-fields nxt fields chkd))))

(defun reduce-options (options &optional (cnt 1000)) ; the cnt is just for safe-guard in case of troubles
  ; reduce set until everyone has just one option
  (let* ((removals (find-unique options))
         (nxtoptions (loop for opt in options 
                           collect (loop for o in opt 
                                         when (or (= 1 (length opt)) (not (in o removals))) collect o)))
         (done (every #'identity (loop for opt in nxtoptions collect (= 1 (length opt))))))
    (when *debug*
      (format t "red-ops: ~a [~a] -> ~a~%" removals nxtoptions done))
    (if (or (= 0 cnt) done) nxtoptions
        (reduce-options nxtoptions (1- cnt)))))

(defun find-values (lst str &optional vals)
  (let* ((elm (car lst))
         (nxt (cdr lst))
         (nxtvals (if (search str (car elm)) (cons (cdr elm) vals) vals)))
    (if (not nxt) nxtvals
        (find-values nxt str nxtvals))))

;; drivers
;;
(defun part1 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         (fields (multiple-value-list (fields (first data))))
         (ranges (second fields))
         (tkts (cdr (third data)))
         (parsed-tkts (multiple-value-list (parse-tkts tkts ranges)))
         (valid-tkts (first parsed-tkts))
         (outsiders (second parsed-tkts)))

    (format t "Part 1~%")
    (format t "trace: ~a~%" (reduce #'+ outsiders))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "f[~a] ~a~%" (length ranges) ranges)
      (format t "t ~a~%" tkts)
      (format t "v-t ~a~%" valid-tkts)
      (format t "o ~a~%" outsiders)
      )))

(defun part2 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         (fields (multiple-value-list (fields (first data))))
         (fieldnames (mapcar #'car (first fields)))
         (mytkt (mapcar #'parse-integer (cl-ppcre:split "," (car (cdr (second data))))))
         (tkts (cdr (third data)))
         (parsed-tkts (multiple-value-list (parse-tkts tkts (second fields))))
         (valid-tkts (first parsed-tkts))
         (options (assign-fields valid-tkts (first fields) (loop for x in (car valid-tkts) collect fieldnames)))
         (assigned (mapcan #'identity (reduce-options options)))
         (mytkt-mapped (pairlis assigned mytkt))
         )

    (format t "Part 2~%")
    (format t "product ~a~%" (reduce #'* (find-values mytkt-mapped "departure")))

    (when *debug*
      (format t "data ~a~%" data)
      (format t "fields ~a~%" (first fields))
      (format t "fieldnames ~a~%" fieldnames)
      (format t "v-t ~a~%" valid-tkts)
      (format t "opts ~a~%" options)
      (format t "assigned ~a~%" assigned)
      (format t "my tkt ~a~%" mytkt)
      (format t "mapped tkt ~a~%" mytkt-mapped)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
