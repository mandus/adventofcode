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

(defun trim-space (s)
  (string-trim " " s))

(defun split-space (s)
  (split-sequence:split-sequence #\Space s))

(defun parse-rules (lst h &optional a b)
  (let* ((rule (car lst))
         (spl (search ":" rule))
         (key (subseq rule 0 spl))
         (defs (mapcar #'split-space (mapcar #'trim-space (split-sequence:split-sequence #\| (subseq rule (+ 2 spl))))))
         (nxt (cdr lst)))
    ;(when *debug* (format t "~a [a: ~a, b: ~a] -> ~{ ~a ~}~%" key a b defs))

    (cond 
      ((and (= 1 (length (car defs))) (string= "\"a\"" (car (car defs)))) (progn (setf a key) (setf (gethash "a" h) key)))
      ((and (= 1 (length (car defs))) (string= "\"b\"" (car (car defs)))) (progn (setf b key) (setf (gethash "b" h) key)))
      (t (setf (gethash key h) defs)))

    (if nxt
        (parse-rules nxt h a b)
        (values a b))))


; take-1, generate all the strings. Works (slow though) for part1, does not work for part2

(defun gen-rule (rule rules a b &optional (res `(())))
  (let ((r (car rule))
        (nxt (cdr rule)))
    (if r (gen-rule nxt rules a b
                    (mapcan #'(lambda (l)
                                (mapcar #'(lambda (r)
                                            (append r l)) res)) (e-val r rules a b)))
        res)))

(defun gen-rules (lst rules a b)
  (mapcan #'(lambda (r) (gen-rule r rules a b)) lst))

(defun e-val (val rules a b)
  (cond ((string= val a) `((#\a))) 
        ((string= val b) `((#\b)))
        (t (gen-rules (gethash val rules) rules a b))))

(defun gen-parts (rule rules a b) 
  (mapcar #'(lambda (lst) (coerce lst 'string)) (gen-rules (gethash rule rules) rules a b)))

; take-2, observe that based on the rules, part 1 is strings "42 42 31" (refering to the rules)
; and in part 2 it is "42 42... 42 31 31...31", such that we have n*42 + m*42 + m*31, n>=1 and m>0
; 

(defun in (val lst)
  "return member list of string value in list"
  (member val lst :test #'string=))


(defun parts-chk (msg parts)
  (let* ((len (length (car parts)))
         (enough (>= (length msg) len)))
    (if (and enough (in (subseq msg 0 len) parts)) (subseq msg len)
        nil)))

(defun count-str (str lst)
  (count-if #'(lambda (l) (string= l str)) lst))

(defun p1-verify-parts (found) 
  (let ((p-31 (count-str "31" found))
        (p-42 (count-str "42" found)))
    (when *debug*
      (format t "p1-v-p: ~a~%" found))
    (and (= 1 p-31) (= 2 p-42))))

(defun p2-verify-parts (found)
  (let ((p-31 (count-str "31" found))
        (p-42 (count-str "42" found)))
    (when *debug*
      (format t "p2-v-p: ~a~%" found))
    (and (> p-31 0) (> p-42 p-31))))

(defun verify (msg p42 p31 predicate)
  (labels ((inner (msg chk42 &optional found) 
             (if (= 0 (length msg))
                 ; done - verify found
                 (funcall predicate found)
                 ; else 
                 (if chk42
                     (let ((res (parts-chk msg p42)))
                       (if res
                           (inner res t (cons "42" found))
                           (inner msg nil found)))
                     (let ((res (parts-chk msg p31)))
                       (if res
                           (inner res nil (cons "31" found))
                           ; need a 31 at the end - if not, it's a fail
                           nil))))))
    (inner msg t)))

;; 
;; "DP" version, based on ideas from reddit solution-thread (much slower than the "hack" tuned for the problem, but more general)
;; 

(defun str-match (str m l r)
  (string= str (subseq m l r)))

(defun rule-is (r chr)
  (string= r chr))

(defun match-range (msg left right rule nxt rules DP &optional (split 1))
  (let ((i (+ left split))) 

    ; (when *debug*
    ;   (format t "m-r: ~a [~a | ~a | ~a] ~a (~a)~%" msg left split right rule nxt))

    (or (and (match-msg msg left i rule rules DP) (match-rule-msg msg i right nxt rules DP))
        (and (< i right) (match-range msg left right rule nxt rules DP (1+ split))))))

(defun match-rule-msg (msg left right rulelist rules DP)
  (let* ((rule (car rulelist))
         (nxt (cdr rulelist)))

    ; (when *debug*
    ;   (format t "m-r-m: -- [~a - ~a] ~a (~a)~%" left right rule nxt))

    (cond 
      ((and (= left right) (not rule)) t)
      ((or (= left right) (not rule)) nil)
      (t (match-range msg left right rule nxt rules DP)))))

(defun match-rule-options-msg (msg left right options rules DP)
  (let* ((opt (car options))
         (nxt (cdr options))
         (res (match-rule-msg msg left right opt rules DP)))

    ; (when *debug*
    ;   (format t "m-r-ops: ~a [~a - ~a] ~a = ~a (~a) ~%" msg left right opt res nxt))

    (or res (and nxt (match-rule-options-msg msg left right nxt rules DP)))))

(defun match-msg (msg left right rule rules DP)
  (let* ((a (gethash "a" rules))
         (b (gethash "b" rules))
         (key (format nil "~a|~a|~a" left right rule)))
    (multiple-value-bind (dpval haskey) (gethash key dp)

      ; (when *debug*
      ;   (when haskey
      ;     (format t "m-m: DP lookup for ~a: ~a~%" key dpval)))

      (if haskey dpval
          (let ((res (or (and (rule-is rule a) (str-match "a" msg left right))
                         (and (rule-is rule b) (str-match "b" msg left right))
                         (match-rule-options-msg msg left right (gethash rule rules) rules DP))))
            (setf (gethash key DP) res))))))

;; drivers
;;
(defun part1 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         (msgs (cadr data))
         (rules (make-hash-table :test #'equalp))
         )

    (format t "Part 1~%")
    (multiple-value-bind (a b) (parse-rules (car data) rules)
      (let ((parts (gen-parts "0" rules a b))
            (p42 (gen-parts "42" rules a b))
            (p31 (gen-parts "31" rules a b))) 
        (format t "Answer (take1): ~a~%" (count-if #'identity (mapcar #'(lambda (msg) (in msg parts)) msgs)))
        (format t "Answer (take2): ~a~%" (count-if #'identity (mapcar #'(lambda (msg) (verify msg p42 p31 #'p1-verify-parts)) msgs)))
        (format t "Answer (take3): ~a~%" (count-if #'identity (mapcar #'(lambda (msg) (match-msg msg 0 (length msg) "0" rules (make-hash-table :test #'equalp))) msgs)))

        (when *debug*
          (format t "data: ~a~%" data)
          (format t "a is ~a and b is ~a~%" a b )
          )))))

(defun part2 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         (msgs (cadr data))
         (rules (make-hash-table :test #'equalp)))

    (format t "Part 2~%")
    (multiple-value-bind (a b) (parse-rules (car data) rules)

      (setf (gethash "8" rules) '(("42") ("42" "8")))
      (setf (gethash "11" rules) '(("42" "31") ("42" "11" "31")))

      (let ((p42 (gen-parts "42" rules a b))
            (p31 (gen-parts "31" rules a b)))

        (format t "Answer: ~a~%" (count-if #'identity (mapcar #'(lambda (msg) (verify msg p42 p31 #'p2-verify-parts)) msgs)))
        (format t "Answer (dp): ~a~%" (count-if #'identity (mapcar #'(lambda (msg) (match-msg msg 0 (length msg) "0" rules (make-hash-table :test #'equalp))) msgs)))

        (when *debug*
          (format t "data ~a~%" data)
          (format t "Rule 42, ~a~%" p42)
          (format t "Rule 31, ~a~%" p31)
          )))))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
