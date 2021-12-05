; Some maybe useful AoC tools.


;; reading files; parsing

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun parse (l)
  (cons (subseq l 0 1) (parse-integer (subseq l 1))))

(defun parse-only-integer (val)
  (handler-case (parse-integer val)
    (t (c) (values val c))))

; convert integers from string, remove not empty values
(defun parse (l)
  (remove nil (mapcar #'parse-only-integer (cl-ppcre:split "," l))))

; same as above, but leave non-numbers
(defun parse (l)
  (mapcar #'parse-only-integer (cl-ppcre:split "," l)))
 
; split file by paragraph (by "split")

(ql:quickload :split-sequence)
(defun read-by-para (fn &optional (split "") (trans #'identity))
  (mapcar trans (split-sequence:split-sequence split (uiop:read-file-lines fn) :test #'equalp)))

; in combination with the above, split individual lines by "split" and, and concatenate
(ql:quickload :cl-ppcre)
(defun split-by-space (lst &optional (split " "))
  (format t "->[~a] ~a~%" (length lst) lst)
  (mapcan (lambda (line) (cl-ppcre:split split line)) lst))

; examples:

; (dolist (d (read-by-para "../4/input_test.txt" "" #'split-by-space)) (format t "d[~a]: ~a~2%" (length d) d))

; (mapcar (lambda (list) (mapcar (lambda (string) (coerce string 'list)) 
;                                list)) 
;         (read-by-para "../6/input_test.txt"))


;; string as key utils
 
(defun alist-val (lst key) 
  "return value in alist for given string key"
  (cdr (assoc key lst :test #'string=)))

(defun in (val lst)
  "return member list of string value in list"
  (member val lst :test #'string=))

; make "str" into label that can be tested for in case
(defun case-str (str)
  (intern (string-upcase str)))

;; some bit-tools

; see aoc-2020-14 for sample use
(defun dpb-pos (val pos &optional (bt 1))
  "update bit-position pos in value with given bit (default 1)"
  (dpb bt (byte 1 pos) val))


;; various loops

; enumerate a list, remove nil (but still count for enumeration)

(defun enumerate-clean (lst &optional (start 0))
  (loop for i in lst
       for j = start then (1+ j) 
      when i collect (cons j i)))

; convert binary list to decimal

(defun bin-to-dec (lst)
  (loop for xp = 1 then (* 2 xp) for elt in (reverse lst) summing (* elt xp)))

; max/min length of string in list of strings - using reduce trick with initial value

(defun maxlen (strs)
  (reduce #'(lambda (l str) (max l (length str))) strs :initial-value 0))

(defun minlen (strs)
  (reduce #'(lambda (l str) (min l (length str))) strs :initial-value (length (car strs))))




; example
; (ecase (case-str key) ; key is "FOO" or "BAR"
;   (FOO (do-something))
;   (BAR (do-something-else)))

;; compass directions, complex numbers
;;

(defun compass ()  
  (list (cons "N" (complex 0 1)) 
        (cons "S" (complex 0 -1))
        (cons "E" (complex 1 0))
        (cons "W" (complex -1 0))) 

(defun movept (pt dir len)
  (let* ((dirs (compass))) 
    (+ pt (* (alist-val dirs dir) len))))

; manhattan (L1) norm of complex number
(defun manhattan (pos)
  (+ (abs (realpart pos)) (abs (imagpart pos))))

;; hash tools
;; 

; count str in hash values

(defun count-occup (h str)
  (loop for val being the hash-value of h
        count (string= val h)))

; unique by hash keys

(defun uniq-parts (p1 p2)
  "use hash to find union of unique strings in to lists of strings"
  (let ((phash (make-hash-table :test #'equalp)))
    (dolist (p (append p1 p2))
      (setf (gethash p phash) t))
    (loop for key being the hash-key of phash collect key)))



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
 
