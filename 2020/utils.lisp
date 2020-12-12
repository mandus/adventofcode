; Some maybe useful AoC tools.


;; reading files; parsing

(defun read-input (fn &optional (trans #'identity))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))

(defun parse (l)
  (cons (subseq l 0 1) (parse-integer (subseq l 1))))

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

;; count str in hash values

(defun count-occup (h str)
  (loop for val being the hash-value of h
        count (string= val h)))


