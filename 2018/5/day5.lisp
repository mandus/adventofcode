; Advent of code 2018 - Åsmund Ødegård

(defvar *lowercase*
  (map 'string #'code-char (loop
                             for c from (char-code #\a) to (char-code #\z)
                             collect c)))

(defun process-internal (l)
  (let ((flag 'nil))

    (labels ((gidx (idx)
                   (if (< idx (length l))
                     (string (aref l idx)))))

      (loop for idx from 0 below (length l) 
            when
            (let* ((c (gidx idx))
                   (n (gidx (1+ idx)))
                   (lc (string-downcase c))
                   (ln (string-downcase n)))
              (if flag
                (psetf flag 'nil) ; skip this entry, but unset flag for next
                (if (and (string= lc ln) 
                              (not (string= c n)))
                  (psetf flag 't)
                  c
                  ))) collect it))))

(defun process-to-string (l)
  (format nil "~{~A~}" (process-internal l)))

(defun process-line (l)
  (let* ((len (length l))
        (lp (process-to-string l))
        (lplen (length lp)))

    (if (>= lplen len)
      lp
      (process-line lp))))

(defun process (l maxl)
  (let ((cm maxl)
        (cl l))
    (loop for c across *lowercase* 
          do
          (let* ((lc (remove (char-upcase c) (remove c l)))
                 (lpc (process-line lc))
                 (lpclen (length lpc)))

            (if (< lpclen cm)
              (progn
                (setf cl lpc)
                (setf cm lpclen)))))
    cl))

(defun main() 
    (let* ((l (read-line))
           (maxl (length l))
           (lp (process l maxl))
           (len (length lp))
           (flag 'nil))

      (if flag 
        (progn
          (format t "Line: ~A.~%" l)))

      (format t "Line: ~A~%" lp)
      (format t "Length: ~A~%" len)))

(main)


;;
;; From subredit adventofcode
;;
;(defun destroy? (a b)
;  (and a b (char/= a b) (char-equal a b)))
;
;(defun reduce-polymer (polymer)
;  (let ((stack (list (elt polymer 0))))
;    (loop :for u :across (subseq polymer 1)
;          :do
;             ;; note - stack will be in reveresed order..
;             (push u stack)
;             (if (destroy? (car stack) (cadr stack))
;                 (progn
;                   (pop stack)
;                   (pop stack))))
;    stack))
;
;(length (reduce-polymer *polymer*)) ;; => 11476
;
;(defun reduce-polymer2 (polymer1 unit-to-skip)
;
;  ;; Note: char-equal is case-insensitive; #\a is char-equal with #\A
;  (let ((polymer (remove-if (lambda (c) (char-equal c unit-to-skip)) polymer1)))
;    (reduce-polymer polymer)))
;
;(loop :for i :from (char-code #\a) :to (char-code #\z)
;      :minimizing (length (reduce-polymer2 *polymer* (code-char i)))) 


