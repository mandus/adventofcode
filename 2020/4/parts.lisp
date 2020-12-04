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
(defparameter *inp* "input.txt")


(defun passport (p line)
  "create/update passport as an alist"
  (when *debug*
    (format t "add line '~a' to [~a]~%" line p))
  (cond 
    ((= 0 (length line)) p)
    (t 
     ; suggestion from phil_g@reddit - I like it better since it saves one acons.
     (reduce (lambda (upd-p pair) (acons (car pair) (cadr pair) upd-p))
               (mapcar (lambda (x) (cl-ppcre:split ":" x)) (cl-ppcre:split " " line))
               :initial-value p) 

     ; (loop for (key value) in (mapcar (lambda (x) (cl-ppcre:split ":" x)) (cl-ppcre:split " " line))
     ;         for upd-p = (acons key value p) then (acons key value upd-p)
     ;         finally (return upd-p))
     )))

(defun get-field (p f)
  (cdr (assoc f p :test #'string=)))

; 8 fields, or 7 if "cid" is missing
(defun p-len-p (p)
  (let ((valid-len 8)
        (valid-len-sans-cid 7)) 
    (or (= valid-len (length p)) 
      (and (= valid-len-sans-cid (length p)) (not (get-field p "cid"))))))

(defun field-int-limits (val low up)
   (let ((ival (if (and val (every #'digit-char-p val)) (parse-integer val) nil)))
     (<= low ival up)))

; byr (Birth Year) - four digits; at least 1920 and at most 2002.
(defun p-byr-p (p)
  (field-int-limits (get-field p "byr") 1920 2002))
    
; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
(defun p-iyr-p (p)
  (field-int-limits (get-field p "iyr") 2010 2020))

; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
(defun p-eyr-p (p)
  (field-int-limits (get-field p "eyr") 2020 2030))

; hgt (Height) - a number followed by either cm or in:
; If cm, the number must be at least 150 and at most 193.
; If in, the number must be at least 59 and at most 76.
(defun check-hgt (hgt unit low up)
  (let* ((hlst (cl-ppcre:split unit hgt))
         (h (car hlst))
         (ih (if (and h (every #'digit-char-p h)) (parse-integer h) nil)))
    (and (= 1 (length hlst))
         ; possible simplification - reuse the field-int-limits func; i.e (field-int-limits h low up)
         ih
         (<= low ih)
         (>= up ih))))

(defun p-hgt-p (p)
  (let* ((hgt (get-field p "hgt"))
         (iscm (search "cm" hgt))
         (isin (search "in" hgt)))
    (if hgt
        (cond
          (iscm (check-hgt hgt "cm" 150 193))
          (isin (check-hgt hgt "in" 59 76))
          (t nil))
        nil)))

; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
(defun p-hcl-p (p)
  (cl-ppcre:all-matches "^#[0-9a-f]{6}$" (get-field p "hcl")))

; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
(defun p-ecl-p (p)
  (let ((ecl (get-field p "ecl")))
    (some (lambda (x) (string= x ecl)) (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))

; pid (Passport ID) - a nine-digit number, including leading zeroes.
(defun p-pid-p (p)
  (cl-ppcre:all-matches "^[0-9]{9}$" (get-field p "pid")))

; cid (Country ID) - ignored, missing or not.
;

(defun passport-p (p)
  "test if passport is valid"
  (and 
    (p-len-p p)
    (p-byr-p p)
    (p-iyr-p p)
    (p-eyr-p p)
    (p-hgt-p p)
    (p-hcl-p p)
    (p-ecl-p p)
    (p-pid-p p)
    ))
 
(defun data-to-passports (data)
  (loop for line in data
        for p = (passport nil line) then (passport p line)
        for passports = nil then passports
        if (= 0 (length line))
        do (progn
             (setf passports (cons p passports))
             (setf p nil))
        finally (return (cons p passports))
        ))

(defun noop (x) x)
(defun read-input (fn &optional (trans 'noop))
  (with-open-file (f fn)
    (loop for line = (read-line f nil)
          while line collect (funcall trans line))))


;; drivers
;;
(defun part1 (fn)
  (let* ((data (read-input fn))
         (passports (data-to-passports data))
         )
    (format t "Part 1~%")
    (format t "Valid passports: ~a~%" (count-if #'p-len-p passports))


    (when *debug*
      (format t "This~%")
      (format t "num passports ~a~%" (length passports))
      ) 
    ))

(defun part2 (fn)
  (let* ((data (read-input fn))
         (passports (data-to-passports data))
         )
    (format t "Part 2~%")
    (format t "Valid passports: ~a~%" (count-if #'passport-p passports))

    (when *debug*
      (format t "data ~a~%" data)
      )
    ))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
