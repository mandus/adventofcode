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

(defun char-bit (chr)
  (let ((chrmap (pairlis 
                  '("." "#") 
                  '(#\0 #\1)))) 
    (cdr (assoc chr chrmap :test #'string=))))

(defun maptile (tile &optional mapped)
  (let* ((line (car tile))
         (nxt (cdr tile))
         (lid (map 'string #'char-bit line))
         (nxtmapped (cons lid mapped)))
    ; (when *debug*
    ;   (format t "line: ~a -> ~a [~a]~%" line lid nxtmapped))
    (if nxt (maptile nxt nxtmapped)
      nxtmapped)))

(defun parse (data)
  (labels ((inner (data tiles)
             (let* ((tile (car data))
                    (nxt (cdr data))
                    (tidstr (car tile))
                    (tid (parse-integer (subseq tidstr 5) :junk-allowed t))) ; junk-allowed just ignore the trailing ':'
               (setf (gethash tid tiles) (maptile (cdr tile)))
               (if nxt (inner nxt tiles) tiles))))
    (inner data (make-hash-table :test #'equalp))))

(car (last (cons '(1 2) (cons '(4 5) (cons '(2 8) nil)))))

(defun front (tile)
  (map 'string #'(lambda (l) (char l 0)) tile))

(defun back (tile)
  (map 'string #'(lambda (l) (char l (1- (length l)))) tile))

(defun mapsides (tile)
  (let* ((sides (list (car tile) (front tile) (car (last tile)) (back tile)))
         (sids (mapcar #'(lambda (str) (parse-integer str :radix 2)) sides))
         (revsids (mapcar #'(lambda (str) (parse-integer (reverse str) :radix 2)) sides))
        )
    (append sids revsids)))

(defun addsides (hash tid sids)
  (let ((sid (car sids))
        (nxt (cdr sids)))
    (setf (gethash sid hash) (cons tid (gethash sid hash)))
    (when nxt (addsides hash tid nxt))))

(defun sidetiles (tiles)
  (let* ((sidemap (make-hash-table :test #'equalp)))
    (loop for tid being the hash-key using (hash-value tile) of tiles
          do (addsides sidemap tid (mapsides tile)))
    sidemap))


(defun imgborder (sidemap)
  (loop for side being the hash-key using (hash-value tids) of sidemap 
        when (oddp (length tids)) 
        append tids))

(defun countborder (lst)
  ; every tile will appear in pairs since we look at both side-id and the reverse-side-id
  ; so divide every number with two
  (let ((bordermap (make-hash-table)))
    (loop for el in lst 
          do (incf (gethash el bordermap 0)))
    (maphash (lambda (key val) (setf (gethash key bordermap) (/ val 2))) bordermap)
    bordermap))

(defun corners (bordermap)
  (loop for tid being the hash-key 
        using (hash-value cnt) of bordermap
        when (= 2 cnt)
        collect tid))

;; drivers
;;
(defun part1 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         (tiles (parse data))
         (sidemap (sidetiles tiles))
         (borderlst (imgborder sidemap))
         (bordercnt (countborder borderlst))
         (corners (corners bordercnt))
         )

    (format t "Part 1~%")
    (format t "Answer: ~a~%" (reduce #'* (corners bordercnt)))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "tiles: ~a~%" (loop for tid being the hash-key of tiles collect tid))
      (format t "side-tiles: ~a~%" borderlst)
      (maphash (lambda (tid cnt) (format t "~a: ~a~%" tid cnt)) bordercnt)
      (format t "corners: ~a~%" corners)
      )))

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
