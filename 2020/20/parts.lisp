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

(defun front (tile)
  (map 'string #'(lambda (l) (char l 0)) tile))

(defun back (tile)
  (map 'string #'(lambda (l) (char l (1- (length l)))) tile))

(defun tilefacelist (tile)
  (let* ((faces (list (car tile) (back tile)(car (last tile)) (front tile)))
         (ids (mapcar #'(lambda (str) (parse-integer str :radix 2)) faces))
         (rids (mapcar #'(lambda (str) (parse-integer (reverse str) :radix 2)) faces))
         (mids (list (nth 0 rids) (nth 1 rids) (nth 2 ids) (nth 3 ids)))
         (revids (list (nth 0 ids) (nth 3 rids) (nth 2 rids) (nth 1 ids))))
    (list mids revids)))

(defun mapfaces (tile)
  (let ((facelist (tilefacelist tile)))
    (append (first facelist) (second facelist))))

(defun addfaces (hash tid sids)
  (let ((sid (car sids))
        (nxt (cdr sids)))
    (setf (gethash sid hash) (cons tid (gethash sid hash)))
    (when nxt (addfaces hash tid nxt))))

(defun facetiles (tiles)
  (let ((facemap (make-hash-table :test #'equalp)))
    (loop for tid being the hash-key using (hash-value tile) of tiles
          do (addfaces facemap tid (mapfaces tile)))
    facemap))

(defun tilefaces (tiles)
  (let ((tilemap (make-hash-table :test #'equalp)))
    (maphash (lambda (tid tile) (setf (gethash tid tilemap) (mapfaces tile))) tiles)
    tilemap))


(defun imgborder (facemap)
  ; the tile-ids where face-ids only belonging to one tile
  (loop for tids being the hash-value of facemap 
        when (oddp (length tids)) 
        append tids))

(defun intfaces (facemap)
  ; the face-ids belonging to two tiles
  (loop for faceid being the hash-key using (hash-value tids) of facemap
        when (evenp (length tids))
        collect faceid))

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

; strategy
; 0. row = 0, col = 0, width = nil
; 1. start with first corner; assign as tile (row,col), pop corner from cornerlist
; 2. find first faceid in main facelist of tile matching face of another tile; or if none the last in the reverse facelist - this will be the right face (after flip/rotate)
;    - first/last in the context of wrap-around; if 0,1 is interior, then 0 is first - but if it is 3,0 then 3 is first. Similar with the reverse.
; 3. calculate flip/rotate of tile  - store in fliprot hash with key (row, col) and value (flipp, n) (i.e (nil/t and an int).
;    (have to decide on how we rotate; always clockwise or if we rotate the other way on flip, depending on what's easiest...)
; 4. increment col; Find matching tile, and assign as (row, col).
;    if col = 0 and tile is corner, pop corner from cornerlist
; 5. calculate flip/rotate of match
; 6. if width is set, and (= col (1- width)), col=-1, row=row+1
;    else if tile is corner, set width=col+1, col=-1, row=row+1, pop corner from cornerlist
; 7. if more corners, continue from 4
;    else done
;
; strategy find matching tile
; 1. if col > 0, look up right face of tile (row, (1- col)), using the stored flip/rotate-info
;    (flip is nil; idx = 1 - rotate (mod 3); flip is t; idx = 3 + rotate (mod 3)
;    When we have the faceid, we can use facemap to look up both tiles. 
; 2. if col = 0, look up the bottom face of the tile ((1- row), col), using the stored flip/rotate-info
;    (same as in 1, but with idx base 2 for main direction and idx base 2 for the reversed)
;    Again, use facemap to look up both tiles and we know the facing one. 

(position 123 `(111 112 123 321))
(position (car (reverse (intersection '(010 111 113 321 341) '(111 112 321 312)))) '(111 112 321 312))

(defun organize (tiles corners 
                       &optional 
                       (org (make-hash-table :test #'equalp)) 
                       (fliprot (make-hash-table :test #'equalp)) 
                       (row 0) (col 0) (width nil))
  (if not corners (values org fliprot)
      ; still have corners, so keep going.
      (cond
        (= 0 row col) (progn
                        (setf (gethash `(,row ,col) org) (car corners))
                        (setf (gethash `(,row ,col) fliprot) (.. calc flip/rot for the tile ..))
                        (organize tiles (cdr corners) org fliprot row (1+ col))
                        )
        )
       
    ; map (row, col) -> tile 
      )
  )

;; drivers
;;
(defun part1 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         (tiles (parse data))
         (facemap (facetiles tiles))
         (borderlst (imgborder facemap))
         (bordercnt (countborder borderlst))
         (corners (corners bordercnt))
         )

    (format t "Part 1~%")
    (format t "Answer: ~a~%" (reduce #'* (corners bordercnt)))

    (when *debug*
      (format t "data: ~a~%" data)
      (format t "tiles: ~a~%" (loop for tid being the hash-key of tiles collect tid))
      (format t "border-tiles: ~a~%" borderlst)
      (maphash (lambda (tid cnt) (format t "~a: ~a~%" tid cnt)) bordercnt)
      (format t "corners: ~a~%" corners)
      )))

(defun part2 (fn &optional inpdata)
  (let* ((data (or inpdata (read-by-para fn "" #'split-lines)))
         (tiles (parse data))
         (facemap (facetiles tiles))
         (iface (intfaces facemap))
         (tilemap (tilefaces tiles))
         (borderlst (imgborder facemap))
         (bordercnt (countborder borderlst))
         (corners (corners bordercnt))
         )

    (format t "Part 2~%")
    (when *debug*
      (format t "data ~a~%" data)
      (loop for sid being the hash-key using (hash-value tids) of facemap do (format t "face ~a: ~a~%" sid tids))
      (maphash (lambda (tid sids) (format t "~a: ~a~%" tid sids)) tilemap)
      (format t "interior faces: ~a~%" iface)
      (format t "corners: ~a~%" corners)
      )))

(defun run ()
  (part1 *inp*)
  (part2 *inp*))
