; Advent of Code 2018 - based on ideas from the world

;; 
;; run: sbcl --noinform --load <file.lisp> --quit
;; compile and run: sbcl --noinform --eval "(compile-file \"<file.lisp>\")" --quit
;;                  sbcl --load <file.fasl> --quit

;; load some deps we may need?

(unless (find-package :unix-opts)
  (ql:quickload "unix-opts"))
(unless (find-package :cl-ppcre)
  (ql:quickload "cl-ppcre"))
(unless (find-package :iterate)
  (ql:quickload "iterate"))

;; set up a package - incase we want to reuse stuff (e.g)
(defpackage :aoc-2018-07
  (:use :common-lisp))

(in-package :aoc-2018-07)
(use-package :iterate)

(defvar *debug* 'nil)

(defun parse-line (line)
  (multiple-value-bind (_ tasks)
      (ppcre:scan-to-strings "Step ([A-Z]) must .+ step ([A-Z]) can begin." line)
    ;; map the array from ppcre scan to list
    (map 'list #'identity tasks)))

(defun read-input (fn)
  (iter (for line in-file fn using #'read-line)
        (collect (parse-line line))))

(defun dep-table (edges)
  (let ((deps (make-hash-table :test 'equal)))
    ;; provide nil as default for all
    (iter (for (val key) in edges)
          (setf (gethash key deps) nil)
          (setf (gethash val deps) nil))
    (iter (for (val key) in edges)
          (unless (member val (gethash key deps) :test 'equal)
            (push val (gethash key deps))))
    deps))

(defun topsort (edges)
  (let ((deps (dep-table edges)))
    (iter (until (= 0 (hash-table-count deps)))
          (let* ((dolist (iter (for (k v) in-hashtable deps)
                               (when (null v) (collect k))))
                 (dosorted (sort dolist #'string<))
                 (t0 (car dosorted)))
            (collect t0)
            (remhash t0 deps)
            (iter (for (k v) in-hashtable deps)
                  (when (member t0 v :test #'string=)
                    (setf (gethash k deps) (remove t0 v :test #'string=))))))))

(defun main(fn)
    (let* ((data (read-input fn))
           (deps (dep-table data)))
      (format t "data: ~A~%" data)
      (iter (for (key val) in-hashtable deps)
            (format t "~A depends on ~A~%" key val))
      (format t "Day7 part1: ~{~a~}~%" (topsort data))))

;; ** startup **
(opts:define-opts
  (:name :help
   :description "print help"
   :short #\h
   :long "help")
  (:name :debug
   :description "enable debug"
   :short #\d
   :long "debug"))

(defmacro when-option  ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun init()
    (let ((fn "input.txt"))
      (multiple-value-bind (options free-args)
        (opts:get-opts)

        (when-option (options :help)
                     (progn
                       (opts:describe
                       :prefix "AoC Day7"
                       :usage-of "day7.lisp"
                       :args  "[input-file]")
                       (opts:exit 0)))
        (when-option (options :debug)
                     (setf *debug* 't))
        (if free-args
          (setf fn (car free-args)))
            (if (probe-file fn)
              (main fn)
              (when *debug*
                (format t "File ~A does not exists~%" fn))))))

(init)
