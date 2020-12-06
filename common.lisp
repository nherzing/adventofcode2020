(defpackage :common
  (:use :common-lisp)
  (:export read-lines split-at))

(defun read-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-at (item seq &key (test #'eql))
  (let ((pos (position item seq :test test)))
    (if pos
        (cons (subseq seq 0 pos)
              (split-at item (subseq seq (1+ pos)) :test test))
        (list seq))))

(defun join (strings sep)
  (reduce #'(lambda (s acc) (concatenate 'string s sep acc)) (rest strings) :initial-value (first strings)))