(defpackage :d6
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun count-group (group)
  (length (reduce #'union (mapcar #'(lambda (person) (coerce person 'list)) group))))

(defun solve ()
  (let* ((groups (split-at "" *lines* :test #'string=)))
    (apply #'+ (mapcar #'count-group groups))))

(solve)

(defun count-group2 (group)
  (length (reduce #'intersection (mapcar #'(lambda (person) (coerce person 'list)) group))))

(defun solve2 ()
  (let* ((groups (split-at "" *lines* :test #'string=)))
    (apply #'+ (mapcar #'count-group2 groups))))

(solve2)
