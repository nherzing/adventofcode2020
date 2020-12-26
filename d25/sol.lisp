(defpackage :d24
  (:use :common-lisp :common))

(defparameter *public-keys* '(12232269 19452773))
(defparameter *public-keys* '(5764801 17807724))

(defun get-loop-size (public-key)
  (do ((loop-size 0 (1+ loop-size))
       (sub-num 1 (mod (* sub-num 7) 20201227)))
      ((= sub-num public-key) loop-size)))

(defun transform (loop-size subject-number)
  (let ((result 1))
    (dotimes (i loop-size)
      (setf result (mod (* result subject-number) 20201227)))
    result))

(defun get-encryption-key (public-keys)
  (transform (get-loop-size (first public-keys))
             (second public-keys)))

(defun solve ()
  )
