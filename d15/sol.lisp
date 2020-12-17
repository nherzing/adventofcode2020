(defpackage :d14
  (:use :common-lisp :common))

(defparameter *input* '(0 5 4 1 10 14 7))
(defparameter *input* '(0 3 6))

(defclass tape ()
  ((end :initform nil :accessor end)
   (idx :initform 0 :accessor idx)
   (mem :initform (make-hash-table) :reader mem)))

(defun record (n tape)
  (let* ((mem (mem tape))
         (n-mem (gethash n mem '()))
         (idx (idx tape)))
    (if n-mem
        (setf (gethash n mem) (cons idx (list (first n-mem))))
        (setf (gethash n mem) (list idx)))
    (incf (idx tape))
    (setf (end tape) n)))

(defun solve ()
  (let ((tape (make-instance 'tape)))
    (dolist (n *input*)
      (record n tape))
    (do ()
        ((= 30000000 (idx tape)) (end tape))
      (let* ((n (end tape))
             (last-n-idx (gethash n (mem tape))))
        ;(format t "~a: ~a - ~a~%" n (idx tape) last-n-idx)
        (if (> (length last-n-idx) 1)
            (record (- (first last-n-idx) (second last-n-idx)) tape)
            (record 0 tape))))))
