(defpackage :d8
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))


(defun solve ()
  (let ((nums (mapcar #'parse-integer *lines*)))
    (push (+ 3 (apply #'max nums)) nums)
    (push 0 nums)
    (setf nums (sort nums #'<))
    (loop for cons on nums
          with ones = 0
          with threes = 0
          when (= 1 (length cons)) return (* ones threes)
          when (= (1+ (car cons)) (cadr cons)) do (incf ones)
          when (= (+ 3 (car cons)) (cadr cons)) do (incf threes))))

(solve)

(defun stepp (a b)
  (and
   b
   (<= 1 (- b a) 3)))

(defun count-solutions (nums memo)
;  (format t "COUNT ~a~%" nums)
;  (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) memo)
  (if (gethash (first nums) memo)
      (progn
;        (format t "GOT EMPTY ~a~%" (first nums))
        (gethash (first nums) memo))
      (let ((start (first nums))
            (ans 0))
        (if (stepp start (second nums))
            (setf ans (+ ans (count-solutions (cdr nums) memo))))
        (if (stepp start (third nums))
            (setf ans (+ ans (count-solutions (cddr nums) memo))))
        (if (stepp start (fourth nums))
            (setf ans (+ ans (count-solutions (cdddr nums) memo))))
        (setf (gethash (first nums) memo) ans)
;        (format t "FILL IN ~a: ~a~%" (first nums) ans)
        ans)))

(defun solve2 ()
  (let ((nums (mapcar #'parse-integer *lines*))
        (memo (make-hash-table)))
    (push (+ 3 (apply #'max nums)) nums)
    (push 0 nums)
    (setf nums (sort nums #'<))
    (setf (gethash (first (last nums)) memo) 1)
    (count-solutions nums memo)
    (gethash (first nums) memo)))

(solve2)
