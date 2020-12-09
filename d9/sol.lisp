(defpackage :d8
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))
(defparameter *preamble* 25)

(defun validp (sum num-lookup)
  (loop for n1 being the hash-keys of num-lookup
        when (and
              (gethash (- sum n1) num-lookup)
              (/= sum (* 2 n1)))
          return t))

(defun upto (min max)
  (loop for i from min to (1- max) collecting i))


(defun solve1 ()
  (let ((nums (mapcar #'parse-integer *lines*))
        (num-lookup (make-hash-table)))
    (dolist (n (subseq nums 0 *preamble*))
      (setf (gethash n num-lookup) t))
    (dolist (i (upto *preamble* (length nums)))
      (let ((n (nth i nums)))
        (unless (validp n num-lookup) (return n))
        (remhash (nth (- i *preamble*) nums) num-lookup)
        (setf (gethash n num-lookup) t)))))

(solve1)

(defun solve2 ()
  (let* ((nums (mapcar #'parse-integer *lines*))
         (goal (solve1))
         (sum 0)
         (sublist
           (dolist (n (upto 0 (length nums)))
             (let ((end
                     (do ((end n (1+ end)))
                         ((>= sum goal) end)
                       (setf sum (+ sum (nth end nums))))))
               (if (= sum goal) (return (subseq nums n end)))
               (setf sum 0))
             )))
    (+ (reduce #'min sublist) (reduce #'max sublist))))

(solve2)
