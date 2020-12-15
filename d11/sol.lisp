(defpackage :d11
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defconstant +EMPTY+ #\L)
(defconstant +OCCUPIED+ #\#)
(defconstant +FLOOR+ #\.)

(defun copy-ferry (ferry)
  (mapcar #'copy-seq ferry))

(defun valid-spot-p (xy ferry)
    (let ((x (first xy))
          (y (first (last xy))))
      (and (< -1 x (length (first ferry)))
           (< -1 y (length ferry)))))

(defun neighbors (xy ferry)
  (let ((x (first xy))
        (y (first (last xy))))
    (mapcar #'(lambda (xy) (get-spot xy ferry))
            (remove-if-not #'(lambda (xy) (valid-spot-p xy ferry))
                           (list (list (1- x) (1- y))
                                 (list x      (1- y))
                                 (list (1+ x) (1- y))
                                 (list (1- x) y)
                                 (list (1+ x) y)
                                 (list (1- x) (1+ y))
                                 (list x      (1+ y))
                                 (list(1+ x) (1+ y)))))))

(defun neighbor-seat (xy dir ferry)
  (let ((new-xy (mapcar #'+ dir xy)))
    (if (valid-spot-p new-xy ferry)
        (if (seatp (get-spot new-xy ferry))
            new-xy
            (neighbor-seat new-xy dir ferry))
        new-xy)))

(defun neighbor-seats (xy ferry)
  (let ((dirs '((-1 -1)
                ( 0 -1)
                ( 1 -1)
                (-1  0)
                ( 1  0)
                (-1  1)
                ( 0  1)
                ( 1  1))))
    (mapcar #'(lambda (xy) (get-spot xy ferry))
            (remove-if-not #'(lambda (xy) (valid-spot-p xy ferry))
                           (mapcar #'(lambda (dir) (neighbor-seat xy dir ferry)) dirs)))))
  )

(defun get-spot (xy ferry)
  (char (nth (first (last xy)) ferry) (first xy)))

(defun seatp (spot)
  (or (emptyp spot)
      (occupiedp spot)))
(defun emptyp (spot)
  (char= +EMPTY+ spot))
(defun occupiedp (spot)
  (char= +OCCUPIED+ spot))

(defun count-occupied (ferry)
  (reduce #'+ (mapcar #'(lambda (row) (count-if #'occupiedp row)) ferry)))

(defun next-spot (xy ferry neighbors-fn max-occupied)
  (let ((spot (get-spot xy ferry)))
    (cond ((and
            (emptyp spot)
            (notany #'occupiedp (funcall neighbors-fn xy ferry)))
           +OCCUPIED+)
          ((and
            (occupiedp spot)
            (>= (count-if #'occupiedp (funcall neighbors-fn xy ferry)) max-occupied))
           +EMPTY+)
          (t
           spot))))

(defun set-spot (xy spot ferry)
  (let ((x (first xy))
        (y (first (last xy))))
    (setf (char (nth y ferry) x) spot)))

(defun next-ferry (ferry neighbors max-occupied)
  (let ((new-ferry (copy-ferry ferry))
        (changed nil))
    (loop for y from 0 below (length ferry) do
                             (loop for x from 0 below (length (first ferry))
                                   do (let* ((xy (list x y))
                                             (oldspot (get-spot xy ferry))
                                            (newspot (next-spot xy ferry neighbors max-occupied)))
                                        (when (char/= oldspot newspot)
                                            (setf changed t)
                                            (set-spot xy newspot new-ferry)))))
    (if changed new-ferry nil)))

(defun solve ()
  (let ((ferry *lines*))
    (loop do
      (let ((new-ferry (next-ferry ferry #'neighbors 4)))
        (if new-ferry
            (setf ferry new-ferry)
            (return (count-occupied ferry)))))))

(defun solve2 ()
  (let ((ferry *lines*))
    (loop do
      (let ((new-ferry (next-ferry ferry #'neighbor-seats 5)))
        (if new-ferry
            (setf ferry new-ferry)
            (return (count-occupied ferry)))))))
