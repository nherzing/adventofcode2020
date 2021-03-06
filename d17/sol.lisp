(defpackage :d14
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun neighbors (coord)
  (let ((x (first coord))
        (y (second coord))
        (z (third coord))
        (w (fourth coord)))
    (list
     (list     x      y  (1- z) (1- w))
     (list     x  (1- y) (1- z) (1- w))
     (list     x  (1+ y) (1- z) (1- w))
     (list (1- x) (1- y) (1- z) (1- w))
     (list (1- x)     y  (1- z) (1- w))
     (list (1- x) (1+ y) (1- z) (1- w))
     (list (1+ x) (1- y) (1- z) (1- w))
     (list (1+ x)     y  (1- z) (1- w))
     (list (1+ x) (1+ y) (1- z) (1- w))

     (list     x      y  (1- z) (1+ w))
     (list     x  (1- y) (1- z) (1+ w))
     (list     x  (1+ y) (1- z) (1+ w))
     (list (1- x) (1- y) (1- z) (1+ w))
     (list (1- x)     y  (1- z) (1+ w))
     (list (1- x) (1+ y) (1- z) (1+ w))
     (list (1+ x) (1- y) (1- z) (1+ w))
     (list (1+ x)     y  (1- z) (1+ w))
     (list (1+ x) (1+ y) (1- z) (1+ w))

     (list     x      y  (1- z)     w)
     (list     x  (1- y) (1- z)     w)
     (list     x  (1+ y) (1- z)     w)
     (list (1- x) (1- y) (1- z)     w)
     (list (1- x)     y  (1- z)     w)
     (list (1- x) (1+ y) (1- z)     w)
     (list (1+ x) (1- y) (1- z)     w)
     (list (1+ x)     y  (1- z)     w)
     (list (1+ x) (1+ y) (1- z)     w)

     (list     x      y  (1+ z) (1- w))
     (list     x  (1- y) (1+ z) (1- w))
     (list     x  (1+ y) (1+ z) (1- w))
     (list (1- x) (1- y) (1+ z) (1- w))
     (list (1- x)     y  (1+ z) (1- w))
     (list (1- x) (1+ y) (1+ z) (1- w))
     (list (1+ x) (1- y) (1+ z) (1- w))
     (list (1+ x)     y  (1+ z) (1- w))
     (list (1+ x) (1+ y) (1+ z) (1- w))

     (list     x      y  (1+ z) (1+ w))
     (list     x  (1- y) (1+ z) (1+ w))
     (list     x  (1+ y) (1+ z) (1+ w))
     (list (1- x) (1- y) (1+ z) (1+ w))
     (list (1- x)     y  (1+ z) (1+ w))
     (list (1- x) (1+ y) (1+ z) (1+ w))
     (list (1+ x) (1- y) (1+ z) (1+ w))
     (list (1+ x)     y  (1+ z) (1+ w))
     (list (1+ x) (1+ y) (1+ z) (1+ w))

     (list     x      y  (1+ z)     w)
     (list     x  (1- y) (1+ z)     w)
     (list     x  (1+ y) (1+ z)     w)
     (list (1- x) (1- y) (1+ z)     w)
     (list (1- x)     y  (1+ z)     w)
     (list (1- x) (1+ y) (1+ z)     w)
     (list (1+ x) (1- y) (1+ z)     w)
     (list (1+ x)     y  (1+ z)     w)
     (list (1+ x) (1+ y) (1+ z)     w)

     (list     x  (1- y)  z     (1- w))
     (list     x  (1+ y)  z     (1- w))
     (list (1- x) (1- y)  z     (1- w))
     (list (1- x)     y   z     (1- w))
     (list (1- x) (1+ y)  z     (1- w))
     (list (1+ x) (1- y)  z     (1- w))
     (list (1+ x)     y   z     (1- w))
     (list (1+ x) (1+ y)  z     (1- w))

     (list     x  (1- y)  z     (1+ w))
     (list     x  (1+ y)  z     (1+ w))
     (list (1- x) (1- y)  z     (1+ w))
     (list (1- x)     y   z     (1+ w))
     (list (1- x) (1+ y)  z     (1+ w))
     (list (1+ x) (1- y)  z     (1+ w))
     (list (1+ x)     y   z     (1+ w))
     (list (1+ x) (1+ y)  z     (1+ w))


     (list     x  (1- y)  z         w)
     (list     x  (1+ y)  z         w)
     (list (1- x) (1- y)  z         w)
     (list (1- x)     y   z         w)
     (list (1- x) (1+ y)  z         w)
     (list (1+ x) (1- y)  z         w)
     (list (1+ x)     y   z         w)
     (list (1+ x) (1+ y)  z         w)

     (list     x      y   z     (1+ w))
     (list     x      y   z     (1- w)))))

(defun activep (val)
  (and val
       (char= val #\#)))

(defun cycle-coord (grid coord)
  (let* ((val (gethash coord grid))
        (neighbor-vals (mapcar #'(lambda (n-coord) (gethash n-coord grid)) (neighbors coord)))
        (active-count (count-if #'activep neighbor-vals)))
    (if (activep val)
        (if (<= 2 active-count 3)
            #\#
            #\.)
        (if (= 3 active-count)
            #\#
            #\.))))

(defun cycle (grid)
  (let ((new-grid (make-hash-table :test 'equal))
        (coords-to-test (make-hash-table :test 'equal)))
    (loop for key being the hash-keys of grid
          do (setf (gethash key coords-to-test) t)
             (dolist (n-coord (neighbors key))
               (setf (gethash n-coord coords-to-test) t)))
    (loop for coord being the hash-keys of coords-to-test
          do (setf (gethash coord new-grid) (cycle-coord grid coord)))
    new-grid))

(defun solve ()
  (let ((grid (make-hash-table :test 'equal)))
    (loop for y from 0 upto (1- (length *lines*))
                            do (loop for x from 0 upto (1- (length (first *lines*)))
                                     do (setf (gethash (list x y 0 0) grid) (char (nth y *lines*) x))))
    (dotimes (i 6)
      (setf grid (cycle grid)))
    (loop for value being the hash-values of grid
          when (activep value) count 1)))
