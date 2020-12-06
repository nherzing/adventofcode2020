(defpackage :d5
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun get-row (row-part)
  (do ((step 64 (/ step 2))
       (min 0 (if (char= c #\F) min (+ min step)))
       (max 127 (if (char= c #\B) max (- max step)))
       (c (char row-part 0) (and (> (length l) 0) (char l 0)))
       (l (subseq row-part 1) (and (> (length l) 0) (subseq l 1))))
      ((< step 1) max)))

(defun get-col (col-part)
  (do ((step 4 (/ step 2))
       (min 0 (if (char= c #\L) min (+ min step)))
       (max 7 (if (char= c #\R) max (- max step)))
       (c (char col-part 0) (and (> (length l) 0) (char l 0)))
       (l (subseq col-part 1) (and (> (length l) 0)(subseq l 1))))
      ((< step 1) max)))

(defun line-to-seat (line)
  (let ((row-part (subseq line 0 7))
         (col-part (subseq line 7 10)))
    (list
     (get-row row-part)
     (get-col col-part))))

(defun seat-to-id (seat)
  (+ (* (first seat) 8) (second seat)))

(defun solve1 ()
  (let* ((seats (mapcar #'line-to-seat *lines*))
         (ids (mapcar #'seat-to-id seats)))
    (apply #'max ids)))

(defun solve2 ()
  (let* ((seats (mapcar #'line-to-seat *lines*))
         (ids (sort (mapcar #'seat-to-id seats) #'<)))
    (loop for (id . rest) on ids
          when (/= id (1- (car rest))) return (1+ id))))
