(defpackage :d23
  (:use :common-lisp :common))

(defparameter *input* "315679824")
;;(defparameter *input* "389125467")

(defparameter *circle* nil)
(defparameter *current* nil)
(defparameter *max-cup* nil)
(defparameter *lookup* nil)

(defun negativep (n) (< n 0))

(defun find-dest (cup to-move)
  (let* ((to-match
           (do ((to-match cup (1- to-match)))
               ((not (find to-match to-move)) to-match))))
    ;;    (format t "~a ~a ~a~%" to-match (subseq *current* 0 10) to-move)
    (if (<= to-match 0)
        (find-dest *max-cup* to-move)
        to-match)))

(defun move ()
  (let* ((to-move-start (gethash *current* *lookup*))
         (to-move (list to-move-start
                        (gethash to-move-start *lookup*)
                        (gethash (gethash to-move-start *lookup*) *lookup*)))
         (to-move-end (nth 2 to-move)))
    (setf (gethash *current* *lookup*) (gethash to-move-end *lookup*))
    (let* ((dest (find-dest (1- *current*) to-move)))
;;      (format t "pick up: ~a (~a), dest: ~a~%" to-move to-move-end dest)
      (setf (gethash to-move-end *lookup*) (gethash dest *lookup*))
      (setf (gethash dest *lookup*) to-move-start)
      (setf *current* (gethash *current* *lookup*)))))

(defun circular (seq)
  (setf (cdr (last seq)) seq))

(defun parse-circle (str)
  (mapcar #'(lambda (c) (parse-integer (coerce (list c) 'string))) (coerce str 'list)))

(defun build-lookup (circle)
  (loop for cons on (circular circle) do
      (let ((n (car cons)))
        (if (gethash n *lookup*)
            (return nil)
            (setf (gethash n *lookup*) (cadr cons))))))

(defun solve ()
  (let ((circle (parse-circle *input*)))
    (setf *max-cup* (reduce #'max circle))
    (setf *current* (first circle))
    (setf *lookup* (make-hash-table))
    (build-lookup circle)
    (dotimes (x 100)
      (do ((z *current* (gethash z *lookup*))
           (i 0 (1+ i)))
          ((= i 9))
        (format t "~a " z))
      (format t "~%")
      (move))
    (do ((z *current* (gethash z *lookup*))
         (i 0 (1+ i)))
        ((= i 9))
      (format t "~a " z))
    (format t "~%")
    ))

(defun extend-circle (circle)
  (let ((max-cup (reduce #'max circle)))
    (append circle (upto 1000001 :min (1+ max-cup)))))


(defun solve2 ()
  (let ((circle  (extend-circle (parse-circle *input*))))
    (setf *max-cup* (reduce #'max circle))
    (setf *current* (first circle))
    (setf *lookup* (make-hash-table))
    (build-lookup circle)
    (dotimes (x 10000000)
      (move))
    (format t "done~%")
    (let ((cup-1 (gethash 1 *lookup*)))
      (format t "~a x ~a = ~a" cup-1 (gethash cup-1 *lookup*) (* cup-1 (gethash cup-1 *lookup*))))))
