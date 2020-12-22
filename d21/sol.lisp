(defpackage :d20
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun parse-food (line)
  (let ((parts (string-split-at " (contains " (string-right-trim ")" line))))
    (list
     (string-split-at " " (first parts))
     (string-split-at ", " (second parts)))))

(defun solve ()
  (let* ((causes (make-hash-table :test 'equal))
        (foods (mapcar #'parse-food *lines*))
        (all-ingredients (reduce #'(lambda (a b) (union a b :test 'equal)) (mapcar #'first foods))))
    (dolist (food foods)
      (dolist (allergen (second food))
        (if (gethash allergen causes)
            (setf (gethash allergen causes) (intersection (gethash allergen causes) (first food) :test 'equal))
            (setf (gethash allergen causes) (first food)))
        ))
    (let ((unknown-ingredients (remove-if #'(lambda (ingredient)
                                              (loop for ingredients being the hash-values of causes do
                                                (if (find ingredient ingredients :test 'equal)
                                                    (return t)))) all-ingredients))
          (count 0))
      (dolist (food foods)
        (setf count (+ count (length (intersection unknown-ingredients (first food) :test 'equal)))))
      count)))

(defun iterate (causes)
  (loop for allergen being the hash-keys of causes do
    (let ((ingredients (gethash allergen causes)))
      (when (= 1 (length ingredients))
        (remhash allergen causes)
        (loop for allergen being the hash-keys of causes do
          (setf (gethash allergen causes) (remove (first ingredients) (gethash allergen causes) :test 'equal)))
        (return (list allergen (first ingredients)))))))

(defun solve2 ()
  (let* ((causes (make-hash-table :test 'equal))
         (foods (mapcar #'parse-food *lines*))
         (all-ingredients (reduce #'(lambda (a b) (union a b :test 'equal)) (mapcar #'first foods))))
    (dolist (food foods)
      (dolist (allergen (second food))
        (if (gethash allergen causes)
            (setf (gethash allergen causes) (intersection (gethash allergen causes) (first food) :test 'equal))
            (setf (gethash allergen causes) (first food)))
        ))
    (let ((result nil))
      (loop
        (let ((new-one (iterate causes)))
          (if new-one
              (push new-one result)
              (return (join (mapcar #'second (sort result #'string< :key #'first)) ","))))))))
    (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) causes)
  ))
