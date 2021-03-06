(defpackage :d24
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun parse-line (line)
  (let ((result nil))
    (loop
      (cond ((string= line "") (return (reverse result)))
            ((string= (subseq line 0 1)  "e")  (setf line (subseq line 1)) (push 'e result))
            ((string= (subseq line 0 1)  "w")  (setf line (subseq line 1)) (push 'w result))
            ((string= (subseq line 0 2) "se") (setf line (subseq line 2)) (push 'se result))
            ((string= (subseq line 0 2) "sw") (setf line (subseq line 2)) (push 'sw result))
            ((string= (subseq line 0 2) "nw") (setf line (subseq line 2)) (push 'nw result))
            ((string= (subseq line 0 2) "ne") (setf line (subseq line 2)) (push 'ne result))))))

(defun walk (path &key (xy '(0 0)))
  (let ((x (first xy))
        (y (second xy)))
    (dolist (dir path)
      (cond ((eq dir 'e ) (setf x (+ x 2)))
            ((eq dir 'se) (incf x) (incf y))
            ((eq dir 'sw) (decf x) (incf y))
            ((eq dir 'w ) (setf x (- x 2)))
            ((eq dir 'nw) (decf x) (decf y))
            ((eq dir 'ne) (incf x) (decf y))))
    (list x y)))

(defun neighbors (xy)
  (mapcar #'(lambda (p) (walk (list p) :xy xy)) '(e se sw w nw ne)))

(defun solve ()
  (let ((states (make-hash-table :test 'equal))
        (paths (mapcar #'parse-line *lines*)))
    (dolist (path paths)
      (let ((end (walk path)))
        (if (gethash end states)
            (remhash end states)
            (setf (gethash end states) t))))
    (loop for key being the hash-keys in states counting 1)))

(defun all-tiles-to-check (states)
  (let ((xy-to-check nil))
    (loop for xy being the hash-keys in states do
      (pushnew xy xy-to-check)
      (setf xy-to-check (union xy-to-check (neighbors xy))))
    xy-to-check))

(defun advance (states)
  (let ((new-states (make-hash-table :test 'equal))
        (xy-to-check (all-tiles-to-check states)))
    (dolist (xy xy-to-check)
      (let ((black (gethash xy states))
            (black-neighbor-count (count-if #'(lambda (xy) (gethash xy states)) (neighbors xy))))
        (if black
            (when (not (or (= 0 black-neighbor-count) (> black-neighbor-count 2)))
              (setf (gethash xy new-states) t))
            (when (= 2 black-neighbor-count)
              (setf (gethash xy new-states) t)))))
    new-states))

(defun solve2 ()
  (let ((states (make-hash-table :test 'equal))
        (paths (mapcar #'parse-line *lines*)))
    (dolist (path paths)
      (let ((end (walk path)))
        (if (gethash end states)
            (remhash end states)
            (setf (gethash end states) t))))
    (dotimes (x 100)
      (setf states (advance states))
      (format t "after ~a: ~a~%" (1+ x) (loop for key being the hash-keys in states counting 1)))))
