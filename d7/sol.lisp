(defpackage :d5
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun trim-bags (str)
  (let ((bags (search " bag" str)))
    (subseq str 0 bags)))

(defun parse-req (raw-req)
  (let ((space (position #\Space raw-req :test #'char=)))
    (list (parse-integer (subseq raw-req 0 space))
          (subseq raw-req (1+ space)))))

(defun parse-reqs (includes)
  (if (string= includes "no other bags")
      '()
      (let ((raw-reqs (mapcar #'trim-bags (string-split-at ", " includes))))
        (mapcar #'parse-req raw-reqs))))

(defun parse-line (line)
  (let* ((parts (string-split-at " contain " (string-trim "." line)))
         (bag-color (trim-bags (first parts)))
         (includes (parse-reqs (second parts))))
    (list bag-color includes)))

(defun can-hold-bagp (rule bag)
  (some #'(lambda (r) (string= bag (second r))) (second rule)))

(defun bags-that-can-hold (bag rules)
  (mapcar #'first (remove-if-not #'(lambda (rule) (can-hold-bagp rule bag)) rules)))

(defun solve1 ()
  (let* ((rules (mapcar #'parse-line *lines*))
        (result (bags-that-can-hold "shiny gold" rules)))
    (loop
      (let* ((new-results (mapcar #'(lambda (bag) (bags-that-can-hold bag rules)) result))
             (new-result (reduce #'(lambda (a b) (union a b :test #'string=)) new-results :initial-value result)))
        (when (subsetp new-result result :test #'string=)
          (return (length new-result)))
        (setf result new-result)))))

(solve1)

(defun bags-needed-to-hold (bag rules)
  (let* ((rule (find bag rules :key #'first :test #'string=))
         (reqs (second rule))
         (nested-counts (mapcar #'(lambda (req) (* (first req) (bags-needed-to-hold (second req) rules))) reqs)))
    (+ 1 (reduce #'+ nested-counts))))

(defun solve2 ()
  (let* ((rules (mapcar #'parse-line *lines*)))
    (1- (bags-needed-to-hold "shiny gold" rules)))
  )

(solve2)
