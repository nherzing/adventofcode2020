(defpackage :d14
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun parse-range (range-str)
  (let* ((parts (string-split-at "-" range-str))
        (lower (parse-integer (first parts)))
        (upper (parse-integer (second parts))))
    (lambda (x) (<= lower x upper))))

(defun parse-rule (rule-str)
  (let* ((parts (string-split-at ": " rule-str))
         (title (subseq (first parts) 0 (length (first parts))))
         (ranges (string-split-at " or " (second parts)))
         (range1 (parse-range (first ranges)))
         (range2 (parse-range (second ranges))))
    (list title
          (lambda (x)
            (or
             (funcall range1 x)
             (funcall range2 x))))))

(defun parse-ticket (str)
  (mapcar #'parse-integer (string-split-at "," str)))

(defun invalid-field (val rules)
  (notany #'(lambda (rule) (funcall (second rule) val)) rules))

(defun invalid-vals (ticket rules)
  (remove-if-not #'(lambda (val) (invalid-field val rules)) ticket))

(defun invalid-ticket-p (ticket rules)
  (some #'(lambda (val) (invalid-field val rules)) ticket))

(defun solve ()
  (let* ((groups (split-at "" *lines* :test #'string=))
         (rules (mapcar #'parse-rule (first groups)))
         (your-ticket (parse-ticket (second (second groups))))
         (nearby-tickets (mapcar #'parse-ticket (rest (third groups)))))
    (reduce #'+ (apply #'append (mapcar #'(lambda (ticket) (invalid-vals ticket rules)) nearby-tickets)))))

(defun matching-rules (val rules)
  (remove-if-not #'(lambda (rule) (funcall (second rule) val)) rules))

(defun ticket-matching-rules (ticket rules)
  (mapcar #'(lambda (val) (mapcar #'first (matching-rules val rules))) ticket))

(defun solve2 ()
  (let* ((groups (split-at "" *lines* :test #'string=))
         (rules (mapcar #'parse-rule (first groups)))
         (your-ticket (parse-ticket (second (second groups))))
         (nearby-tickets (mapcar #'parse-ticket (rest (third groups))))
         (valid-tickets (remove-if #'(lambda (ticket) (invalid-ticket-p ticket rules)) nearby-tickets))
         (tickets-matching-rules (mapcar #'(lambda (ticket) (ticket-matching-rules ticket rules)) valid-tickets))
         (rule-mappings (make-hash-table :test 'equal))
         (result (apply #'mapcar #'(lambda (&rest rs)
                                     (reduce #'(lambda (a b) (intersection a b :test #'string=)) rs)) tickets-matching-rules))
         (ones '()))
    (do ((i 0 (1+ i)))
        ((= 20 i) result)
     (format t "ones: ~a~%" ones)
      (format t "result: ~a~%~%" result)
      (setf ones (apply #'append (remove-if-not #'(lambda (x) (= 1 (length x))) result)))
      (setf result (mapcar #'(lambda (x) (if (= (length x) 1) x (remove-if #'(lambda (rule) (find rule ones :test #'string=)) x))) result)))))
