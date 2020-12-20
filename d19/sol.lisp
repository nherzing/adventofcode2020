(defpackage :d19
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun consume (chars rule rule-table)
;;  (format t "consume: '~a', rule: ~a~%" (coerce chars 'string) rule)
  (cond ((eq 'char (first rule))
         (if (and (first chars) (char= (second rule) (first chars)))
             (list (rest chars))
             nil))
        ((eq 'seq (first rule))
         (let ((consumed (list chars)))
           (dolist (rule-idx (second rule))
             (setf consumed (apply
                             #'append
                             (mapcar #'(lambda (n-chars)
                                         (consume n-chars (gethash rule-idx rule-table) rule-table))
                                     consumed))))
           consumed))
        ((eq 'or (first rule))
         (append
          (consume chars (second rule) rule-table)
          (consume chars (third rule) rule-table)))))

(defun validp (str rule-table)
;;  (format t "TEST: ~a~%" str)
  (position nil (consume (coerce str 'list) (gethash 0 rule-table) rule-table)))

(defun build-rule-table (rule-strs)
  (let ((rule-table (make-hash-table)))
    (dolist (rule-str rule-strs)
      (let* ((colon-idx (search ":" rule-str))
             (rule-id (parse-integer (subseq rule-str 0 colon-idx)))
             (bar-idx (search "|" rule-str))
             (quote-idx (search "\"" rule-str))
             (rule
               (cond (quote-idx
                      (list 'char (char rule-str (1+ quote-idx))))
                     (bar-idx
                      (let* ((parts (string-split-at " | " (subseq rule-str (+ 2 colon-idx))))
                             (first-seq (mapcar #'parse-integer (string-split-at " " (first parts))))
                             (second-seq (mapcar #'parse-integer (string-split-at " " (second parts)))))
                        (list 'or
                              (list 'seq first-seq)
                              (list 'seq second-seq))))
                     (t
                      (let ((sub-rules (mapcar #'parse-integer (string-split-at " " (subseq rule-str (+ 2 colon-idx))))))
                        (list 'seq sub-rules))))))
;;        (format t "~a: ~a~%" rule-id rule)
        (setf (gethash rule-id rule-table) rule)))
    rule-table))

(defun solve ()
  (let* ((parts (split-at "" *lines* :test #'string=))
         (rule-table (build-rule-table (first parts))))
    (count-if #'(lambda (str) (validp str rule-table)) (second parts))))

(defun solve2 ()
  (let* ((parts (split-at "" *lines* :test #'string=))
         (rules-strs (append (first parts) '("8: 42 | 42 8" "11: 42 31 | 42 11 31")))
         (rule-table (build-rule-table rules-strs)))
    (count-if #'(lambda (str) (validp str rule-table)) (second parts))))
