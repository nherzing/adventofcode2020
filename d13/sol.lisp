(defpackage :d12
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun wait-time (ts bus)
  (let ((m (mod ts bus)))
    (if (= 0 m)
        0
        (- bus m))))

(defun solve ()
  (let* ((my-earliest (parse-integer (first *lines*)))
        (buses (mapcar #'parse-integer (remove-if #'(lambda (x) (string= "x" x)) (string-split-at "," (second *lines*)))))
        (earliest-bus (first (sort buses #'< :key #'(lambda (bus) (wait-time my-earliest bus))))))
    (* earliest-bus (wait-time my-earliest earliest-bus))))

(defun validp (n rule)
  (or (= n (second rule))
      (= 0 (mod (+ n (second rule)) (first rule)))))

(defun find-it (first-depart freq rule)
  (do ((res '())
       (n first-depart (+ n freq)))
      ((> (length res) 1) (list (- (first res) (second res)) (second res)))
    (when (validp n rule)
      (push n res))))

(defun solve2 ()
  (let* ((rules (loop for item in (string-split-at "," (second *lines*))
                     for i = 0 then (1+ i)
                      when (string/= item "x") collect (list (parse-integer item) i)))
         (base-rule (first rules))
         (first-depart (second base-rule))
         (freq (first base-rule)))
    (dolist (rule (rest rules))
      (let ((sol (find-it first-depart freq rule)))
        (setf first-depart (second sol))
        (setf freq (first sol))))
    first-depart))
