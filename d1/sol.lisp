(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *input* (mapcar #'parse-integer (get-file "input.txt")))

(defun sum-top (sum)
  (lambda (pair) (= sum (apply #'+ pair))))

(defun find-sum (seq sum)
  (apply #'*
         (find-if (sum-top 2020)
                  (apply #'append
                         (maplist #'(lambda (a)
                                      (mapcar #'(lambda (b)
                                                  (list (car a) b)) (cdr a)))
                                  seq)))))


(defun find-sum-3 (seq sum)
  (apply #'*
         (find-if (sum-top 2020)
                  (apply #'append
                         (maplist #'(lambda (a)
                                      (apply #'append
                                             (maplist #'(lambda (b)
                                                          (mapcar #'(lambda (c) (list (car a) (car b) c)) (cdr b)))
                                                      (cdr a))))
                                  seq)))))
