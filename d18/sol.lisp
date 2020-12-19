(defpackage :d18
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defun read-operand (str eval-expr)
;;  (format t "read-operand:~a~%" str)
  (if (char= #\( (char str 0))
      (funcall eval-expr (subseq str 1))
      (let ((first-space (search " " str))
            (rparen (search ")" str)))
        (cond ((or first-space rparen)
               (values
                (parse-integer (subseq str 0 (apply #'min (remove nil (list rparen first-space)))))
                (subseq str (apply #'min (remove nil (list rparen first-space))))))
              (t
               (values
                (parse-integer str)
                ""))))))

(defun read-operator (str)
;;  (format t "read-operator:~a~%" str)
  (if (char= #\+ (char str 1))
      (values #'+ (subseq str 3))
      (values #'* (subseq str 3))))

(defun eval-expr (str)
  (multiple-value-bind (lval str) (read-operand str #'eval-expr)
    (if (string= "" str)
        lval
        (multiple-value-bind (op str) (read-operator str)
          (multiple-value-bind (rval str) (read-operand str #'eval-expr)
;;            (format t "eval-expr:~a ~a, ~a, \"~a\"~%" lval rval op str)
            (cond ((string= str "")
                   (funcall op lval rval))
                  ((char= #\) (char str 0))
                   (values (funcall op lval rval) (subseq str 1)))
                  (t
                   (eval-expr (concatenate 'string (format nil "~a" (funcall op lval rval)) str)))))))))

(defun eval-expr2 (str)
;;  (format t "call:~a~%" str)
  (multiple-value-bind (lval str) (read-operand str #'eval-expr2)
    (if (string= "" str)
        lval
        (if (char= #\) (char str 0))
            (values lval (subseq str 1))
            (multiple-value-bind (op str) (read-operator str)
              (if (eq op #'+)
                  (multiple-value-bind (rval str) (read-operand str #'eval-expr2)
;;                    (format t "eval-expr:~a ~a, ~a, \"~a\"~%" lval rval op str)
                    (cond ((string= str "")
                           (funcall op lval rval))
                          ((char= #\) (char str 0))
                           (values (funcall op lval rval) (subseq str 1)))
                          (t
                           (eval-expr2 (concatenate 'string (format nil "~a" (funcall op lval rval)) str)))))
                  (multiple-value-bind (rval str) (eval-expr2 str)
;;                    (format t "eval-expr:~a ~a, ~a, \"~a\"~%" lval rval op str)
                    (values (funcall op lval rval) str))))))))

(defun solve ()
  (apply #'+ (mapcar #'eval-expr *lines*)))

(defun solve2 ()
  (apply #'+ (mapcar #'eval-expr2 *lines*)))
