(defun read-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *lines* (read-lines "input.txt"))

(defun test-line (line)
  (let* ((dash-pos (position #\- line))
         (space-pos (position #\Space line))
         (start (parse-integer (subseq line 0 dash-pos)))
         (end (parse-integer (subseq line (1+ dash-pos) space-pos)))
         (repeated (char line (1+ space-pos)))
         (password (subseq line (+ 4 space-pos)))
         (occurrences (count repeated password)))
    (and (<= start occurrences)
         (>= end occurrences))))

(defun test-line2 (line)
  (let* ((dash-pos (position #\- line))
         (space-pos (position #\Space line))
         (start (1- (parse-integer (subseq line 0 dash-pos))))
         (end (1- (parse-integer (subseq line (1+ dash-pos) space-pos))))
         (repeated (char line (1+ space-pos)))
         (password (subseq line (+ 4 space-pos))))
    (or
     (and (char= (char password start) repeated)
          (char/= (char password end) repeated))
     (and (char/= (char password start) repeated)
          (char= (char password end) repeated)))))

;; (test-line2 "1-3 a: abcde")
;; (test-line2 "1-3 b: cdefg")
;; (test-line2 "2-9 c: ccccccccc")


(count-if #'test-line *lines*)
(count-if #'test-line2 *lines*)
