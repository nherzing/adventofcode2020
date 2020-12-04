(defun read-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *lines* (read-lines "input.txt"))

(defun get-loc (map x y)
  (let* ((row (nth (mod y (length map)) map))
         (loc (char row (mod x (length row)))))
    loc))

(defun treep (loc)
  (char= loc #\#))

(defun solve (map right down)
  (do ((trees 0 (if (treep (get-loc map x y)) (1+ trees) trees))
       (x 0 (+ right x))
       (y 0 (+ down y)))
      ((>= y (length map)) trees)))

(solve *lines* 3 1)

(defun solve2 (map slopes)
  (apply #'*
         (map 'list
              #'(lambda (slope) (solve map (first slope) (second slope)))
              slopes)))


(solve2 *lines* '((1 1) (3 1) (5 1) (7 1) (1 2)))
