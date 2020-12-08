(defpackage :d8
  (:use :common-lisp :common))

(defparameter *lines* (read-lines "input.txt"))

(defclass instr ()
  ((opcode :initarg :opcode)
   (arg :initarg :arg)))

(defclass program ()
  ((instrs :initarg :instrs)
   (acc :initform 0)
   (pc :initform 0)))

(defun parse-line (line)
  (let ((parts (string-split-at " " line)))
    (make-instance 'instr
                   :opcode (first parts)
                   :arg (parse-integer (second parts)))))


(defun parse (input)
  (make-instance 'program :instrs (mapcar #'parse-line input)))

(defun reset-program (program)
  (setf (slot-value program 'pc) 0)
  (setf (slot-value program 'acc) 0))

(defun get-pc (program)
  (slot-value program 'pc))

(defun get-acc (program)
  (slot-value program 'acc))

(defun get-instr (program i)
  (nth i (slot-value program 'instrs)))

(defun terminalp (program)
  (= (get-pc program) (length (slot-value program 'instrs))))

(defun step-program (program)
  (let* ((current-instr (nth (get-pc program) (slot-value program 'instrs)))
         (opcode (slot-value current-instr 'opcode)))
    (cond
      ((string= opcode "acc")
       (setf (slot-value program 'acc) (+ (slot-value program 'acc) (slot-value current-instr 'arg)))
       (incf (slot-value program 'pc)))
      ((string= opcode "jmp")
       (setf (slot-value program 'pc) (+ (slot-value program 'pc) (slot-value current-instr 'arg))))
      ((string= opcode "nop")
       (incf (slot-value program 'pc)))
      )))

(defun eval-program (program)
  (reset-program program)
  (do ((next-pc 0 (get-pc program))
       (pcs '() (cons next-pc pcs)))
      ((terminalp program) (list t (get-acc program)))
    (if (find next-pc pcs)
        (return (list nil (get-acc program))))
    (step-program program)))


(defun solve1 ()
  (let ((program (parse *lines*)))
    (eval-program program)))

(solve1)

(defun upto (max)
  (loop for i from 0 to (1- max) collecting i))

(defun solve2 ()
  (let ((program (parse *lines*)))
    (dolist (i (upto (length *lines*)))
      (let* ((instr (get-instr program i))
            (opcode (slot-value instr 'opcode)))
        (cond ((string= "nop" opcode)
               (setf (slot-value instr 'opcode) "jmp")
               (let ((res (eval-program program)))
                 (if (first res)
                     (return (second res))))
               (setf (slot-value instr 'opcode) "nop"))
              ((string= "jmp" opcode)
               (setf (slot-value instr 'opcode) "nop")
               (let ((res (eval-program program)))
                 (if (first res)
                     (return (second res))))
               (setf (slot-value instr 'opcode) "jmp")))))))

(solve2)
