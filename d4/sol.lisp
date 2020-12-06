(defpackage :d4
  (:use :common-lisp)
  (:use :common))

(defparameter *lines* (read-lines "input.txt"))

(defun passport-parse (str)
  (mapcar
       #'(lambda (str) (split-at ":" str :test #'string=))
       (split-at " " str :test #'string=)))

(defun passport-keys (passport)
  (map 'list #'first passport))

(defun passport-val (passport key)
  (second (find key passport :key #'first :test #'string=)))

(defconstant required-keys '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(defun validp (pp)
  (let ((keys (passport-keys pp)))
    (and (subsetp required-keys keys :test #'string=)
         (let ((byr (parse-integer (passport-val pp "byr"))))
           (and byr
                (<= 1920 byr 2002)))
         (let ((iyr (parse-integer (passport-val pp "iyr"))))
           (and iyr
                (<= 2010 iyr 2020)))
         (let ((eyr (parse-integer (passport-val pp "eyr"))))
           (and eyr
                (<= eyr 2030)
                (>= eyr 2020)))
         (let* ((hgt (passport-val pp "hgt"))
               (num (parse-integer (subseq hgt 0 (- (length hgt) 2))))
               (unit (subseq hgt (- (length hgt) 2))))
           (and num
                (or (and
                     (string= unit "cm")
                     (<= 150 num 193))
                    (and
                     (string= unit "in")
                     (<= 59 num 76)))))
         (let ((hcl (passport-val pp "hcl")))
           (and (char= (char hcl 0) #\#)
                (= 7 (length hcl))
                (every #'(lambda (c) (find c "0123456789abcdef" :test #'char=)) (subseq hcl 1))))
         (find (passport-val pp "ecl") '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=)
         (let ((id (passport-val pp "pid")))
           (and (= (length id) 9)
                (parse-integer id)))
         )))

(defun solve ()
  (let* ((raw-passports (map 'list #'(lambda (s) (join s " ")) (split-at "" *lines* :test #'string=)))
        (passports (map 'list #'passport-parse raw-passports)))
    (count-if #'validp passports)))


(solve)
