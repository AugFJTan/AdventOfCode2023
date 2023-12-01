(load "~/.sbclrc")
(ql:quickload "cl-ppcre")


(defun filter-digits (lst)
    (loop for c in lst
        if (digit-char-p c) collect c))


(defun combine-digits (lst)
    (parse-integer
    (if (> (list-length lst) 1) 
    (coerce (list (first lst) (car (last lst))) 'string)
    (coerce (list (first lst) (first lst)) 'string))))


(defun replace-number (str name digit)
    (if (search name str) (cl-ppcre:regex-replace-all name str digit) str))


(defun substitute-numbers (str)
    (replace-number 
    (replace-number
    (replace-number
    (replace-number
    (replace-number
    (replace-number
    (replace-number
    (replace-number
    (replace-number str "one" "o1e")
                        "two" "t2o")
                        "three" "t3e")
                        "four" "f4r")
                        "five" "f5e")
                        "six" "s6x")
                        "seven" "s7n")
                        "eight" "e8t")
                        "nine" "n9e"))  ; Not optimal, but it works XD


(defvar n 0)
(with-open-file (file "input.txt" :direction :input)
    (loop for line = (read-line file nil)
        while line
        do  
        (incf n (combine-digits (filter-digits (coerce (substitute-numbers line) 'list))))
    ))
(format t "~a~%" n)  ; Answer: 54875
