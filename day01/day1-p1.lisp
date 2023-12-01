(defun filter-digits (lst)
    (loop for c in lst
        if (digit-char-p c) collect c))


(defun combine-digits (lst)
    (parse-integer
    (if (> (list-length lst) 1) 
    (coerce (list (first lst) (car (last lst))) 'string)
    (coerce (list (first lst) (first lst)) 'string))))


(defvar n 0)
(with-open-file (file "input.txt" :direction :input)
    (loop for line = (read-line file nil)
        while line
        do  
        (incf n (combine-digits (filter-digits (coerce line 'list))))
    ))
(format t "~a~%" n)  ; Answer: 55538
