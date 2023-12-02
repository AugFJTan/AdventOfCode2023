(load "~/.sbclrc")
(ql:quickload "cl-ppcre")


(defun regex-match (str regex)
    (let ((x (cl-ppcre:all-matches regex str)))
    (subseq str (car x) (cadr x))))


(defun parse-sets (str)
    (loop for x in (cl-ppcre:split ";" (cadr (cl-ppcre:split ":" str)))
     append (list (
        loop for y in (cl-ppcre:split "," x)
            collect (list (parse-cube-num y) (parse-cube-colour y))
        ))))


(defun parse-cube-num (str)
    (parse-integer (regex-match str "([0-9]+)")))


(defun parse-cube-colour (str)
    (regex-match str "(red|green|blue)"))


(defun get-colour (set colour)
    (loop for ball in set
        do (if (equal (cadr ball) colour)
            (return-from get-colour (car ball))
        ))
    0)


(defun get-colour-sets (sets colour)
    (loop for set in sets
        collect (get-colour set colour)))


(defun compute-power (sets)
    (*  (apply 'max (get-colour-sets sets "red"))
        (apply 'max (get-colour-sets sets "green"))
        (apply 'max (get-colour-sets sets "blue"))))


(defvar n 0)
(with-open-file (file "input.txt" :direction :input)
    (loop for line = (read-line file nil)
        while line
        do  
        (incf n (compute-power (parse-sets line)))
    ))
(format t "~a~%" n)  ; Answer: 67363
