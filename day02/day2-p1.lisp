(load "~/.sbclrc")
(ql:quickload "cl-ppcre")


(defun regex-match (str regex)
    (let ((x (cl-ppcre:all-matches regex str)))
    (subseq str (car x) (cadr x))))


(defun parse-id (str)
    (parse-integer (regex-match (car (cl-ppcre:split ":" str)) "([0-9]+)")))


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


(defun verify-sets (sets)
    (loop for set in sets
    append (list
        (loop for ball in set
            collect (if (equal (cadr ball) "red")
                        (if (<= (car ball) 12) 1 0)
                    (if (equal (cadr ball) "green")
                        (if (<= (car ball) 13) 1 0)
                    (if (equal (cadr ball) "blue")
                        (if (<= (car ball) 14) 1 0))))
        ))))


(defun verify-game (results)
    (loop for result in results
    do (if (find 0 result) (return-from verify-game NIL)))
    1)


(defvar n 0)
(with-open-file (file "input.txt" :direction :input)
    (loop for line = (read-line file nil)
        while line
        do  
        (if (verify-game (verify-sets (parse-sets line))) (incf n (parse-id line)))
    ))
(format t "~a~%" n)  ; Answer: 2528
