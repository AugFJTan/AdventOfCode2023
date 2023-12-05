(load "~/.sbclrc")
(ql:quickload "cl-ppcre")


(defun parse-numbers (str)
    (let ((lst (cl-ppcre:all-matches "([0-9]+)" str)))
    (loop while (not (null lst))
        collect (parse-integer (subseq str (car lst) (cadr lst)))
        do (setf lst (cddr lst)))))


(defun parse-scratchcard (str)
    (let ((scratchcard (cl-ppcre:split "\\|" (cadr (cl-ppcre:split ":" str)))))
    (list (parse-numbers (car scratchcard)) (parse-numbers (cadr scratchcard)))))


(defun calculate-points (numbers)
    (let ((points 0) (winning (car numbers)) (on-hand (cadr numbers)))
    (loop for num in on-hand
        do (if (find num winning) (if (= points 0) (setf points 1) (setf points (* points 2)))))
    points))


(defvar n 0)
(with-open-file (file "input.txt" :direction :input)
    (loop for line = (read-line file nil)
        while line
        do  
        (incf n (calculate-points (parse-scratchcard line)))
    ))
(format t "~a~%" n)  ; Answer: 25004
