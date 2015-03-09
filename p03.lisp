(use-package :rt)

(deftest example
    (element-at '(a b c d e) 3) C)

(defun nth-cdr (list n)
  (if (plusp n)
      (nth-cdr (cdr list) (1- n))
      list))

(defun element-at (list position)
  (car (nth-cdr list (1- position))))

(do-tests)
