(in-package :cl-user)
(use-package :rt)

(deftest example
 (split '(a b c d e f g h i k) 3)
  ((A B C) (D E F G H I K)))

(defun split (list n)
  (let* ((copy (copy-seq list))
         (part2 (nthcdr n copy)))
    (setf (cdr (nthcdr (1- n) copy)) nil) ; feels evil but clever
    (list copy part2)))

(do-tests)
