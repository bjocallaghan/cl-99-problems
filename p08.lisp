(use-package :rt)

(deftest example
    (compress '(a a a a b c c a a d e e e e)) (A B C A D E))

(defun compress (list)
  (labels ((compress* (list acc)
             (if list
                 (compress* (cdr list) (if (equal (car list) (cadr list))
                                           acc
                                           (cons (car list) acc)))
                 (nreverse acc))))
    (compress* list nil)))

(do-tests)
