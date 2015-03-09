(use-package :rt)

(deftest example
    (my-length '(a b c d)) 4)

(defun my-length (list)
  (labels ((len (list acc)
             (if list
                 (len (cdr list) (1+ acc))
                 acc)))
    (len list 0)))

(do-tests)
