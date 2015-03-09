(use-package :rt)

(deftest example
    (my-reverse '(a b c d)) (d c b a))

(defun my-reverse (list)
  (labels ((rev (list acc)
             (if list
                 (progn (push (car list) acc) (rev (cdr list) acc))
                 acc)))
    (rev list nil)))

(do-tests)
