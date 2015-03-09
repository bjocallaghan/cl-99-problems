(use-package :rt)

(deftest example
    (my-reverse '(a b c d)) (d c b a))

(defun my-reverse (list)
  (labels ((rev (list acc)
             (if list
                 (rev (cdr list) (cons (car list) acc))
                 acc)))
    (rev list nil)))

(do-tests)
