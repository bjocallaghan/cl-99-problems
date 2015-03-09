(use-package :rt)

(deftest example
    (my-last '(a b c d)) (d))

(defun my-last (list)
  (if (null (cdr list))
      list
      (my-last (cdr list))))

(do-tests)
