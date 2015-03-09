(use-package :rt)

(deftest example
    (my-but-last '(a b c d)) (C D))

(defun my-but-last (list)
  (if (cddr list)
      (my-but-last (cdr list))
      list))

(do-tests)
