(use-package :rt)

(deftest example-1
    (palindrome '(x a m a x)) t)

(deftest example-2
    (palindrome '(1 2 3 4 5)) nil)

(defun palindrome (list)
  (equal (reverse list) list))

(do-tests)
