(use-package :rt)

(deftest example-1
    (paldindromep '(x a m a x)) t)

(deftest example-2
    (paldindromep '(1 2 3 4 5)) nil)

(defun paldindromep (list)
  (equal (reverse list) list))

(do-tests)
