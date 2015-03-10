(use-package :rt)

(deftest example
    (dupli '(a b c c d))
  (A A B B C C C C D D))

(defun dupli (list)
  (when list
    (cons (car list) (cons (car list) (dupli (cdr list))))))

(do-tests)
