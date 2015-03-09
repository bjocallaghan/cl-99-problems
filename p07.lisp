(use-package :rt)

(deftest example
    (my-flatten '(a (b (c d) e))) (A B C D E))

(defun my-flatten (list)
  (labels ((flat (list acc)
             (if list
                 (flat (cdr list) (if (atom (car list))
                                      (cons (car list) acc)
                                      (nconc (flat (car list) nil) acc)))
                 acc)))
    (nreverse (flat list nil))))

(do-tests)
