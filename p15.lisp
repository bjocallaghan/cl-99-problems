(use-package :rt)

(deftest example
    (repli '(a b c) 3)
  (A A A B B B C C C))

(defun repli (list times)
  (labels ((replicate-item (x)
             (let (x-list)
               (dotimes (i times x-list) (push x x-list))))
           (replicate (list)
             (when list (nconc (replicate-item (car list))
                               (replicate (cdr list))))))
    (replicate list)))

(do-tests)
