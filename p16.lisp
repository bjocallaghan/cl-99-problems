(in-package :cl-user)
(use-package :rt)

(deftest example
    (drop '(a b c d e f g h i k) 3)
  (A B D E G H K))

(defun drop (list n)
  (labels ((drop* (list acc count)
             (if list
                 (drop* (cdr list)
                        (if (= count (1- n)) acc (push (car list) acc))
                        (mod (1+ count) n))
                 (nreverse acc))))
    (drop* list nil 0)))

(do-tests)
