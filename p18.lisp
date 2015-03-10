(in-package :cl-user)
(use-package :rt)

(deftest example
    (slice '(a b c d e f g h i k) 3 7)
  (C D E F G))

(defun first-n (list count acc)
  (if (plusp count)
      (first-n (cdr list) (1- count) (cons (car list) acc))
      (nreverse acc)))

(defun slice (list start end)
  (first-n (nthcdr (1- start) list) (- (1+ end) start) nil))

(do-tests)
