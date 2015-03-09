(use-package :rt)

(deftest example
    (pack '(a a a a b c c a a d e e e e))
  ((A A A A) (B) (C C) (A A) (D) (E E E E)))

(defun pack (list)
  (labels ((pack* (list acc)
             (if list
                 (pack* (cdr list) (if (equal (car list) (caar acc))
                                       (progn
                                         (setf (car acc)
                                               (cons (car list)
                                                     (car acc))) acc)
                                       (cons (list (car list)) acc)))
                 (nreverse acc))))
    (pack* list nil)))

(do-tests)
