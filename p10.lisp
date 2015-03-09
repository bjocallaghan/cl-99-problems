(use-package :rt)

(deftest example
    (encode '(a a a a b c c a a d e e e e))
  ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E)))

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

(defun encode (list)
  (mapcar #'(lambda (x) (list (length x) (car x))) (pack list)))

(do-tests)
