(use-package :rt)

(deftest example
    (encode-modified '(a a a a b c c a a d e e e e))
  ((4 A) B (2 C) (2 A) D (4 E)))

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

(defun encode-modified (list)
  (mapcar #'(lambda (x) (if (cdr x)
                            (list (length x) (car x))
                            (car x)))
          (pack list)))

(do-tests)
