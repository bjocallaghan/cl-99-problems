(use-package :rt)

(deftest example
    (encode-direct '(a a a a b c c a a d e e e e))
  ((4 A) B (2 C) (2 A) D (4 E)))

(defun encode-direct (list)
  (labels ((last-sym (acc)
             if (atom (car acc)) blah blah
             ;; (cadar acc)
             
             )
           (pack* (list acc)
             (if list
                 (pack* (cdr list) (if (equal (car list) (last-sym acc))
                                       (progn (incf (caar acc)) acc)
                                       (cons (list (car list)) acc)))
                 (nreverse acc))))
    (pack* list nil)))

(do-tests)
