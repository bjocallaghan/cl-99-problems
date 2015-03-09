(use-package :rt)

(deftest example
    (decode '  ((4 a) b (2 c) (2 a) d (4 e)))
  (A A A A B C C A A D E E E E))

(defun decode (list)
  (labels ((decode* (list acc)
             (if list
                 (decode* (cdr list) (nconc (decode-subunit (car list)) acc))
                 (nreverse acc)))
           (decode-subunit (subunit)
             (if (atom subunit)
                 (list subunit)
                 (let (acc)
                   (dotimes (i (first subunit) acc)
                     (push (second subunit) acc))))))
    (decode* list nil)))

(do-tests)
