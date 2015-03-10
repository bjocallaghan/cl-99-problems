(deftest example-1
    (rotate '(a b c d e f g h) 3)
(D E F G H A B C))

(deftest example-2
    (rotate '(a b c d e f g h) -2)
  (G H A B C D E F))

(defun first-n (n list)
  "Returns the first N elements in LIST as a list, or nil if N less than 1."
  (labels ((first-n (n list acc)
             (if (and list (plusp n))
                 (first-n (1- n) (cdr list) (cons (car list) acc))
                 (nreverse acc))))
    (first-n n list nil)))

(defun rotate (list n)
  (let ((copy (copy-seq list)))
    (cond
      ((zerop n) copy)
      ((plusp n) (append (nthcdr n copy) (first-n n copy)))
      ((minusp n) (progn
                    (setf (cdr (last copy (1+ (abs n)))) nil) ; cons surgery!
                    (append (last list (abs n)) copy))))))

;; run tests twice--assure non-destructiveness
(do-tests) (do-tests)
