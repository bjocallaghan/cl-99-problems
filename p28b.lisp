(defparameter *input* '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))

(defun lfsort (lists)
  (let* ((lists-w-len (mapcar (lambda (x) (cons (length x) x)) lists))
         (lists-w-score (mapcar (lambda (x)
                                  (cons (count (car x)
                                               (mapcar #'car lists-w-len))
                                        x))
                                lists-w-len)))
    (mapcar #'cddr (sort lists-w-score #'< :key #'car))))

(format t "~&~a~%" (lfsort *input*))
