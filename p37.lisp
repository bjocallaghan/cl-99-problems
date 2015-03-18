(defun totient-phi-old (n)
  (unless (plusp n) (error "N must be a positive number."))
  (if (= n 1)
      1
      (1+ (loop for i from 2 to (1- n) when (factor:coprime n i) count i))))

(defun prime-factors-mult (n)
  (let ((factors (factor:prime-factorization n)))
    (mapcar #'(lambda (x) (list x (count x factors)))
            (remove-duplicates factors))))

;;; This is the implementation for the formula given in the problem
;;; statement. However, it produces inconsistent results with the old
;;; totient-phi function, and furthermore the old results can be verified by
;;; sources on the web.
(defun totient-phi-wrong (n)
  (reduce #'+ (mapcar #'(lambda (x) (* (1- (first x)) (expt (first x)
                                                            (1- (second x)))))
                      (prime-factors-mult n))))

;;; I found a different formula for quick computing of the totient
;;; function. Seems to work...
(defun totient-phi (n)
  (* n (reduce #'* (mapcar #'(lambda (x) (- 1 (/ (first x))))
                           (prime-factors-mult n)))))
