(defun totient-phi (n)
  (unless (plusp n) (error "N must be a positive number."))
  (if (= n 1)
      1
      (1+ (loop for i from 2 to (1- n) when (factor:coprime n i) count i))))
