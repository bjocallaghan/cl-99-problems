(defun goldbach (n)
  "Evaluates to a Goldbach composition for positive even N greater than 2."
  (unless (and (> n 2) (plusp n)) (error "N must be greater than 2 and even."))
  (labels ((seek-solution (prime-list)
             (let ((complement (- n (car prime-list))))
               (if (factor:primep complement)
                   (list (car prime-list) complement)
                   (seek-solution (cdr prime-list))))))
    (seek-solution (loop for i from 3 to n when (factor:primep i) collect i))))

(defun goldbach-list (n m)
  (loop for i from n to m when (evenp i)
     do (let ((composition (goldbach i)))
          (format t "~&~d = ~d + ~d~%"
                  i (first composition) (second composition)))))
