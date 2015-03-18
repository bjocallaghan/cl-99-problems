(defun prime-factors-mult (n)
  (let ((factors (factor:prime-factorization n)))
    (mapcar #'(lambda (x) (list x (count x factors)))
            (remove-duplicates factors))))
