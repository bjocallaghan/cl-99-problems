(defun my-gcd (n m)
  (labels ((sync-walk (a b acc)
             (cond ((or (null a) (null b)) acc)
                   ((= (car a) (car b))
                    (push (car a) acc)
                    (sync-walk (cdr a) (cdr b) acc))
                   ((> (car a) (car b))
                    (sync-walk a (cdr b) acc))
                   ((< (car a) (car b))
                    (sync-walk (cdr a) b acc)))))
    (apply #'* (sync-walk (factor:prime-factorization n)
                          (factor:prime-factorization m)
                          nil))))

(loop for i from 2 to 1000 do
     (loop for j from 2 to 1000 do
          (unless (= (my-gcd i j) (gcd i j))
            (error (format nil "validation fail: ~d ~d" i j)))))
