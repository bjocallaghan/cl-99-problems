(deftest example
    (remove-at '(a b c d) 2)
  (A C D))

(defun remove-at (list position)
  (labels ((remove-at (list position)
             (when list
               (cons (car list)
                     (remove-at (if (zerop position)
                                    (cddr list)
                                    (cdr list))
                                (1- position))))))
    (remove-at list (- position 2))))

;; run tests twice--assure non-destructiveness
(do-tests) (do-tests)
