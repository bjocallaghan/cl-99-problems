(defun palindrome-p (lst)
  (let ((rev (reverse lst)))
    (equal lst rev)))

(palindrome-p '(x a m a x))
(palindrome-p '(a b c d e))
