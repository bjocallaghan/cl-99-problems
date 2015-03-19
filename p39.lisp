(defun prime-range (n m)
  "Return a list of prime numbers between N and M (inclusive, inclusive)."
  (loop for i from n to m when (factor:primep i) collect i))

