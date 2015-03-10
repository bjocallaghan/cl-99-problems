(deftest example-1
    (range 4 9)
  (4 5 6 7 8 9))

(deftest example-2
    (range 9 4)
  (9 8 7 6 5 4))

;; a bit of obscurity/backward thinking means we don't have to nreverse result
(defun range (n m)
  (let (result)
    (do ((i m (+ i (if (< n m) -1 1))))
        ((= i (+ n (if (< n m) -1 1))) result)
      (push i result))))

;; run tests twice--assure non-destructiveness
(do-tests) (do-tests)
