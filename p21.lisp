(deftest example
    (insert-at 'alfa '(a b c d) 2)
  (A ALFA B C D))

(defun insert-at (item list position)
  (let (result)
    (loop
       for elt in list
       for counter = 1 then (1+ counter)
       do (when (= counter position) (push item result)) (push elt result))
    (nreverse result)))

;; run tests twice--assure non-destructiveness
(do-tests) (do-tests)
