;; it's random, so a test won't work
;; (deftest example
;;     (lotto-select 6 49)
;;   (23 1 17 33 21 37)

;; get range from problem 20
(load (make-pathname :name "p22" :defaults *load-pathname*))

;; get rnd-select from probelm 23
(load (make-pathname :name "p23" :defaults *load-pathname*))

(defun lotto-select (k n)
  (rnd-select (range 1 n) k))

(princ (lotto-select 6 49))
