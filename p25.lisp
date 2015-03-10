;; it's random, so a test won't work
;; (deftest example
;;     (rnd-permu '(a b c d e f))
;;   (B A D C E F)

;; get rnd-select from probelm 23
(load (make-pathname :name "p23" :defaults *load-pathname*))

(defun rnd-permu (list)
  (rnd-select list (length list)))

(princ (rnd-permu '(a b c d e f)))
