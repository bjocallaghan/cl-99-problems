;; it's random, so a test won't work
;; (deftest example
;;     (rnd-select '(a b c d e f g h) 3)
;;   (E D A))

;; get remove-at-copy from problem 20
(load (make-pathname :name "p20" :defaults *load-pathname*))

(defun rnd-select (list num-elements)
  (let (result
        (len (length list))
        (copy (copy-seq list)))
    (dotimes (i num-elements)
      (let ((index (random len)))
        (push (nth index copy) result)
        (setf copy (remove-at copy (1+ index)))
        (decf len)))
    result))
