(deftest example-1
    (third (combination 3 '(a b c d e f)))
  (A B E))

(deftest example-2
    (length (combination 3 '(a b c d e f g h i j k l)))
  220)

(defun range (n m)
  (let (result)
    (do ((i m (+ i (if (< n m) -1 1))))
        ((= i (+ n (if (< n m) -1 1))) result)
      (push i result))))

(defun get-subsequence-at (sequence indicies)
  (mapcar #'(lambda (x) (nth x sequence)) (reverse indicies)))

(defun indicies-1+ (indicies max-index)
  (let ((max-indicies (range max-index (1+ (- max-index (length indicies))))))
    (if (equal max-indicies indicies)
        :busted
        (let ((position (do ((place 0 (1+ place)))
                            ((not (= (nth place indicies)
                                     (nth place max-indicies)))
                             place))))
          (incf (nth position indicies))
          (setf indicies (reset-following-indicies indicies position))))))

(defun reset-following-indicies (indicies i)
  (let ((copy (copy-seq indicies)))
    (do ((position (1- i) (1- position)))
        ((< position 0) copy)
      (setf (nth position copy) (1+ (nth (1+ position) copy))))))

(defun combination (k list)
  (let (combinations
        (max (1- (length list))))
    (do ((indicies (range (1- k) 0) (indicies-1+ indicies max)))
        ((eq indicies :busted) (nreverse combinations))
      (push (get-subsequence-at list indicies) combinations))))

;; run tests twice--assure non-destructiveness
(do-tests) (do-tests)
