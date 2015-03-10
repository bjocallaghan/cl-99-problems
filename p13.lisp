(use-package :rt)

(deftest example
    (encode-direct '(a a a a b c c a a d e e e e))
  ((4 A) B (2 C) (2 A) D (4 E)))


;;; first attempt: i was still in the groove of the previous problems
;;; i kind of like it, but possibly overcomplicated

(defun encode-direct (list)
  (labels ((last-sym (acc)
             (if (atom (car acc))
                 (car acc)
                 (cadar acc)))
           (incf-acc (acc)
             (if (atom (car acc))
                 (cons (list 2 (car acc)) (cdr acc))
                 (progn (incf (caar acc)) acc)))
           (encode (list acc)
             (if list
                 (encode (cdr list) (if (equal (car list) (last-sym acc))
                                       (incf-acc acc)
                                       (cons (car list) acc)))
                 (nreverse acc))))
    (encode list nil)))

;; simpler(?) approach; use multiple registers

(defun encode-direct-alternate (list)
  (let ((start (gensym)))
    (labels ((encode-subitem (symbol score)
               (if (= 1 score)
                   symbol
                   (list score symbol)))
             (encode (list current score acc)
               (if list
                   (if (equal (car list) current)
                       (encode (cdr list) current (1+ score) acc)
                       (encode (cdr list) (car list) 1
                               (unless (equal current start)
                                 (cons (encode-subitem current score) acc))))
                   (cons (encode-subitem current score) acc))))
      (nreverse (encode list start 0 nil)))))

(do-tests)
