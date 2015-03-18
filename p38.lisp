(defun totient-phi-old (n)
  (unless (plusp n) (error "N must be a positive number."))
  (if (= n 1)
      1
      (1+ (loop for i from 2 to (1- n) when (factor:coprime n i) count i))))

(defun prime-factors-mult (n)
  (let ((factors (factor:prime-factorization n)))
    (mapcar #'(lambda (x) (list x (count x factors)))
            (remove-duplicates factors))))

;;; This is the implementation for the formula given in the problem
;;; statement. However, it produces inconsistent results with the old
;;; totient-phi function, and furthermore the old results can be verified by
;;; sources on the web.
(defun totient-phi-wrong (n)
  (reduce #'+ (mapcar #'(lambda (x) (* (1- (first x)) (expt (first x)
                                                            (1- (second x)))))
                      (prime-factors-mult n))))

;;; I found a different formula for quick computing of the totient
;;; function. Seems to work...
(defun totient-phi (n)
  (* n (reduce #'* (mapcar #'(lambda (x) (- 1 (/ (first x))))
                           (prime-factors-mult n)))))

(defparameter *totients-old* (time (loop for i from 2 to 1000
                                      collect (totient-phi-old i))))
(defparameter *totients-new* (time (loop for i from 2 to 1000
                                      collect (totient-phi i))))

;; Evaluation took:
;;   1.237 seconds of real time
;;   1.230000 seconds of total run time (1.230000 user, 0.000000 system)
;;   [ Run times consist of 0.021 seconds GC time, and 1.209 seconds non-GC time. ]
;;   99.43% CPU
;;   2,680,535,234 processor cycles
;;   61,931,520 bytes consed
  
;; Evaluation took:
;;   0.004 seconds of real time
;;   0.005000 seconds of total run time (0.005000 user, 0.000000 system)
;;   125.00% CPU
;;   9,063,574 processor cycles
;;   589,680 bytes consed

(unless (equal *totients-new* *totients-old*)
  (error "Results from quick calculation do not match 'long' way."))

(loop for i from 0 to 498
   do (format t "~&~2d: ~2d ~2d~%"
              (+ 2 i) (nth i *totients-old*) (nth i *totients-new*)))

