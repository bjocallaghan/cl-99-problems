;;; i think this cons unnecessarily
(defun my-flatten (lst)
  (labels ((flat (lst acc)
             (if (null lst)
                 (nreverse acc)
                 (progn
                   (if (atom (car lst))
                       (push (car lst) acc)
                       (dolist (i (flat (car lst) nil))
                         (push i acc))) ; unnecessary cons?
                   (flat (cdr lst) acc)))))
    (flat lst nil)))

;;; i think this doesn't cons unnecessarily
(defun my-flatten (lst)
  (labels ((flat (lst acc)
             (if (null lst)
                 acc
                 (progn
                   (if (atom (car lst))
                       (push (car lst) acc)
                       (setf acc (flat (car lst) acc)))
                   (flat (cdr lst) acc)))))
    (nreverse (flat lst nil))))
