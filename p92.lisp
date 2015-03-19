(defparameter *partial-tree* '(x 4 (9 8 x x)))
(defparameter *blank-tree* '(x x (x x (x x) x)))

(defun von-koch-construction (blank-tree)
  "Given an unenumerated tree of N nodes, returns a tree of N nodes and N-1
edges, with nodes enumerated 1 to N and edges 1 to N-1, such that each edge's
enumeration is equal to the difference between the nodes."
  (let ((n (num-nodes blank-tree)))
    (let ((nodes-left (loop for i from 1 to n collect i))
          (edges-left (loop for i from 1 to (1- n) collect i)))
    )))

(defun num-nodes (tree)
  "Determine the number of nodes in an arbitrary tree."
  (labels ((num-descendents (node)
             (apply #'+ (cons (length (cdr node))
                              (mapcar #'num-descendents
                                      (remove-if #'atom (cdr node)))))))
    (1+ (num-descendents tree))))

(defun tree->dot (tree)
  (format t "~&graph{~%")
  (let ((anon-counter 0))
    (labels ((write-subnode (parent children &optional parent-name)
               (let ((parent-name (if parent-name
                                      parent-name
                                      (format nil
                                              "~a"
                                              (if (eq parent 'x)
                                                  (format nil
                                                          "anon~d" 
                                                          (incf anon-counter))
                                                  parent)))))
                 (format t "~&~a[label=\"~a\"];~%" parent-name parent)
                 (dolist (child children)
                   (let ((immediate-child (if (atom child)
                                              child
                                              (car child))))
                     (let ((child-name (if (eq immediate-child 'x)
                                           (format nil
                                                   "anon~d"
                                                   (incf anon-counter))
                                           immediate-child)))
                       (if (and (integerp parent) (integerp immediate-child))
                           (format t "~&~a--~a[label=\"~a\"];~%"
                                   parent-name child-name (abs (- parent immediate-child)))
                           (format t "~&~a--~a;~%"
                                   parent-name child-name))
                       (if (atom child) 
                           (format t "~&~a[label=\"~a\"];~%" child-name child)
                           (write-subnode immediate-child (cdr child) child-name))))))))
      (write-subnode (car tree) (cdr tree))))
  (format t "~&}~%"))


;;; check out this usage...
;; (dot->png "empty.dot" (lambda () (tree->dot '(x x (x x x x)))))
;; (dot->png "empty.dot" (lambda () (tree->dot '(x 4 (9 8 x x)))))


(defun next-von-koch-possibility (tree)
  )
