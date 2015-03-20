(defparameter *partial-tree* '(x ((4 ((9 ()) (x ()))) (6 ()) (x ()))))
(defparameter *simple-tree* '(x ((x ()) (x ()))))
(defparameter *blank-tree* '(x ((x ((x ()) (x ()))) (x ()) (x ()))))

(defun von-koch-construction (blank-tree)
  "Given an unenumerated tree of N nodes, returns a tree of N nodes and N-1
edges, with nodes enumerated 1 to N and edges 1 to N-1, such that each edge's
enumeration is equal to the difference between the nodes."
  (let ((n (num-nodes blank-tree)))
    (let ((nodes-left (loop for i from 1 to n collect i))
          (edges-left (loop for i from 1 to (1- n) collect i)))
    )))

;; (defun num-descendents (node)
;;   "Determine the number of descendent nodes of a node (or tree)."
;;   (apply #'+ (cons (length (cdr node))
;;                    (mapcar #'num-descendents
;;                            (remove-if #'atom (cdr node))))))

(defun normalize-tree-shape (tree)
  ;; i don't actually think i need to worry about this one just yet
  (error "not implemented"))

(defparameter *anon-counter* 0)

(defun get-node-gname (node)
  (if (eq (car node) 'x)
      (format nil "anon~d" (incf *anon-counter*))
      (car node)))

(defun node->dot (node &optional node-name)
  (let ((node-name (if (null node-name)
                       (get-node-gname node)
                       node-name)))
    (format t "~&~a[label=\"~a\"];~%" node-name (car node))
    (dolist (child (cadr node))
      (let ((child-name (get-node-gname child)))
        (if (and (integerp node-name) (integerp child-name))
            (format t "~&~a--~a[label=\"~a\"];~%"
                    node-name child-name (abs (- node-name child-name)))
            (format t "~&~a--~a;~%"
                    node-name child-name))
        (node->dot child child-name)))))

(defun tree->dot (root-node)
  (let ((*anon-counter* 0))
    (format t "~&graph{~%")
    (node->dot root-node)
    (format t "~&}~%")))
  

(defun next-von-koch-possibility (tree)
  )
