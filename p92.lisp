(defparameter *partial-tree* '(x ((4 ((9 nil) (x nil))) (6 nil) (x nil))))
(defparameter *simple-tree* '(x ((x nil) (x nil))))
(defparameter *blank-tree* '(x ((x ((x nil) (x nil))) (x nil) (x nil))))
(defparameter *filled-tree* '(3 ((1 nil) (2 nil))))

(defun von-koch-construction (blank-tree)
  "Given an unenumerated tree of N nodes, returns a tree of N nodes and N-1
edges, with nodes enumerated 1 to N and edges 1 to N-1, such that each edge's
enumeration is equal to the difference between the nodes."
  (let ((n (num-nodes blank-tree)))
    (let ((nodes-left (loop for i from 1 to n collect i))
          (edges-left (loop for i from 1 to (1- n) collect i)))
    )))

(defun num-nodes (root-node)
  "Determine the number of nodes in a tree (or subtree)."
  (1+ (reduce #'+ (mapcar #'num-nodes (cadr root-node)))))

(defun nested-list-copy (structure)
  "Creates deep[er] copy of a nested list. End result, no shared cons cells
between the original structure and the copy."
  (when structure
    (if (atom (car structure))
        (cons (car structure)
              (nested-list-copy (cdr structure)))
        (cons (nested-list-copy (car structure))
              (nested-list-copy (cdr structure)))))

(defun find-node (node key-fn)
  "Find the first node in a tree (or subtree) that KEY-FN returns non-nil."
  (if (funcall key-fn node)
      node
      (do* ((children (cadr node) (cdr children))
            (child-result (find-node (car children) key-fn)
                          (find-node (car children) key-fn)))
           (child-result child-result))))

(defun normalize-tree-shape (tree)
  ;; i don't actually think i need to worry about this one just yet
  (error "not implemented"))

(defparameter *anon-counter* 0)

(defun get-node-graph-name (node)
  (if (eq (car node) 'x)
      (format nil "anon~d" (incf *anon-counter*))
      (car node)))

(defun node->dot (node &optional node-name)
  (let ((node-name (if (null node-name)
                       (get-node-graph-name node)
                       node-name)))
    (format t "~&~a[label=\"~a\"];~%" node-name (car node))
    (dolist (child (cadr node))
      (let ((child-name (get-node-graph-name child)))
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

(defun next-von-koch-possibility (tree nodes-left edges-left)
  (let (possibilities)
    (let ((blank (find-node tree #'(lambda (x) (eq (car x) 'x)))))
