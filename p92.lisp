(defparameter *partial-tree* '(x ((4 ((9 nil) (x nil))) (6 nil) (x nil))))
(defparameter *simple-tree* '(x ((x nil) (x nil))))
(defparameter *blank-tree* '(x ((x ((x nil) (x nil))) (x nil) (x nil))))
(defparameter *filled-tree* '(3 ((1 nil) (2 nil))))
(defparameter *bigger-tree* '(x ((x ((x ((x ((x ((x nil) (x nil))) (x nil) (x nil))))) (x nil))) (x nil) (x nil))))
(defparameter *giant-tree* '(x ((x ((x ((x ((x ((x nil) (x nil))) (x nil) (x nil))))) (x nil))) (x ((x ((x ((x nil) (x nil))) (x ((x ((x ((x nil) (x nil))) (x nil) (x nil))))) (x nil))))) (x nil))))

(defun von-koch-construction (blank-tree &optional verbose)
  "Given an unenumerated tree of N nodes, returns a tree of N nodes and N-1
edges, with nodes enumerated 1 to N and edges 1 to N-1, such that each edge's
enumeration is equal to the difference between the nodes."
  (let* ((round 0)
         (n (num-nodes blank-tree))
         (states (list (list blank-tree (loop for i from 1 to n collect i)))))
    (dotimes (i n)
      (let (new-states)
        (dolist (state states)
          (setf new-states (nconc (next-von-koch-states state) new-states)))
        (when verbose (format t "~&Round ~d: ~d states became ~d states.~%"
                (incf round) (length states) (length new-states)))
        (setf states new-states)))
    (caar states)))

(defun next-von-koch-states (state)
  (let ((tree (first state))
        (values-left (second state)))
    (labels ((assign (value)
               (let* ((copy (nested-list-copy tree))
                      (blank-node (find-node copy (lambda (x)
                                                    (eq (car x) 'x)))))
                 (setf (car blank-node) value)
                 copy)))
      (remove-if-not #'von-koch-state-valid-p
                     (mapcar #'(lambda (x) (list (assign x)
                                                 (remove x values-left)))
                             values-left)))))

(defun von-koch-state-valid-p (von-koch-state)
  "Returns true if TREE does not have duplicate edge values."
  (let* ((tree (first von-koch-state))
         (n (num-nodes tree))
         (edge-values-remaining (loop for i from 1 to (1- n) collect i)))
    (labels ((inspect-edges (node)
               (dolist (child (cadr node))
                 (when (and (integerp (car node)) (integerp (car child)))
                   (let ((edge-value (abs (- (car node) (car child)))))
                     (if (member edge-value edge-values-remaining)
                         (setf edge-values-remaining
                               (remove edge-value edge-values-remaining))
                         (return-from von-koch-state-valid-p nil))))
                 (inspect-edges child))))
      (inspect-edges tree)
      t)))

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
              (nested-list-copy (cdr structure))))))

(defun find-node (node key-fn)
  "Find the first node in a tree (or subtree) that KEY-FN returns non-nil."
  (if (funcall key-fn node)
      node
      (dolist (child (cadr node))
        (let ((child-result (find-node child key-fn)))
          (when child-result (return child-result))))))

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
