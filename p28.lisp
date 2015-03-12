(deftest example
    (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
  ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L)))

(defun lsort (list)
  (sort (copy-seq list) #'< :key #'length))

;; run tests twice--assure non-destructiveness
(do-tests) (do-tests)

;;; Pascal Bourguignon gave a similar answer to the one above but also posted
;;; another purportedly better solution:

;; Simple direct solution, using Common Lisp, taking care of calling length only
;; once [otherwise, as in the first example, sort may call the key multiple
;; times, which may be expensive]. Moreover, we take care of using a vector as
;; temporary structure, so that we use less memory (and sorting vectors may be
;; faster):

(defun lsort-bourguignon (llist)
  (map 'list (function cdr)
       (sort (map 'vector (lambda (list) (cons (length list) list)) llist)
             (function <) :key (function car))))

;;; However, so-called efficiency gains are dependent upon input (size), so
;;; watch out. In this case (a short list of short lists) extra calls to the
;;; sorting key (length) create less extra work than the overhead of doing it
;;; the fancy way.

(defparameter aaaa '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))

;; CL-USER> (time (dotimes (i 10000000) (lsort aaaa)))
;; Evaluation took:
;;   6.871 seconds of real time
;;   6.848444 seconds of total run time (6.832844 user, 0.015600 system)
;;   [ Run times consist of 0.219 seconds GC time, and 6.630 seconds non-GC time. ]
;;   99.67% CPU
;;   23,311,664,557 processor cycles
;;   5,280,017,792 bytes consed

;; CL-USER> (time (dotimes (i 10000000) (lsort-bourguignon aaaa)))
;; Evaluation took:
;;   10.833 seconds of real time
;;   10.795269 seconds of total run time (10.795269 user, 0.000000 system)
;;   [ Run times consist of 0.454 seconds GC time, and 10.342 seconds non-GC time. ]
;;   99.65% CPU
;;   36,749,723,467 processor cycles
;;   9,120,026,816 bytes consed

(defun shuffle (list)
  (let* (new-order
         (len (length list))
         (indicies-remaining (range 0 len)))
    (dotimes (i len new-order)
      (let ((next (nth (random (- len i)) indicies-remaining)))
        (setf indicies-remaining (delete next indicies-remaining))
        (push next new-order)))
    (mapcar #'(lambda (x) (elt list x)) new-order)))

(defparameter longer-list (shuffle (loop for i from 1 to 1000
                                      collect (range 1 (random 10000)))))

;;; ... and now the performance boost kicks in:

;; CL-USER> (time (progn (dotimes (i 100) (lsort longer-list)) nil))
;; Evaluation took:
;;   7.731 seconds of real time
;;   7.706450 seconds of total run time (7.706450 user, 0.000000 system)
;;   99.68% CPU
;;   26,225,083,904 processor cycles
;;   30,933,904 bytes consed

;; CL-USER> (time (progn (dotimes (i 100) (lsort-bourguignon longer-list)) nil))
;; Evaluation took:
;;   0.976 seconds of real time
;;   0.967206 seconds of total run time (0.967206 user, 0.000000 system)
;;   99.08% CPU
;;   3,309,930,927 processor cycles
;;   58,046,704 bytes consed
