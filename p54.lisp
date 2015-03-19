(defun istree (structure)
  (and (nthcdr 2 structure) ; length at least 3
       (not (nthcdr 3 structure)) ; length less than 4
       (symbolp (first structure))
       (or (null (second structure))
           (istree (second structure)))
       (or (null (third structure))
           (istree (third structure)))))
