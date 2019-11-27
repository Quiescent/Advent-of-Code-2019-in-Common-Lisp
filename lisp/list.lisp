(defun list-less-than-or-equal (xs ys)
  "Produce t if the elements of XS are less than those of YS earlier.

i.e. what one would think of as one list being less than another."
  (let ((zipped  (mapcar #'cons xs ys))
        (longest (max (length xs) (length ys))))
    (labels ((less    (a) (< (car a) (cdr a)))
             (greater (a) (> (car a) (cdr a))))
      (<= (or (position-if #'less    zipped) longest)
          (or (position-if #'greater zipped) longest)))))
