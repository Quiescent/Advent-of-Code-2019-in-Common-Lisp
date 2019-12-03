(ql:quickload "iterate")

(defpackage :computer
  (:use :common-lisp)
  (:use :iter)
  (:export interpret)
  (:export parse-computer-registers))

(in-package :computer)

(defun parse-computer-registers (lines-numbers)
  (map 'vector #'identity (car lines-numbers)))

(defun interpret (array)
  (iter
    (with ptr = 0)
    (when (or (> ptr (length array))
              (< ptr 0))
      (error (format nil "Pointer: ~a is out of bounds!" ptr)))
    (for op   = (aref array (+ ptr 0)))
    (for p1   = (aref array (+ ptr 1)))
    (for p2   = (aref array (+ ptr 2)))
    (for dest = (aref array (+ ptr 3)))
    (cond
      ((eq op 99) (return))
      ((eq op 1)  (setf (aref array dest) (+ (aref array p1)
                                             (aref array p2))
                        ptr               (+ ptr 4)))
      ((eq op 2)  (setf (aref array dest) (* (aref array p1)
                                             (aref array p2))
                        ptr               (+ ptr 4))))))
