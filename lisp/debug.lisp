(ql:quickload "iterate")

(defpackage :debug
  (:use :common-lisp)
  (:use :iter)
  (:export debug-hashtable)
  (:export debug-matrix))

(in-package :debug)

(defun debug-hashtable (table)
  (format nil
          "(~{~a~^
~})"
          (let ((key-values))
            (maphash (lambda (key value) (push (cons key value) key-values))
                     table)
            key-values)))

(defun debug-matrix (matrix)
  (destructuring-bind (x-dim y-dim) (array-dimensions matrix)
    (format nil "
~{~a
~}" (iter
      (for y from 0 below y-dim)
      (collect
          (apply #'concatenate
                 'string
                 (iter
                   (for x from 0 below x-dim)
                   (collect (format nil "~5d" (aref matrix x y))))))))))
