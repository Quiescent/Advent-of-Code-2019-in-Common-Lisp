;;; day8 --- My solution to day8 -*-

;;; Commentary:
;; My solution to advent of code: day8

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day8
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day8)

;; # PART 1:

(defun day8-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for chunk in (chunk (car input-elements)))
    (finding (* (count-if (lambda (c) (char-equal c #\1)) chunk)
                (count-if (lambda (c) (char-equal c #\2)) chunk))
             minimizing (count-if (lambda (c) (char-equal c #\0)) chunk))))

(defun chunk (str)
  (iter
   (with current = (copy-seq str))
   (while (> (length current) 0))
   (for layer = (subseq current 0 (* 25 6)))
   (setf current (subseq current (* 25 6)))
   (collect layer)))

;; Wrong: 2332

;; # PART 2:

(defun day8-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (with image  = (make-array (list (* 25 6)) :initial-element #\2))
    (with layers = (map 'vector #'identity (chunk (car input-elements))))
    (with i = 0)
    (for y from 0 below 6)
    (iter
      (for x from 0 below 25)
      (setf (aref image i)
            (iter
              (for layer in-vector layers)
              (for pixel = (aref layer i))
              (when (not (eq #\2 pixel))
                (return pixel))
              (finally (return nil))))
      (incf i))
    (finally (iter (for y from 0 below 6)
                   (iter (for x from 0 below 25)
                         (format t "~a" (case (aref image (+ x (* y 25)))
                                          (#\0 #\_)
                                          (#\1 #\#)
                                          (#\2 #\~))))
                   (format t "~%")))))

;; (iter
;;     (with image = (make-array '(25 6) :initial-element #\2))
;;     (for layer in (chunk (car input-elements)))
;;     (iter
;;       (for y from 0 below 6)
;;       (iter
;;         (for x from 0 below 25)
;;         (when (and (eq (aref image x y) #\2)
;;                    (not (eq (aref layer (+ x (* 25 y))) #\2)))
;;           ;(print (cons x y))
;;           (setf (aref image x y)
;;                 (aref layer (+ x (* 25 y)))))))
;;     (finally (iter
;;                (for y from 0 below 6)
;;                (iter
;;                  (for x from 0 below 25)
;;                  (format t "~a" (case (aref image x y)
;;                                   (#\0 #\#)
;;                                   (#\1 #\_)
;;                                   (#\2 #\~))))
;;                (format t "~%"))))

;; (iter
;;     (with image = (make-array '(25 6) :initial-element #\~))
;;     (for chunk in (chunk (car input-elements)))
;;     (iter
;;       (for y from 0 below 6)
;;       (iter
;;         (for x from 0 below 25)
;;         (for new-colour = (case (aref chunk (+ x (* y 25)))
;;                             (#\0 #\#)
;;                             (#\1 #\_)
;;                             (#\2 #\~)))
;;         (for cur-colour = (aref image x y))
;;         (when (eq cur-colour #\~)
;;           (setf (aref image x y) new-colour))))
;;     (finally (iter (for y from 0 below 6)
;;                    (iter (for x from 0 below 25)
;;                          (format t "~a" (aref image x y)))
;;                    (format t "~%"))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '())
        (expected-1 '())
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day8-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day8-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day8-part-1"))
        (input-2 (file-lines "day8-part-1")))
    (format t "
Part 1: ~s
" (day8-part-1 input-1))
    (format t "
Part 2: ~s
" (day8-part-2 input-2))))

