;;; day2 --- My solution to day2 -*-

;;; Commentary:
;; My solution to advent of code: day2

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day2
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day2)

;; # PART 1:

(defun day2-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((program (map 'vector #'identity (car input-elements))))
    (setf (aref program 1) 12)
    (setf (aref program 2) 2)
    (interpret program)
    (aref program 0)))

;; Wrong: 1

(defun interpret (array)
  (iter
    (for i from 0 below (length array) by 4)
    (for op   = (aref array (+ i 0)))
    (for p1   = (aref array (+ i 1)))
    (for p2   = (aref array (+ i 2)))
    (for dest = (aref array (+ i 3)))
    (cond
      ((eq op 99) (return))
      ((eq op 1)  (setf (aref array dest)
                        (+ (aref array p1)
                           (aref array p2))))
      ((eq op 2)  (setf (aref array dest)
                        (* (aref array p1)
                           (aref array p2)))))))

;; # PART 2:

(defun day2-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((program (map 'vector #'identity (car input-elements))))
    (iter outer
      (for noun from 0 below (length program))
      (iter
        (for verb from 0 below (length program))
        (for program-copy = (map 'vector #'identity program))
        (setf (aref program-copy 1) noun)
        (setf (aref program-copy 2) verb)
        (interpret program-copy)
        (when (eq 19690720 (aref program-copy 0))
          (print program-copy)
          (return-from outer (+ (* 100 noun) verb)))))))

;; wrong: 121000
;; wrong: 2255

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day2-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day2-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day2-part-1"))
        (input-2 (file-lines-numbers "day2-part-1")))
    (format t "
Part 1: ~s
" (day2-part-1 input-1))
    (format t "
Part 2: ~s
" (day2-part-2 input-2))))

