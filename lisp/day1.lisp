;;; day1 --- My solution to day1 -*-

;;; Commentary:
;; My solution to advent of code: day1

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day1
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day1)

;; # PART 1:

(defun day1-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for (mass) in input-elements)
    (sum (- (floor mass 3) 2))))

;; # PART 2:

(defun zero-if-negative (x)
  (if (< x 0) 0 x))

(defun day1-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for (mass) in input-elements)
    (sum (iter
           (with current-mass = (zero-if-negative (- (floor mass 3) 2)))
           (sum current-mass into total-mass)
           (when (<= current-mass 0)
             (return total-mass))
           (setf current-mass
                 (zero-if-negative (- (floor current-mass 3) 2)))))))

;; wrong: 4915968

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '())
        (expected-1 '())
        (input-2 '((1969)))
        (expected-2 966))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day1-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day1-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines-numbers "day1-part-1"))
        (input-2 (file-lines-numbers "day1-part-1")))
    (format t "
Part 1: ~s
" (day1-part-1 input-1))
    (format t "
Part 2: ~s
" (day1-part-2 input-2))))

