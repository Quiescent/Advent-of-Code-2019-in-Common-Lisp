;;; day5 --- My solution to day5 -*-

;;; Commentary:
;; My solution to advent of code: day5

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")
(ql:quickload "str")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "computer.lisp")

(defpackage :day5
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :computer)
  (:use :hash)
  (:use :iter))

(in-package :day5)

;; # PART 1:

(defun day5-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (interpret (map 'vector #'read-from-string (str:words (ppcre:regex-replace "," (car input-elements) " ")))))

;; Wrong: 223
;; Correct: 7839346

;; # PART 2:

(defun day5-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (interpret (map 'vector #'read-from-string (str:words (ppcre:regex-replace "," (car input-elements) " ")))))

;; Wrong: 5687357
;; Correct: 447803

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
;; " expected-1 (day5-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day5-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day5-part-1"))
        (input-2 (file-lines "day5-part-1")))
    (format t "
Part 1: ~s
" (day5-part-1 input-1))
    (format t "
Part 2: ~s
" (day5-part-2 input-2))))

