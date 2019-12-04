;;; day4 --- My solution to day4 -*-

;;; Commentary:
;; My solution to advent of code: day4

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "computer.lisp")

(defpackage :day4
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :computer)
  (:use :hash)
  (:use :iter))

(in-package :day4)

;; # PART 1:

(defun day4-part-1 (start end)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for i from start to end)
    (for pass = (format nil "~a" i))
    (count (meets-criteria pass))))

(defun meets-criteria (pass)
  (and (iter (for char in-string pass)
             (for p-char previous char initially nil)
             (when (eq char p-char)
               (return t)))
       (iter (for char in-string pass)
             (for n-char = (read-from-string (string char)))
             (for p-n-char previous n-char initially 0)
             (when (> p-n-char n-char)
               (return nil))
             (finally (return t)))))

;; # PART 2:

(defun day4-part-2 (start end)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for i from start to end)
    (for pass = (format nil "~a" i))
    (count (meets-other-criteria pass))))

(defun meets-other-criteria (pass)
  (and (iter (for char in-string pass)
             (for p-char previous char initially nil)
             (when (and (eq char p-char) (eq 2 (count-if (lambda (c) (eq c char)) pass)))
               (return t)))
       (iter (for char in-string pass)
             (for n-char = (read-from-string (string char)))
             (for p-n-char previous n-char initially 0)
             (when (> p-n-char n-char)
               (return nil))
             (finally (return t)))))

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
;; " expected-1 (day4-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day4-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ()
    (format t "
Part 1: ~s
" (day4-part-1 136818 685979))
    (format t "
Part 2: ~s
" (day4-part-2 136818 685979))))

