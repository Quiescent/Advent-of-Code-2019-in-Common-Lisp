;;; day9 --- My solution to day9 -*-

;;; Commentary:
;; My solution to advent of code: day9

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "computer.lisp")

(defpackage :day9
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :computer)
  (:use :hash)
  (:use :iter))

(in-package :day9)

;; # PART 1:

(defun day9-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((program (map 'vector
                  #'read-from-string
                  (str:words (ppcre:regex-replace-all ","
                                                      (car input-elements)
                                                      " "))))
        (bigger-program (make-array '(1000000) :initial-element 0))
        (*relative-base* 0))
    (iter (for i from 0 below (length program))
          (setf (aref bigger-program i)
                (aref program        i)))
    (interpret bigger-program)))

;; Wrong: 203

;; # PART 2:

(defun day9-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((program (map 'vector
                      #'read-from-string
                      (str:words (ppcre:regex-replace-all ","
                                                          (car input-elements)
                                                          " "))))
        (bigger-program (make-array '(1000000) :initial-element 0))
        (*relative-base* 0))
    (iter (for i from 0 below (length program))
          (setf (aref bigger-program i)
                (aref program        i)))
    (interpret bigger-program)))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
        (expected-1 nil))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day9-part-1 input-1))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day9-part-1"))
        (input-2 (file-lines "day9-part-1")))
    (format t "
Part 1: ~s
" (day9-part-1 input-1))
    (format t "
Part 2: ~s
" (day9-part-2 input-2))))

