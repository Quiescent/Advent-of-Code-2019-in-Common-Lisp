;;; day3 --- My solution to day3 -*-

;;; Commentary:
;; My solution to advent of code: day3

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")
(ql:quickload "str")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "computer.lisp")

(defpackage :day3
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :computer)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day3)

;; # PART 1:

(defun day3-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((directions (parse-directions input-elements))
        (lines-crossed-a (make-hash-table :test #'equal)))
    (iter
      (with x = 0)
      (with y = 0)
      (for (direction . count) in (car directions))
      (iter
        (for i from 0 below count)
        (setf (gethash (cons x y) lines-crossed-a) t)
        (case direction
          (#\U (decf y))
          (#\D (incf y))
          (#\L (decf x))
          (#\R (incf x)))))
    (iter outer
          (with x = 0)
          (with y = 0)
          (for (direction . count) in (cadr directions))
          (minimizing
           (iter
             (for i from 0 below count)
             (when (and (gethash (cons x y) lines-crossed-a)
                        (not (equal (cons x y)
                                    '(0 . 0))))
               (finding (+ (abs x) (abs y)) minimizing (+ (abs x) (abs y)) into answer))
             (case direction
               (#\U (decf y))
               (#\D (incf y))
               (#\L (decf x))
               (#\R (incf x)))
             (finally (return (or answer most-positive-fixnum))))))))

;; 489

(defun parse-directions (lines)
  (mapcar #'parse-line lines))

(defun parse-line (line)
  (mapcar (lambda (word)
            (cons (aref word 0)
                  (read-from-string (subseq word 1))))
          (str:words (ppcre:regex-replace-all "," line " "))))

;; # PART 2:

(defun day3-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((directions (parse-directions input-elements))
        (lines-crossed-a (make-hash-table :test #'equal)))
    (iter
      (with steps = 0)
      (with x = 0)
      (with y = 0)
      (for (direction . count) in (car directions))
      (iter
        (for i from 0 below count)
        (when (null (gethash (cons x y) lines-crossed-a))
          (setf (gethash (cons x y) lines-crossed-a) steps))
        (case direction
          (#\U (decf y))
          (#\D (incf y))
          (#\L (decf x))
          (#\R (incf x)))
        (incf steps)))
    (iter outer
          (with steps = 0)
          (with x = 0)
          (with y = 0)
          (for (direction . count) in (cadr directions))
          (minimizing
           (iter
             (for i from 0 below count)
             (when (and (gethash (cons x y) lines-crossed-a)
                        (not (equal (cons x y)
                                    '(0 . 0))))
               (finding (+ (gethash (cons x y) lines-crossed-a) steps)
                        minimizing (+ (gethash (cons x y) lines-crossed-a) steps)
                        into answer))
             (case direction
               (#\U (decf y))
               (#\D (incf y))
               (#\L (decf x))
               (#\R (incf x)))
             (incf steps)
             (finally (return (or answer most-positive-fixnum))))))))

;; Answer: 93654

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("R8,U5,L5,D3"
                   "U7,R6,D4,L4"))
        (expected-1 '())
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day3-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day3-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day3-part-1"))
        (input-2 (file-lines "day3-part-1")))
    (format t "
Part 1: ~s
" (day3-part-1 input-1))
    (format t "
Part 2: ~s
" (day3-part-2 input-2))))

