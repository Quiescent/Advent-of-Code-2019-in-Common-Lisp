;;; day19 --- My solution to day19 -*-

;;; Commentary:
;; My solution to advent of code: day19

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "halting-computer.lisp")

(defpackage :day19
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :halting-computer)
  (:use :iter))

(in-package :day19)

;; # PART 1:

(declaim (optimize speed))

(defun day19-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((program (parse-computer-registers input-elements))
         (program-copy (parse-computer-registers input-elements))
         (*relative-base* 0)
         (effected 0)
         (grid (make-hash-table :test #'equal)))
    (declare (type fixnum effected))
    (declare (type fixnum *relative-base*))
    (iter
      (for y from 0 below 50)
      (declare (type fixnum y))
      (iter
        (iter
          (for i from 0 below 10000000)
          (declare (type fixnum i))
          (setf (aref program-copy i) (aref program i)))
        (for ptr = 0)
        (declare (type fixnum ptr))
        (for x from 0 below 50)
        (declare (type fixnum x))
        (multiple-value-bind (new-ptr) (interpret program-copy x ptr)
          (setf ptr new-ptr))
        (multiple-value-bind (new-ptr output) (interpret program-copy y ptr)
          (declare (type fixnum output))
          (setf ptr new-ptr)
          (setf (gethash (cons x y) grid) output)
          (when (eq output 1)
            (incf effected)))))
    (print-grid grid)
    effected))

(defun print-grid (grid)
  (multiple-value-bind (min-x max-x min-y max-y) (iter
                                                   (for (key value) in-hashtable grid)
                                                   (for (x . y) = key)
                                                   (minimizing x into min-x)
                                                   (maximizing x into max-x)
                                                   (minimizing y into min-y)
                                                   (maximizing y into max-y)
                                                   (finally (return (values min-x max-x min-y max-y))))
    (format t "~a ~a ~a ~a~%" min-x max-x min-y max-y)
    (format t "~%")
    (iter
      (for y from min-y to max-y)
      (iter
        (for x from min-x to max-x)
        (for tile = (gethash (cons x y) grid))
        (format t "~a" tile))
      (format t "~%"))))

;; # PART 2:
(defun day19-part-2 (program-input input-elements)
  "Run my solution to part two of the problem on the input in PROGRAM-INPUT and INPUT-ELEMENTS."
  (let* ((program (parse-computer-registers program-input))
         (program-copy (parse-computer-registers program-input))
         (*relative-base* 0)
         (effected 0)
         (grid (make-hash-table :test #'equal)))
    (declare (type fixnum effected))
    (declare (type fixnum *relative-base*))
    (labels ((works-at (x y)
               (let ((ptr 0))
                 (iter
                   (for i from 0 below 10000000)
                   (declare (type fixnum i))
                   (setf (aref program-copy i) (aref program i)))
                 (multiple-value-bind (new-ptr) (interpret program-copy x ptr)
                   (setf ptr new-ptr))
                 (multiple-value-bind (new-ptr output) (interpret program-copy y ptr)
                   (eq output 1)))))
      ;; (4493, 3266) Found by running for a while...
      ;; (5825, 4234)
      ;; (6472, 4704)
      (let* ((x 12)
             (x-bottom 12)
             x-bottoms
             (y 9)
             (y-bottom 10)
             y-bottoms
             (x-pattern (map 'vector #'identity '(1 1 2 1 1 2 1 2)))
             (x-pattern-bottom (map 'vector #'identity '(1 1 1 1 1 1 1 1 1 2)))
             (x-len     (length x-pattern))
             ;; (y-pattern (map 'vector #'identity '(1))) (always go down 1...)
             )
        (iter
          (format t "Current: (~a, ~a)~%" x y)
          (with pos = 0)
          (when (and (member (- x 100) x-bottoms)
                     (member (- y 100) y-bottoms)
                     ;(works-at (- x 100) (- y 100))
                     )
            (format t "Closest point: (~a, ~a)~%" (- x 100) y)
            (return))
          (iter
            (for j from 0 below (aref x-pattern pos))
            (incf x))
          (iter
            (for ))
          (incf pos)
          (setf pos (mod pos x-len))
          (incf y))))))

;; Wrong: 9580860

;; Wrong: 1058

(defun bounds-of-beam (grid-lines)
  (iter
    (with top-x = 0)
    (with top-y = 0)
    (with bot-x = 0)
    (with bot-y = 0)
    (for y from 0)
    (for line in grid-lines)
    (iter
      (for x from 0)
      (for tile in-string line)
      (when (eq tile #\1)
        (when (> y bot-y)
          (setf bot-y y)
          (setf bot-x x))
        (when (> x top-x)
          (setf top-y y)
          (setf top-x x))))
    (finally (return (values bot-x bot-y top-x top-y)))))

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
;; " expected-1 (day19-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day19-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day19-part-1"))
        (input-2 (file-lines "day19-part-2")))
    ;; (format t "
;; Part 1: ~s
;; " (day19-part-1 input-1))
    (format t "
Part 2: ~s
" (day19-part-2 input-1 input-2))))

