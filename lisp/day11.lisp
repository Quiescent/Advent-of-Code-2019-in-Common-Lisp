;;; day11 --- My solution to day11 -*-

;;; Commentary:
;; My solution to advent of code: day11

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "halting-computer.lisp")

(defpackage :day11
  (:use :common-lisp)
  (:use :halting-computer)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day11)

;; # PART 1:

(defun day11-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((program (parse-computer-registers input-elements))
        (*relative-base* 0)
        (direction 'north)
        (grid (make-hash-table :test #'equal)))
    (length (remove-duplicates (iter
                                 (for i from 0 below 100000)
                                 (with x = 0)
                                 (with y = 0)
                                 (with ptr = 0)
                                 (for current-panel-colour = (gethash (cons x y) grid 0)) ; 1 is white
                                 (multiple-value-bind (new-ptr colour halted) (interpret program current-panel-colour ptr)
                                   (when halted
                                     (print "BAILING!")
                                     (return result))
                                   (setf ptr new-ptr)
                                   (multiple-value-bind (new-new-ptr turn halted) (interpret program current-panel-colour ptr)
                                     (when halted
                                       (print "BAILING!")
                                       (return result))
                                     (setf direction (turn-from turn direction))
                                     (setf ptr new-new-ptr)
                                        ;(format t "colour: ~a, turn: ~a~%" colour turn)
                                     (setf (gethash (cons x y) grid) colour)
                                     ))
                                 (case direction
                                   (north (decf y))
                                   (south (incf y))
                                   (east  (incf x))
                                   (west  (decf x)))
                                 (adjoining (cons x y) into result))
                               :test #'equal))))

;; Wrong: 9461

(defun turn-from (turn direction)
  (case turn
    (0 (case direction
         (north 'west)
         (west  'south)
         (south 'east)
         (east  'north)))
    (1 (case direction
         (north 'east)
         (east  'south)
         (south 'west)
         (west  'north)))))

;; # PART 2:

(defun day11-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((program (parse-computer-registers input-elements))
        (*relative-base* 0)
        (direction 'north)
        (grid (make-hash-table :test #'equal)))
    (iter
      (for i from 0 below 100000)
      (with x = 0)
      (with y = 0)
      (with ptr = 0)
      (for current-panel-colour = (gethash (cons x y) grid 0)) ; 1 is white
      (initially
       (setf (gethash (cons x y) grid) 1)
       (setf current-panel-colour 1))
      (multiple-value-bind (new-ptr colour halted) (interpret program current-panel-colour ptr)
        (when halted
          (print "BAILING!")
          (return))
        (setf ptr new-ptr)
        (multiple-value-bind (new-new-ptr turn halted) (interpret program current-panel-colour ptr)
          (when halted
            (print "BAILING!")
            (return))
          (setf direction (turn-from turn direction))
          (setf ptr new-new-ptr)
          (setf (gethash (cons x y) grid) colour)))
      (case direction
        (north (decf y))
        (south (incf y))
        (east  (incf x))
        (west  (decf x))))
    (format t "~%")
    (multiple-value-bind (min-x min-y max-x max-y)
        (iter
          (for (key value) in-hashtable grid)
          (for (x . y) = key)
          (minimizing x into min-x)
          (minimizing y into min-y)
          (maximizing x into max-x)
          (maximizing y into max-y)
          (finally (return (values min-x min-y max-x max-y))))
      (iter
        (for y from min-y to max-y)
        (iter
          (for x from min-x to max-x)
          (format t "~a" (case (gethash (cons x y) grid 0)
                           (0 #\_)
                           (1 #\#))))
        (format t "~%")))))

;; Wrong: FKFKAFRK

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-1 ())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day11-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day11-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day11-part-1"))
        (input-2 (file-lines "day11-part-1")))
    (format t "
Part 1: ~s
" (day11-part-1 input-1))
    (format t "
Part 2: ~s
" (day11-part-2 input-2))))

