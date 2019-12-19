;;; day19 --- My solution to day19 -*-

;;; Commentary:
;; My solution to advent of code: day19

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "hash-table-computer.lisp")

(defpackage :day19
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :hash-table-computer)
  (:use :iter))

(in-package :day19)

;; # PART 1:

(declaim (optimize speed))

(defun day19-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((program (parse-computer-registers input-elements))
         (effected 0)
         (grid (make-hash-table :test #'equal)))
    (declare (type fixnum effected))
    (declare (type fixnum *relative-base*))
    (iter
      (for y from 0 below 50)
      (declare (type fixnum y))
      (iter
        (for program-copy = (copy-computer program))
        (for *relative-base* = 0)
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
         (x 4)
         (y 3))
    (labels ((works-at (x y)
               (let ((ptr 0)
                     (*relative-base* 0)
                     (program-copy (copy-computer program)))
                 (multiple-value-bind (new-ptr) (interpret program-copy x ptr)
                   (setf ptr new-ptr))
                 (multiple-value-bind (new-ptr output) (interpret program-copy y ptr)
                   (eq output 1)))))
      ;; (6519, 4738) <- best with brute force
      ;; (4493, 3266) Found by running for a while...
      ;; (5825, 4234)
      ;; (6472, 4704)
      ;; (32566, 23685)
      ;; (2804664, 2038248)
      ;; (works-at 3280 2384)
      ;; (works-at (- 3280 10) (+ 2384 10))
      (multiple-value-bind (result-x result-y)
          (iter outer
            (for i from 0 below 100000)
            (format t "Current: (~a, ~a)~%" x y)
            (when (and (> (- x 99) 0)
                       (> (- y 99) 0)
                       (works-at x y)
                       (works-at (- x 99) (+ y 99)))
              (format t "Closest point: (~a, ~a)~%" (- x 99) y)
              (return-from outer (values x y)))
            (iter
              (incf x)
              (when (and (> (- x 99) 0)
                         (> (- y 99) 0)
                         (works-at (- x 99) (+ y 99))
                         (works-at x y))
                (format t "Closest point: (~a, ~a)~%" (- x 99) y)
                (return-from outer (values x y)))
              (while (works-at x y)))
            (iter
              (incf y)
              (while (not (works-at x y)))))
        (let ((min-along-x (iter
                             (for sliding-x from result-x downto (- result-x 99))
                             (finding (cons sliding-x result-y) minimizing (sqrt (+ (* result-y result-y)
                                                                                    (* sliding-x sliding-x))))))
              (min-along-y (iter
                             (for sliding-y from result-y to (+ result-y 99))
                             (finding (cons result-x sliding-y) minimizing (sqrt (+ (* sliding-y sliding-y)
                                                                                    (* result-x  result-x)))))))
          (if (< (sqrt (+ (* (car min-along-x) (car min-along-x))
                          (* (cdr min-along-x) (cdr min-along-x))))
                 (sqrt (+ (* (car min-along-y) (car min-along-y))
                          (* (cdr min-along-y) (cdr min-along-y)))))
              (+ (* 10000 (car min-along-x)) (cdr min-along-x))
              (+ (* 10000 (car min-along-y)) (cdr min-along-y))))))))

;; Wrong:    9580860
;; Too high: 10590770
;; Wrong:    10580769
;; Wrong:    9580769
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
    (format t "
Part 1: ~s
" (day19-part-1 input-1))
    (format t "
Part 2: ~s
" (day19-part-2 input-1 input-2))))

