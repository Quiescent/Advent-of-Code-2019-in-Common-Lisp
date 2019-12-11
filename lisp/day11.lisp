;;; day11 --- My solution to day11 -*-

;;; Commentary:
;; My solution to advent of code: day11

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day11
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day11)

;; # PART 1:

(defun day11-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((program (map 'vector
                      #'read-from-string
                      (str:words (ppcre:regex-replace-all ","
                                                          (car input-elements)
                                                          " "))))
        (bigger-program (make-array '(1000000) :initial-element 0))
        (*relative-base* 0)
        (direction 'north)
        (grid (make-hash-table :test #'equal)))
    (iter (for i from 0 below (length program))
          (setf (aref bigger-program i)
                (aref program        i)))
    (length (remove-duplicates (iter
                                 (for i from 0 below 100000)
                                 (with x = 0)
                                 (with y = 0)
                                 (with ptr = 0)
                                 (for current-panel-colour = (gethash (cons x y) grid 0)) ; 1 is white
                                 (multiple-value-bind (new-ptr colour halted) (interpret bigger-program current-panel-colour ptr)
                                   (when halted
                                     (print "BAILING!")
                                     (return result))
                                   (setf ptr new-ptr)
                                   (multiple-value-bind (new-new-ptr turn halted) (interpret bigger-program current-panel-colour ptr)
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

(defun arg-1-mode (x)
  (case (floor (mod x 1000) 100)
    (2 'relative)
    (1 'immediate)
    (0 'position)))

(defun arg-2-mode (x)
  (case (floor (mod x 10000) 1000)
    (2 'relative)
    (1 'immediate)
    (0 'position)))

(defun arg-3-mode (x)
  (case (floor x 10000)
    (2 'relative)
    (1 'immediate)
    (0 'position)))

(defun get-arg-1 (ptr program)
  (let ((op (aref program ptr))
        (p1 (aref program (+ ptr 1))))
    (case (arg-1-mode op)
      (immediate p1)
      (position  (aref program p1))
      (relative  (aref program (+ p1 *relative-base*))))))

(defun get-arg-2 (ptr program)
  (let ((op (aref program ptr))
        (p2 (aref program (+ ptr 2))))
    (case (arg-2-mode op)
      (immediate p2)
      (position  (aref program p2))
      (relative  (aref program (+ p2 *relative-base*))))))

(defun get-arg-3 (ptr program)
  (let ((op (aref program ptr))
        (p3 (aref program (+ ptr 3))))
    (case (arg-3-mode op)
      (immediate p3)
      (position  (aref program p3))
      (relative  (aref program (+ p3 *relative-base*))))))

(defun get-dest-1 (ptr program)
  (let ((op (aref program ptr))
        (p1 (aref program (+ ptr 1))))
    (or (case (arg-1-mode op)
          (position  p1)
          (relative  (+ p1 *relative-base*)))
        p1)))

(defun get-dest-2 (ptr program)
  (let ((op (aref program ptr))
        (p2 (aref program (+ ptr 2))))
    (or (case (arg-2-mode op)
          (position  p2)
          (relative  (+ p2 *relative-base*)))
        p2)))

(defun get-dest-3 (ptr program)
  (let ((op (aref program ptr))
        (p3 (aref program (+ ptr 3))))
    (or (case (arg-3-mode op)
          (position  p3)
          (relative  (+ p3 *relative-base*)))
        p3)))

;; Uses anaphors for p1..p3, d1..d3 program and ptr.
;; Only valid in this package!!
(defmacro defcpu (ops)
  `(let ((p1 (let ((res)) (lambda () (or res (setf res (get-arg-1 ptr program))))))
         (p2 (let ((res)) (lambda () (or res (setf res (get-arg-2 ptr program))))))
         (p3 (let ((res)) (lambda () (or res (setf res (get-arg-3 ptr program))))))
         (d1 (let ((res)) (lambda () (or res (setf res (get-dest-1 ptr program))))))
         (d2 (let ((res)) (lambda () (or res (setf res (get-dest-2 ptr program))))))
         (d3 (let ((res)) (lambda () (or res (setf res (get-dest-3 ptr program)))))))
     (case (mod (aref program ptr) 10)
       ,@(mapcar (lambda (op) (list (car op) (replace-anaphors (cadr op)))) ops))))

(defun replace-anaphors (xs)
  (cond
    ((null  xs)       nil)
    ((listp (car xs)) (cons (replace-anaphors (car xs))
                            (replace-anaphors (cdr xs))))
    ((eq (car xs) 'p1) (cons '(funcall p1) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'p2) (cons '(funcall p2) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'p3) (cons '(funcall p3) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'd1) (cons '(funcall d1) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'd2) (cons '(funcall d2) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'd3) (cons '(funcall d3) (replace-anaphors (cdr xs))))
    (t                 (cons (car xs) (replace-anaphors (cdr xs))))))

(defvar *relative-base*)

(defun interpret (program input ptr)
  (iter
    (for op = (aref program ptr))
    (when (eq op 99)
      (format t "BAILING!~%")
      (return (values ptr nil t)))
    (with current-input = input)
    (defcpu ((1 (setf (aref program d3) (+ p1 p2)
                      ptr               (+ ptr 4)))
             (2 (setf (aref program d3) (* p1 p2)
                      ptr               (+ ptr 4)))
             (3 (setf (aref program d1) (if (null current-input)
                                            (return (values ptr nil nil))
                                            (prog1
                                              input
                                              (setq current-input nil)))
                      ptr               (+ ptr 2)))
             (4 (return (values (+ ptr 2) p1 nil)))
             (5 (if (not (eq 0 p1))
                    (setf ptr p2)
                    (setf ptr (+ ptr 3))))
             (6 (if (eq 0 p1)
                    (setf ptr p2)
                    (setf ptr (+ ptr 3))))
             (7 (setf (aref program d3) (if (< p1 p2) 1 0)
                      ptr               (+ ptr 4)))
             (8 (setf (aref program d3) (if (eql p1 p2) 1 0)
                      ptr               (+ ptr 4)))
             (9 (setf *relative-base* (+ *relative-base* p1)
                      ptr             (+ ptr 2)))))
    (finally (return nil))))


;; # PART 2:

(defun day11-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((program (map 'vector
                      #'read-from-string
                      (str:words (ppcre:regex-replace-all ","
                                                          (car input-elements)
                                                          " "))))
        (bigger-program (make-array '(1000000) :initial-element 0))
        (*relative-base* 0)
        (direction 'north)
        (grid (make-hash-table :test #'equal)))
    (iter (for i from 0 below (length program))
          (setf (aref bigger-program i)
                (aref program        i)))
    (iter
      (for i from 0 below 100000)
      (with x = 0)
      (with y = 0)
      (with ptr = 0)
      (for current-panel-colour = (gethash (cons x y) grid 0)) ; 1 is white
      (initially
       (setf (gethash (cons x y) grid) 1)
       (setf current-panel-colour 1))
      (multiple-value-bind (new-ptr colour halted) (interpret bigger-program current-panel-colour ptr)
        (when halted
          (print "BAILING!")
          (return))
        (setf ptr new-ptr)
        (multiple-value-bind (new-new-ptr turn halted) (interpret bigger-program current-panel-colour ptr)
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

