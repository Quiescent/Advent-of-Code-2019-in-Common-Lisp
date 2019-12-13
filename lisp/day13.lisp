;;; day13 --- My solution to day13 -*-

;;; Commentary:
;; My solution to advent of code: day13

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "halting-computer.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day13
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :halting-computer)
  (:use :hash)
  (:use :iter))

(in-package :day13)

;; # PART 1:

(defun day13-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((program (parse-computer-registers input-elements))
        (*relative-base* 0)
        (grid (make-hash-table :test #'equal)))
    (iter
      (for i from 0 below 100000)
      (with ptr = 0)
      (multiple-value-bind (new-ptr x halted) (interpret program nil ptr)
        (when halted
          (print "BAILING!")
          (return (iter (for (key value) in-hashtable grid)
                        (counting (eq value 'block)))))
        (setf ptr new-ptr)
        (multiple-value-bind (new-new-ptr y halted) (interpret program nil ptr)
          (when halted
            (print "BAILING!")
            (return (iter (for (key value) in-hashtable grid)
                             (counting (eq value 'block)))))
          (setf ptr new-new-ptr)
          (multiple-value-bind (new-new-new-ptr tilde halted) (interpret program nil ptr)
            (when halted
              (print "BAILING!")
              (return (iter (for (key value) in-hashtable grid)
                            (counting (eq value 'block)))))
            (setf ptr new-new-new-ptr)
            (for tile = (case tilde
                           (0 'empty)
                           (1 'wall)
                           (2 'block)
                           (3 'paddle)
                           (4 'ball)))
            (when (eq tile 'paddle)
              (format t "Paddle: ~a, ~a~%" x y))
            (setf (gethash (cons x y) grid) tile))))
      (finally (return (iter (for (key value) in-hashtable grid)
                             (counting (eq value 'block))))))))

;; Correct: 427
;; Wrong: 139
;; Wrong: 411
;; Wrong: 1050

    ;; 0 is an empty tile. No game object appears in this tile.
    ;; 1 is a wall tile. Walls are indestructible barriers.
    ;; 2 is a block tile. Blocks can be broken by the ball.
    ;; 3 is a horizontal paddle tile. The paddle is indestructible.
    ;; 4 is a ball tile. The ball moves diagonally and bounces off objects.


;; # PART 2:

;; Paddle at 21, 23 initially...

(defun day13-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((program (parse-computer-registers input-elements))
        (*relative-base* 0)
        (grid (make-hash-table :test #'equal))
        (paddle-x 21)
        (paddle-y 23)
        (ptr 0))
    (setf (aref program 0) 2)
    (iter
      (for map-drawing-round from 0 below 1050)
      (multiple-value-bind (new-ptr x halted) (interpret program nil ptr)
        (setf ptr new-ptr)
        (multiple-value-bind (new-new-ptr y halted) (interpret program nil ptr)
          (setf ptr new-new-ptr)
          (multiple-value-bind (new-new-new-ptr tilde halted) (interpret program nil ptr)
            (setf ptr new-new-new-ptr)))))
    (print "Starting game...")
    (print "")
    (iter
      (for counter from 0 below 10000)
      (with input = 0)
      (multiple-value-bind (new-ptr x y score halted requires-input) (play-round program ptr input)
        (setf input 0)
        (setf input (diff-to-one paddle-x x))
        (incf paddle-x input)
        (format t "New paddle pos: ~a, ~a~%" paddle-x paddle-y)
        (when halted
          (print "BAILING")
          (return score))
        (format t "~a, ~a: ~a~%" x y score)
        (setf ptr new-ptr)))))

(defun diff-to-one (x y)
  (cond
    ((= x y) 0)
    ((> x y)  -1)
    ((< x y)  1)))

(defun play-round (program ptr input)
  (block outer
    (multiple-value-bind (new-ptr x halted requires-input-1) (interpret program input ptr)
      (print "first")
      (when halted
        (print "BAILING!")
        (return-from outer nil))
      (setf ptr new-ptr)
      (multiple-value-bind (new-new-ptr y halted requires-input-2) (interpret program input ptr)
        (print "second")
        (when halted
          (print "BAILING!")
          (return-from outer nil))
        (setf ptr new-new-ptr)
        (multiple-value-bind (new-new-new-ptr score halted requires-input-3) (interpret program input ptr)
          (print "third")
          (return-from outer (values new-new-new-ptr x y score halted requires-input-3)))))))

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
;; " expected-1 (day13-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day13-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day13-part-1"))
        (input-2 (file-lines "day13-part-1")))
    (format t "
Part 1: ~s
" (day13-part-1 input-1))
    (format t "
Part 2: ~s
" (day13-part-2 input-2))))

