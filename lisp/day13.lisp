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
            ;(format t "~a, ~a: ~a~%" x y tile)
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
;; Dimensions: 42 x 25

(defun day13-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((program (parse-computer-registers input-elements))
        (grid (make-hash-table :test #'equal))
        (*relative-base* 0)
        (ptr 0)
        (joystick-input 0)
        (game-running nil)
        (paddle-x 21)
        game-won
        tile
        score
        x
        y)
    (setf (aref program 0) 2)
    (iter
      (for i from 0 below 10000000)
      (when (not (or (not game-running)
                 (iter (for (key value) in-hashtable grid)
                       (when (eq value 'block)
                         (return t)))))
        (setf game-won t))
      (multiple-value-bind (new-ptr new-x halted) (interpret program joystick-input ptr)
        (when halted
          (print "GAME OVER!")
          (return nil))
        (setf ptr new-ptr)
        (setf x new-x))
      (multiple-value-bind (new-ptr new-y halted) (interpret program joystick-input ptr)
        (when halted
          (print "GAME OVER!")
          (return nil))
        (setf y new-y)
        (setf ptr new-ptr))
      (multiple-value-bind (new-ptr tilde halted) (interpret program joystick-input ptr)
        (setf ptr new-ptr)
        (when halted
          (print "GAME OVER!")
          (return nil))
                                        ;(format t "~a ~a: ~a~%" x y tilde)
        (if (not (and (eq x -1)
                      (eq y 0)))
            (progn
              (setf tile (case tilde
                           (0 'empty)
                           (1 'wall)
                           (2 'block)
                           (3 'paddle)
                           (4 'ball)))
              (setf (gethash (cons x y) grid) tile))
            (setf score tilde)))
      (when (and game-running (eq tile 'ball))
        (for ball-x = (iter (for (key value) in-hashtable grid)
                            (when (eq value 'ball)
                              (return (car key)))))
        (print-map grid)
        (setf joystick-input (diff-to-one paddle-x ball-x))
        (incf paddle-x joystick-input)
        (format t "Paddle: ~a~%" paddle-x)
        (format t "Ball: ~a~%" ball-x)
        (format t "Input: ~a~%" joystick-input))
      (when (and (eq x -1)
                 (eq y 0))
        (when game-won
          (format t "Final score: ~a~%" score)
          (return score))
        (setf game-running t)
        (format t "Score: ~a~%" score)))))

(defun diff-to-one (x y)
  (cond
    ((= x y) 0)
    ((> x y)  -1)
    ((< x y)  1)))

;; (defun read-score (ptr program joystick-input)
;;   (multiple-value-bind (new-ptr x halted) (interpret program joystick-input ptr)
;;     (when halted
;;       (print "BAILING!"))
;;     (multiple-value-bind (new-new-ptr y halted) (interpret program joystick-input new-ptr)
;;       (when halted
;;         (print "BAILING!"))
;;       (multiple-value-bind (new-new-new-ptr score halted) (interpret program joystick-input new-new-ptr)
;;         (when halted
;;           (print "BAILING!"))
;;         (format t "~a ~a ~a~%" x y score)
;;         (when (not (and (eq x -1)
;;                         (eq y 0)))
;;           (error "expecting score..."))
;;         (values new-new-new-ptr score)))))

;; (defun read-map (input-ptr program joystick-input)
;;   (let ((grid (make-hash-table :test #'equal))
;;         (ptr  input-ptr))
;;     (iter
;;       (for i from 0 below 1050)
;;       (multiple-value-bind (new-ptr x halted) (interpret program joystick-input ptr)
;;         (when halted
;;           (print "BAILING!"))
;;         (setf ptr new-ptr)
;;         (multiple-value-bind (new-new-ptr y halted) (interpret program joystick-input ptr)
;;           (when halted
;;             (print "BAILING!"))
;;           (setf ptr new-new-ptr)
;;           (multiple-value-bind (new-new-new-ptr tilde halted) (interpret program joystick-input ptr)
;;             (when halted
;;               (print "BAILING!"))
;;             (setf ptr new-new-new-ptr)
;;             (format t "~a ~a: ~a~%" x y tilde)
;;             (for tile = (case tilde
;;                           (0 'empty)
;;                           (1 'wall)
;;                           (2 'block)
;;                           (3 'paddle)
;;                           (4 'ball)))
;;             (setf (gethash (cons x y) grid) tile))))
;;       (finally (return (values ptr grid))))))

(defun print-map (grid)
  (iter
    (for y from 0 below 25)
    (iter
      (for x from 0 below 42)
      (format t "~a" (case (gethash (cons x y) grid)
                       (empty  #\.)
                       (wall   #\|)
                       (block  #\#)
                       (paddle #\_)
                       (ball   #\o))))
    (format t "~%")))

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

