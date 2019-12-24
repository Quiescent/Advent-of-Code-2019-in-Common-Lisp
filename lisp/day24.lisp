;;; day24 --- My solution to day24 -*-

;;; Commentary:
;; My solution to advent of code: day24

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-heap")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "stepping-computer.lisp")

(defpackage :day24
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :stepping-computer)
  (:use :hash)
  (:use :iter))

(in-package :day24)

;; # PART 1:

(defun day24-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((grid    (map 'vector #'copy-seq input-elements))
         (nextgen (map 'vector #'copy-seq grid))
         (x-dim   (length (aref grid 0)))
         (y-dim   (length input-elements))
         (seen    (make-hash-table :test #'equal)))
    (iter
      (for i from 0 below 100)
      (iter
        (for row in-vector grid)
        (format t "~a~%" row))
      (format t "~%")
      (for key = (apply #'concatenate 'string (map 'list #'identity grid)))
      (when (gethash key seen)
        (return (iter
                  (with i = 1)
                  (for row in-vector grid)
                  (summing
                   (iter
                     (for char in-string row)
                     (when (eq char #\#)
                       (summing i))
                     (setf i (* i 2)))))))
      (setf (gethash key seen) t)
      (iter
        (for y from 0)
        (for row in-vector grid)
        (iter
          (for x from 0)
          (for char in-string row)
          (for adjacent =
               (+ (if (and (> x 0)          (eq #\# (aref (aref grid y)      (1- x)))) 1 0)
                  (if (and (< x (1- x-dim)) (eq #\# (aref (aref grid y)      (1+ x)))) 1 0)
                  (if (and (> y 0)          (eq #\# (aref (aref grid (1- y)) x)))      1 0)
                  (if (and (< y (1- y-dim)) (eq #\# (aref (aref grid (1+ y)) x)))      1 0)))
          (setf (aref (aref nextgen y) x)
                (cond ((and (eq adjacent 1)          (eq char #\#)) #\#)
                      ((and (member adjacent '(1 2)) (eq char #\.)) #\#)
                      (t                                            #\.)))))
      (let ((old-grid grid))
        (setf grid    nextgen)
        (setf nextgen old-grid)))))

;; Wrong: 18402865

;; I think that it spells merry christmas :)

;; # PART 2:

(defun day24-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((grid       (map 'vector #'copy-seq input-elements))
         (grids      (make-hash-table))
         (empty-grid (iter
                       (for row in-vector grid)
                       (collecting (iter
                                     (for char in-string row)
                                     (collecting #\. :result-type 'string))
                                   :result-type 'vector)))
         (nextgen    (map 'vector #'copy-seq grid))
         (x-dim      5)
         (y-dim      5)
         (max-neg    -1)
         (max-pos    1))
    (setf (gethash 0 grids) grid)
    (iter
      (for i from 0 below 200)
      (for new-grids = (make-hash-table))
      (iter
        (for level from 0 downto max-neg)
        (for current-grid = (or (gethash level      grids) (map 'vector #'copy-seq empty-grid)))
        (for grid-above   = (or (gethash (1+ level) grids) (map 'vector #'copy-seq empty-grid)))
        (for grid-below   = (or (gethash (1- level) grids) (map 'vector #'copy-seq empty-grid)))
        (iter
          (for y from 0)
          (for row in-vector current-grid)
          (iter
            (for x from 0)
            (for char in-string row)
            (when (and (eq x 2)
                       (eq y 2))
              (next-iteration))
            (for adjacent =
                 (+
                  ;; Outer ring
                  (if (and (= x 0) (eq #\# (aref (aref grid-above 2) 1))) 1 0)
                  (if (and (= x 4) (eq #\# (aref (aref grid-above 2) 3))) 1 0)
                  (if (and (= y 0) (eq #\# (aref (aref grid-above 1) 2))) 1 0)
                  (if (and (= y 4) (eq #\# (aref (aref grid-above 3) 2))) 1 0)

                  ;; Inner ring
                  ;; Top
                  (if (and (= x 2) (= y 1))
                      (iter (for i from 0 below x-dim)
                            (counting (eq (aref (aref grid-below 0) i) #\#)))
                      0)
                  ;; Bottom
                  (if (and (= x 2) (= y 3))
                      (iter (for i from 0 below x-dim)
                            (counting (eq (aref (aref grid-below 4) i) #\#)))
                      0)
                  ;; Left
                  (if (and (= x 1) (= y 2))
                      (iter (for i from 0 below y-dim)
                            (counting (eq (aref (aref grid-below i) 0) #\#)))
                      0)
                  (if (and (= x 3) (= y 2))
                      (iter (for i from 0 below y-dim)
                            (counting (eq (aref (aref grid-below i) 4) #\#)))
                      0)

                  ;; Internal
                  (if (and (not (and (= x 3) (= y 2)))
                           (> x 0)
                           (eq #\# (aref (aref current-grid y) (1- x))))
                      1 0)
                  (if (and (not (and (= x 1) (= y 2)))
                           (< x (1- x-dim))
                           (eq #\# (aref (aref current-grid y) (1+ x))))
                      1 0)
                  (if (and (not (and (= y 3) (= x 2)))
                           (> y 0)
                           (eq #\# (aref (aref current-grid (1- y)) x)))
                      1 0)
                  (if (and (not (and (= y 1) (= x 2)))
                           (< y (1- y-dim))
                           (eq #\# (aref (aref current-grid (1+ y)) x)))
                      1 0)))
            ;;(format t "Adj: ~a~%" adjacent)
            (setf (aref (aref nextgen y) x)
                  (cond ((and (eq adjacent 1)          (eq char #\#)) #\#)
                        ((and (member adjacent '(1 2)) (eq char #\.)) #\#)
                        (t                                            #\.))))
          ;;(format t "~%")
          )
        (setf (gethash level new-grids) (map 'vector #'copy-seq nextgen)))
      (iter
        (for level from 1 to max-pos)
        (for current-grid = (or (gethash level      grids) (map 'vector #'copy-seq empty-grid)))
        (for grid-above   = (or (gethash (1+ level) grids) (map 'vector #'copy-seq empty-grid)))
        (for grid-below   = (or (gethash (1- level) grids) (map 'vector #'copy-seq empty-grid)))
        ;; (iter
        ;;   (for row in-vector grid-below)
        ;;   (format t "~a~%" row))
        (iter
          (for y from 0)
          (for row in-vector current-grid)
          (iter
            (for x from 0)
            (for char in-string row)
            (when (and (eq x 2)
                       (eq y 2))
              (next-iteration))
            (for adjacent =
                 (+
                  ;; Outer ring
                  (if (and (= x 0) (eq #\# (aref (aref grid-above 2) 1))) 1 0)
                  (if (and (= x 4) (eq #\# (aref (aref grid-above 2) 3))) 1 0)
                  (if (and (= y 0) (eq #\# (aref (aref grid-above 1) 2))) 1 0)
                  (if (and (= y 4) (eq #\# (aref (aref grid-above 3) 2))) 1 0)

                  ;; Inner ring
                  ;; Top
                  (if (and (= x 2) (= y 1))
                      (iter (for i from 0 below x-dim)
                            (counting (eq (aref (aref grid-below 0) i) #\#)))
                      0)
                  ;; Bottom
                  (if (and (= x 2) (= y 3))
                      (iter (for i from 0 below x-dim)
                            (counting (eq (aref (aref grid-below 4) i) #\#)))
                      0)
                  ;; Left
                  (if (and (= x 1) (= y 2))
                      (iter (for i from 0 below y-dim)
                            (counting (eq (aref (aref grid-below i) 0) #\#)))
                      0)
                  (if (and (= x 3) (= y 2))
                      (iter (for i from 0 below y-dim)
                            (counting (eq (aref (aref grid-below i) 4) #\#)))
                      0)

                  ;; Internal
                  (if (and (not (and (= x 3) (= y 2)))
                           (> x 0)
                           (eq #\# (aref (aref current-grid y) (1- x))))
                      1 0)
                  (if (and (not (and (= x 1) (= y 2)))
                           (< x (1- x-dim))
                           (eq #\# (aref (aref current-grid y) (1+ x))))
                      1 0)
                  (if (and (not (and (= y 3) (= x 2)))
                           (> y 0)
                           (eq #\# (aref (aref current-grid (1- y)) x)))
                      1 0)
                  (if (and (not (and (= y 1) (= x 2)))
                           (< y (1- y-dim))
                           (eq #\# (aref (aref current-grid (1+ y)) x)))
                      1 0)))
            ;;(format t "Adj: ~a~%" adjacent)
            (setf (aref (aref nextgen y) x)
                  (cond ((and (eq adjacent 1)          (eq char #\#)) #\#)
                        ((and (member adjacent '(1 2)) (eq char #\.)) #\#)
                        (t                                            #\.))))
          ;;(format t "~%")
          )
        (setf (gethash level new-grids) (map 'vector #'copy-seq nextgen)))
      (setf grids new-grids)
      (incf max-pos)
      (incf max-neg -1))
    (iter
      (for (level level-grid) in-hashtable grids)
      (format t "level: ~a~%" level)
      (summing
       (iter
         (for y from 0)
         (for row in-vector level-grid)
         (format t "~a~%" row)
         (summing
          (iter
            (for x from 0)
            (for char in-string row)
            (counting (and (not (and (eq y 2) (eq x 2)))
                           (eq char #\#)))))))
      (format t "~%"))))

;; Wrong: 4717

;; Scratch area:

(progn
  (format t "~%********** SCRATCH **********~%")
  (let ((input-2    '("....#"
                      "#..#."
                      "#.?##"
                      "..#.."
                      "#...."))
        (input-1    '("....#"
                      "#..#."
                      "#..##"
                      "..#.."
                      "#...."))
        (expected-2 '()))
    (format t "~%Part 1:~%Expected: ~s~%     Got: ~s~%" expected-2 (day24-part-1 input-1))
    (format t "~%Part 2:~%Expected: ~s~%     Got: ~s~%" expected-2 (day24-part-2 input-2))))

;; Run the solution:

(progn
  (format t "~%********** OUTPUT **********~%")
  (let ((input-1 (file-lines "day24-part-1"))
        (input-2 (file-lines "day24-part-1")))
    (format t "~%Part 1: ~s~%" (day24-part-1 input-1))
    (format t "~%Part 2: ~s~%" (day24-part-2 input-2))))

