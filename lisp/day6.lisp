;;; day6 --- My solution to day6 -*-

;;; Commentary:
;; My solution to advent of code: day6

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")
(ql:quickload "str")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day6
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day6)

;; # PART 1:

(defun day6-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((tuples (parse-tuples input-elements))
         (graph  (tuples-to-graph tuples)))
    (iter
      (for start in (remove-duplicates (mapcar #'car tuples) :test #'equal))
      (sum (1- (iter
                 (for i dfs-across-graph-without-duplicates graph :from start)
                 (count t)))))))

(defun parse-tuples (xs)
  (mapcar (lambda (x) (let ((words (str:words (ppcre:regex-replace "\\)" x " "))))
                   (cons (car words)
                         (cadr words))))
          xs))

;; # PART 2:

(defun day6-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((tuples           (parse-tuples input-elements))
         (graph            (reverse-graph (tuples-to-graph tuples)))
         (santas-distances (make-hash-table :test #'equal))
         (you-distances    (make-hash-table :test #'equal)))
    (setf (gethash "SAN" santas-distances) 0)
    (setf (gethash "YOU" you-distances)    0)
    (iter
      (for i bfs-across-graph-without-duplicates graph :from "SAN")
      (for current = (car i))
      (for prev    = (cdr i))
      (when prev
        (setf (gethash current santas-distances) (1+ (gethash prev santas-distances)))))
    (iter
      (for i bfs-across-graph-without-duplicates graph :from "YOU")
      (for current = (car i))
      (for prev    = (cdr i))
      (when prev
        (setf (gethash current you-distances) (1+ (gethash prev you-distances)))))
    (iter
      (for start in (remove-duplicates (mapcar #'car tuples) :test #'equal))
      (minimizing (+ (gethash start santas-distances most-positive-fixnum)
                     (gethash start you-distances    most-positive-fixnum))
                  into minimized)
      (finally (return (- minimized 2))))))

;; Wrong: 263
;; Wrong: 363
;; Wrong: 364


;; (iter
;;       (for i bfs-across-graph-without-duplicates graph :from "YOU")
;;       ;; (for current = (car i))
;;       ;; (for prev    = (cdr i))
;;       (when prev
;;         (setf (gethash current you-distances) (1+ (gethash prev you-distances)))))

;; (iter
;;       (for i bfs-across-graph-without-duplicates graph :from "SAN")
;;       )

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"))
        (expected-1 42)
        (input-2 '("COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"))
        (expected-2 4))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day6-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day6-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day6-part-1"))
        (input-2 (file-lines "day6-part-1")))
    (format t "
Part 1: ~s
" (day6-part-1 input-1))
    (format t "
Part 2: ~s
" (day6-part-2 input-2))))

