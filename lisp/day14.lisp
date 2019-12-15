;;; day14 --- My solution to day14 -*-

;;; Commentary:
;; My solution to advent of code: day14

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day14
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day14)

;; # PART 1:

(defun day14-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((edges (mapcar #'parse-line input-elements))
         (graph (to-graph edges))
         (*spare-ingredients* (make-hash-table)))
    (count-ore graph 'fuel 1)))

(defvar *spare-ingredients* nil)

(defun count-ore (graph node needs)
  (if (eq node 'ORE)
      (progn
        ;(format t "[ORE]~%")
        needs)
      (iter
        ;(initially (format t "~%Creating: ~a, with ~a~%" node (cdr (gethash node graph))))
        (for (num . ingredient) in (cdr (gethash node graph)))
        (for num-needed = (* needs num))
        ;(format t "Needs: ~a ~a~%" num ingredient)
        (for creates   = (or (car (gethash ingredient graph)) 1))
        (for available = (gethash ingredient *spare-ingredients* 0))
        (for taken     = (if (> num-needed available) available num-needed))
        (setf (gethash ingredient *spare-ingredients*) (- available taken))
        (for required  = (- num-needed taken))
        (for times-run = (ceiling required creates))
        (for created   = (* times-run creates))
        (incf (gethash ingredient *spare-ingredients*) (- created required))
        ;(format t "[~a]: creates: ~a, taken: ~a, required: ~a, times-run: ~a, created: ~a~%" ingredient creates taken required times-run created)
        (sum (if (eq times-run 0) 0 (count-ore graph ingredient times-run))))))

(defun to-graph (edges)
  (iter
    (with graph = (make-hash-table))
    (for ((out-count . out-type) . inputs) in edges)
    (setf (gethash out-type graph) (cons out-count inputs))
    (finally (return graph))))

(defun parse-line (line)
  (destructuring-bind (ingredients output) (ppcre:split " => " line)
    (cons (parse-chem output)
          (mapcar #'parse-chem (ppcre:split "," ingredients)))))

(defun parse-chem (in-pair-str)
  (destructuring-bind (count chem) (remove "" (ppcre:split " " in-pair-str) :test #'equal)
    (cons (read-from-string count)
          (read-from-string chem))))

;; Wrong: 20053834909354724272
;; Wrong: 1186785

;; # PART 2:

(defun day14-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((edges (mapcar #'parse-line input-elements))
         (graph (to-graph edges))
         (*spare-ingredients* (make-hash-table)))
    (iter
      (for i from 0 below 100000000)
      (sum (count-ore graph 'fuel 1) into ore-so-far)
      ;(print ore-so-far)
      (when (> ore-so-far 1000000000000)
        (return i)))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("10 ORE => 10 A"
                   "1 ORE => 1 B"
                   "7 A, 1 B => 1 C"
                   "7 A, 1 C => 1 D"
                   "7 A, 1 D => 1 E"
                   "7 A, 1 E => 1 FUEL"))
        ;; (input-1 '("9 ORE => 2 A"
        ;;            "8 ORE => 3 B"
        ;;            "7 ORE => 5 C"
        ;;            "3 A, 4 B => 1 AB"
        ;;            "5 B, 7 C => 1 BC"
        ;;            "4 C, 1 A => 1 CA"
        ;;            "2 AB, 3 BC, 4 CA => 1 FUEL"))
        (expected-1 '())
        (input-2 '("2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
                   "17 NVRVD, 3 JNWZP => 8 VPVL"
                   "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
                   "22 VJHF, 37 MNCFX => 5 FWMGM"
                   "139 ORE => 4 NVRVD"
                   "144 ORE => 7 JNWZP"
                   "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
                   "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
                   "145 ORE => 6 MNCFX"
                   "1 NVRVD => 8 CXFTF"
                   "1 VJHF, 6 MNCFX => 4 RFSQX"
                   "176 ORE => 6 VJHF"))
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day14-part-1 input-1))
    ;; (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day14-part-2 input-2))
    ))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day14-part-1"))
        (input-2 (file-lines "day14-part-1")))
    (format t "
Part 1: ~s
" (day14-part-1 input-1))
    (format t "
Part 2: ~s
" (day14-part-2 input-2))))

