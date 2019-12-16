;;; day16 --- My solution to day16 -*-

;;; Commentary:
;; My solution to advent of code: day16

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day16
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day16)

;; # PART 1:

(defun day16-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((digits (map 'vector (lambda (c) (read-from-string (format nil "~a" c))) input-elements)))
    (iter
      (for i from 0 below 100)
      (n-one-phase digits))
    (read-from-string
     (apply #'concatenate 'string (map 'list (lambda (c) (format nil "~a" c))
                                       (subseq digits 0 8))))))

;; Wrong: 58244815

(defun one-phase (digits)
  (let ((pattern (map 'vector #'identity '(0 1 0 -1))))
    (iter
      (for i from 0 below (length digits))
      (collecting
       (mod (abs (iter
                   (for j from 0 below (length digits))
                   (for digit in-vector digits)
                   (for pos = (mod (floor (1+ j) (1+ i)) 4))
                   (summing (* digit (aref pattern pos)))))
            10)
       result-type vector))))

;; # PART 2:

(defun day16-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((digits-seed (map 'vector (lambda (c) (read-from-string (format nil "~a" c))) input-elements))
         (digits (apply #'concatenate 'vector (iter (for i from 0 below 10000) (collecting digits-seed))))
         (offset (read-from-string (map 'string (lambda (c) (aref (format nil "~a" c) 0)) (subseq digits-seed 0 7)))))
    (iter
      (for i from 0 below 100)
      (n-one-phase-offset digits offset))
    (read-from-string
     (apply #'concatenate 'string (map 'list (lambda (c) (format nil "~a" c))
                                       (subseq digits offset (+ offset 8)))))))

(defun n-one-phase (digits)
  (let ((pattern (map 'vector #'identity '(0 1 0 -1))))
    (iter
      (for i from 0 below (length digits))
      (for res = (mod (abs (iter
                             (for j from 0 below (length digits))
                             (for digit in-vector digits)
                             (for pos = (mod (floor (1+ j) (1+ i)) 4))
                             (summing (* digit (aref pattern pos)))))
                      10))
      (setf (aref digits i) res))))

(defun n-one-phase-offset (digits offset)
  (iter
    (with sum = (iter (for j from offset below (length digits))
                      (summing (aref digits j))))
    (for i from offset below (length digits))
    (for p = (aref digits i))
    (setf (aref digits i) (mod (abs sum) 10))
    (decf sum p)))

;; 16327
;; 528011
;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 "80871224585914546619083218645595")
        (expected-1 24176176)
        (input-2 "03036732577212944063491565474664")
        (expected-2 84462026))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day16-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day16-part-2 input-2))
    ))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day16-part-1"))
        (input-2 (file-lines "day16-part-1")))
    (format t "
Part 1: ~s
" (day16-part-1 "59719896749391372935980241840868095901909650477974922926863874668817926756504816327136638260644919270589305499504699701736406883012172909202912675166762841246709052187371758225695359676410279518694947094323466110604412184843328145082858383186144864220867912457193726817225273989002642178918584132902751560672461100948770988856677526693132615515437829437045916042287792937505148994701494994595404345537543400830028374701775936185956190469052693669806665481610052844626982468241111349622754998877546914382626821708059755592288986918651172943415960912020715327234415148476336205299713749014282618817141515873262264265988745414393060010837408970796104077"))
    (format t "
Part 2: ~s
" (day16-part-2 "59719896749391372935980241840868095901909650477974922926863874668817926756504816327136638260644919270589305499504699701736406883012172909202912675166762841246709052187371758225695359676410279518694947094323466110604412184843328145082858383186144864220867912457193726817225273989002642178918584132902751560672461100948770988856677526693132615515437829437045916042287792937505148994701494994595404345537543400830028374701775936185956190469052693669806665481610052844626982468241111349622754998877546914382626821708059755592288986918651172943415960912020715327234415148476336205299713749014282618817141515873262264265988745414393060010837408970796104077"))))
