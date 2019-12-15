;;; day15 --- My solution to day15 -*-

;;; Commentary:
;; My solution to advent of code: day15

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "halting-computer.lisp")

(defpackage :day15
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :halting-computer)
  (:use :hash)
  (:use :iter))

(in-package :day15)

;; # PART 1:

(defun day15-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((program (parse-computer-registers input-elements))
         (*relative-base* 0)
         (coords (list (cons 0 (cons 0 0))))
         (seen (make-hash-table :test #'equal)))
    (multiple-value-bind (grid from destination) (discover-map program)
      ;;(format t "Destination: ~a~%" destination)
      (print-grid grid)
      (iter
        (for (steps . coord) = (pop coords))
        ;;(format t "steps: ~a~%" steps)
        ;;(format t "coord: ~a~%" coord)
        (when (equal coord destination)
          (return steps))
        (setf (gethash coord seen) t)
        (destructuring-bind (x . y) coord
          (setf coords
                (nconc coords
                       (remove nil
                               (list (let ((new-coord (cons x (1- y))))
                                       (if (and (not (eq 'wall (gethash new-coord grid)))
                                                (not (null (gethash new-coord grid)))
                                                (not (gethash new-coord seen)))
                                           (cons (1+ steps) new-coord)
                                           nil))
                                     (let ((new-coord (cons x (1+ y))))
                                       (if (and (not (eq 'wall (gethash new-coord grid)))
                                                (not (null (gethash new-coord grid)))
                                                (not (gethash new-coord seen)))
                                           (cons (1+ steps) new-coord)
                                           nil))
                                     (let ((new-coord (cons (1- x) y)))
                                       (if (and (not (eq 'wall (gethash new-coord grid)))
                                                (not (null (gethash new-coord grid)))
                                                (not (gethash new-coord seen)))
                                           (cons (1+ steps) new-coord)
                                           nil))
                                     (let ((new-coord (cons (1+ x) y)))
                                       (if (and (not (eq 'wall (gethash new-coord grid)))
                                                (not (null (gethash new-coord grid)))
                                                (not (gethash new-coord seen)))
                                           (cons (1+ steps) new-coord)
                                           nil)))))))))))

;; Wrong: 237
;; Wrong: 236
;; Wrong: 235

(defun discover-map (program)
  (let* ((grid       (make-hash-table :test #'equal))
         (origin     (cons 0 0))
         (seen       (make-hash-table :test #'equal))
         (from       (make-hash-table :test #'equal))
         (ptr        0)
         (coord      origin)
         (direction  1)
         destination)
    (setf (gethash origin grid) 'floor)
    (iter
      (for i from 0 below 100000)
      ;;(format t "coord: ~a: " coord)
      (setf (gethash coord seen) t)
      ;; Find first direction...
      (destructuring-bind (x . y) coord
       (setf direction
             (cond
               ((not (gethash (cons x (1- y)) seen)) 1)
               ((not (gethash (cons x (1+ y)) seen)) 2)
               ((not (gethash (cons (1- x) y) seen)) 3)
               ((not (gethash (cons (1+ x) y) seen)) 4))))
      ;; If there is nowhere to go and we're back at the origin then bail
      (when (and (not direction)
                 (equal coord origin))
        (return (values grid from destination)))
      ;; If there is none then do
      (when (not direction)
        ;;(format t "Back tracking...~%")
        (let ((old-coord (gethash coord from)))
          (setf direction (direction-to old-coord coord))
          (setf coord old-coord))
        (multiple-value-bind (new-ptr status halted) (interpret program direction ptr)
          (setf ptr new-ptr))
        (next-iteration))
      ;; Otherwise account for what's there and record where we came from
      (for current-coord = (new-coord direction coord))
      (multiple-value-bind (new-ptr status halted) (interpret program direction ptr)
        (setf ptr new-ptr)
        (case status
          (0 (progn
               (setf (gethash current-coord grid) 'wall)
               (setf (gethash current-coord seen) t)
               ;;(format t "WALL~%")
               ))
          (2 (progn
               (setf (gethash current-coord grid) 'destination)
               (setf (gethash current-coord seen) t)
               (setf (gethash current-coord from) coord)
               (setf coord current-coord)
               (setf destination coord)
               ;;(format t "DEST~%")
               ))
          (1 (progn
               ;;(format t "FLOOR~%")
               (setf (gethash current-coord grid) 'floor)
               (setf (gethash current-coord seen) t)
               (setf (gethash current-coord from) coord)
               (setf coord current-coord))))))))

(defun new-coord (direction coord)
  (destructuring-bind (x . y) coord
    (case direction
      (1 (cons x (1- y)))
      (2 (cons x (1+ y)))
      (3 (cons (1- x) y))
      (4 (cons (1+ x) y)))))

(defun direction-to (to-coord from-coord)
  (destructuring-bind (x1 . y1) to-coord
    (destructuring-bind (x2 . y2) from-coord
      (cond
        ((> x1 x2) 4)
        ((< x1 x2) 3)
        ((> y1 y2) 2)
        ((< y1 y2) 1)))))

(defun discover-map-2 (program)
  (let ((ptr   0)
        (coord (cons 0 0))
        (from  (make-hash-table :test #'equal))
        (grid  (make-hash-table :test #'equal)))
    (setf (gethash (cons 0 0) grid) 'floor)
    (iter
      (for i from 0 below 100000)
      (for (x . y) = coord)
      (for direction =
           (cond
             ((not (gethash (cons x (1- y)) grid)) 1)
             ((not (gethash (cons x (1+ y)) grid)) 2)
             ((not (gethash (cons (1- x) y) grid)) 3)
             ((not (gethash (cons (1+ x) y) grid)) 4)))
      (when (null direction)
        ;;(format t "Backtracking...~%")
        (when (equal coord (cons 0 0))
          (return grid))
        (setf coord (gethash coord from))
        (next-iteration))
      (for new-coord =
           (case direction
             (1 (cons x (1- y)))
             (2 (cons x (1+ y)))
             (3 (cons (1- x) y))
             (4 (cons (1+ x) y))))
      (setf (gethash new-coord from) coord)
      (multiple-value-bind (new-ptr code) (interpret program direction ptr)
        (setf ptr new-ptr)
        (case code
          (0 (setf (gethash new-coord grid) 'wall))
          (1 (setf (gethash new-coord grid) 'floor))
          (2 (setf (gethash new-coord grid) 'destination))))
      (when (not (eq (gethash new-coord grid) 'wall))
        (setf coord new-coord)))))

;; # PART 2:

(defun day15-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((program (parse-computer-registers input-elements))
         (*relative-base* 0)
         (coords (list (cons 0 (cons -16 14))))
         (seen (make-hash-table :test #'equal)))
    (multiple-value-bind (grid from destination) (discover-map program)
      (print-grid grid)
      (iter
        (for (steps . coord) = (pop coords))
        ;;(format t "coord: ~a~%" coord)
        (setf (gethash coord seen) t)
        (destructuring-bind (x . y) coord
          (setf coords
                (nconc coords
                       (remove nil
                               (list (let ((new-coord (cons x (1- y))))
                                       (if (and (not (eq 'wall (gethash new-coord grid)))
                                                (not (null (gethash new-coord grid)))
                                                (not (gethash new-coord seen)))
                                           (cons (1+ steps) new-coord)
                                           nil))
                                     (let ((new-coord (cons x (1+ y))))
                                       (if (and (not (eq 'wall (gethash new-coord grid)))
                                                (not (null (gethash new-coord grid)))
                                                (not (gethash new-coord seen)))
                                           (cons (1+ steps) new-coord)
                                           nil))
                                     (let ((new-coord (cons (1- x) y)))
                                       (if (and (not (eq 'wall (gethash new-coord grid)))
                                                (not (null (gethash new-coord grid)))
                                                (not (gethash new-coord seen)))
                                           (cons (1+ steps) new-coord)
                                           nil))
                                     (let ((new-coord (cons (1+ x) y)))
                                       (if (and (not (eq 'wall (gethash new-coord grid)))
                                                (not (null (gethash new-coord grid)))
                                                (not (gethash new-coord seen)))
                                           (cons (1+ steps) new-coord)
                                           nil)))))))
        (maximizing steps into most-steps)
        ;;(format t "coords: ~a~%" coords)
        (when (null coords)
          (return most-steps))))))

(defun print-grid (grid)
  (multiple-value-bind (min-x max-x min-y max-y) (iter
                                                   (for (key value) in-hashtable grid)
                                                   (for (x . y) = key)
                                                   (minimizing x into min-x)
                                                   (maximizing x into max-x)
                                                   (minimizing y into min-y)
                                                   (maximizing y into max-y)
                                                   (finally (return (values min-x max-x min-y max-y))))
    (format t "~%")
    (iter
      (for y from min-y to max-y)
      (iter
        (for x from min-x to max-x)
        (when (and (eq x 0)
                   (eq y 0))
          (format t "O")
          (next-iteration))
        (for tile = (gethash (cons x y) grid))
        (format t "~a"
                (cond
                  ((eq tile 'wall)        #\#)
                  ((eq tile 'destination) #\X)
                  ((eq tile 'floor)       #\_)
                  (t                      #\#))))
      (format t "~%"))))

;; Wrong: 391
;; Wrong: 499
;; Wrong: 500
;; Wrong: 501
;; Wrong: 502
;; Wrong: 503

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
;; " expected-1 (day15-part-1 input-1))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (day15-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day15-part-1"))
        (input-2 (file-lines "day15-part-1")))
    (format t "
Part 1: ~s
" (day15-part-1 input-1))
    (format t "
Part 2: ~s
" (day15-part-2 input-2))))

