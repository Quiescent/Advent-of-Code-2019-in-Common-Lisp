;;; day12 --- My solution to day12 -*-

;;; Commentary:
;; My solution to advent of code: day12

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day12
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day12)

;; # PART 1:

(defun day12-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((moons (parse-moons input-elements)))
    (iter
      (for i from 0 below 1000)
      ;(format t "[~a]: ~a~%" i moons)
      ;; (when (eq 0 (mod i 10))
      ;;   (format t "[~a]: ~a~%" i (energy moons)))
      (setf moons (move-moons (apply-gravity moons (compute-gravity moons))))
      (finally (return (energy moons))))))

;; Wrong: 6995100668646659090113861592797785832021591251572054995440191623545036180979798426356374356687059104594433541049290396310436317613039632550742980936594987983247678794081754451914965736698511228054563269494600931851895774172015315556646318239925741108586050516215969869043086913362993184973958813773319174662765614616490591400502650946082150346528625584918882995146573308775939055477439484438478347370774547262575844499626
;; Wrong: 10628169

(defun energy (moons)
  (iter
    (for (x y z vx vy vz) in moons)
    (summing (* (+ (abs x) (abs y) (abs z))
                (+ (abs vx) (abs vy) (abs vz))))))

(defun move-moons (moons)
  (iter
    (for (x y z vx vy vz) in moons)
    (collect (list (+ x vx)
                   (+ y vy)
                   (+ z vz)
                   vx
                   vy
                   vz))))

(defun apply-gravity (moons gravity)
  (mapcar (lambda (moon gravity)
            (destructuring-bind (x y z vx vy vz) moon
              (destructuring-bind (ax ay az) gravity
                (list x y z (+ vx ax) (+ vy ay) (+ vz az)))))
          moons gravity))

(defun compute-gravity (moons)
  (iter
    (for moon in moons)
    (collect
        (sum-positions
         (iter
           (for other-moon in moons)
           (when (eq moon other-moon)
             (next-iteration))
           (for (x1 y1 z1 . r1) = moon)
           (for (x2 y2 z2 . r2) = other-moon)
           (collect (list (diff-to-one x1 x2)
                          (diff-to-one y1 y2)
                          (diff-to-one z1 z2))))))))

(defun sum-positions (xs)
  (iter
    (for (x y z) in xs)
    (summing x into x-sum)
    (summing y into y-sum)
    (summing z into z-sum)
    (finally (return (list x-sum y-sum z-sum)))))

(defun diff-to-one (x y)
  (cond
    ((= x y) 0)
    ((> x y)  -1)
    ((< x y)  1)))

(defun parse-moons (lines)
  (iter
    (for line in lines)
    (collect (append (mapcar #'read-from-string (remove "" (cl-ppcre:split " "
                                                                    (cl-ppcre:regex-replace-all "[^-0-9]" line " "))
                                                        :test #'equal))
                     '(0 0 0)))))

;; # PART 2:

(defun day12-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((seen-positions (make-hash-table :test #'equal))
        (moons (parse-moons input-elements)))
    (iter
      (when (gethash moons seen-positions)
        (return i))
      (setf (gethash moons seen-positions) t)
      (for i from 0 below 10000000)
      (setf moons (move-moons (apply-gravity moons (compute-gravity moons)))))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("<x=-1, y=0, z=2>"
                   "<x=2, y=-10, z=-7>"
                   "<x=4, y=-8, z=8>"
                   "<x=3, y=5, z=-1>"))
        (expected-1 '())
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day12-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day12-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day12-part-1"))
        (input-2 (file-lines "day12-part-1")))
    (format t "
Part 1: ~s
" (day12-part-1 input-1))
    (format t "
Part 2: ~s
" (day12-part-2 input-2))))

