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
      (apply-gravity moons (compute-gravity moons))
      (move-moons moons)
      (finally (return (energy moons))))))

;; Wrong: 6995100668646659090113861592797785832021591251572054995440191623545036180979798426356374356687059104594433541049290396310436317613039632550742980936594987983247678794081754451914965736698511228054563269494600931851895774172015315556646318239925741108586050516215969869043086913362993184973958813773319174662765614616490591400502650946082150346528625584918882995146573308775939055477439484438478347370774547262575844499626
;; Wrong: 10628169

(defun energy (moons)
  (iter
    (for moon in-vector moons)
    (summing (* (+ (abs (aref moon 0)) (abs (aref moon 1)) (abs (aref moon 2)))
                (+ (abs (aref moon 3)) (abs (aref moon 4)) (abs (aref moon 5)))))))

(defun move-moons (moons)
  (iter
    (for moon in-vector moons)
    (for vx = (aref moon 3))
    (for vy = (aref moon 4))
    (for vz = (aref moon 5))
    (setf (aref moon 0) (+ (aref moon 0) vx))
    (setf (aref moon 1) (+ (aref moon 1) vy))
    (setf (aref moon 2) (+ (aref moon 2) vz))))

(defun apply-gravity (moons gravities)
  (iter
    (for moon       in-vector moons)
    (for (ax ay az) in        gravities)
    (setf (aref moon 3) (+ (aref moon 3) ax))
    (setf (aref moon 4) (+ (aref moon 4) ay))
    (setf (aref moon 5) (+ (aref moon 5) az))))

(defun compute-gravity (moons)
  (iter
    (for moon in-vector moons)
    (collect
        (sum-positions
         (iter
           (for other-moon in-vector moons)
           (when (eq moon other-moon)
             (next-iteration))
           (collect (list (diff-to-one (aref moon 0) (aref other-moon 0))
                          (diff-to-one (aref moon 1) (aref other-moon 1))
                          (diff-to-one (aref moon 2) (aref other-moon 2)))))))))

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
    (collect (map 'vector
                  #'identity
                  (append (mapcar #'read-from-string (remove "" (cl-ppcre:split " "
                                                                                (cl-ppcre:regex-replace-all "[^-0-9]" line " "))
                                                             :test #'equal))
                          '(0 0 0)))
      :result-type 'vector)))

;; # PART 2:

(defun day12-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((seen-moon-xs (make-hash-table :test #'equal))
         (seen-moon-ys (make-hash-table :test #'equal))
         (seen-moon-zs (make-hash-table :test #'equal))
         (moons (parse-moons input-elements))
         (x-moons (iter (for moon in-vector moons)
                        (collect (vector (aref moon 0)
                                         (aref moon 3))
                          :result-type 'vector)))
         (x-repeat (iter
                     (for moon-key = (format nil "~a" x-moons))
                     (for i from 0 below 1000000)
                     (when (gethash moon-key seen-moon-xs)
                       (return i))
                     (setf (gethash moon-key seen-moon-xs) t)
                     (apply-gravity-1 x-moons (compute-gravity-1 x-moons))
                     (move-moons-1 x-moons)))
         (y-moons (iter (for moon in-vector moons)
                        (collect (vector (aref moon 1)
                                         (aref moon 4))
                          :result-type 'vector)))
         (y-repeat (iter
                     (for moon-key = (format nil "~a" y-moons))
                     (for i from 0 below 1000000)
                     (when (gethash moon-key seen-moon-ys)
                       (return i))
                     (setf (gethash moon-key seen-moon-ys) t)
                     (apply-gravity-1 y-moons (compute-gravity-1 y-moons))
                     (move-moons-1 y-moons)))
         (z-moons (iter (for moon in-vector moons)
                        (collect (vector (aref moon 2)
                                         (aref moon 5))
                          :result-type 'vector)))
         (z-repeat (iter
                     (for moon-key = (format nil "~a" z-moons))
                     (for i from 0 below 1000000)
                     (when (gethash moon-key seen-moon-zs)
                       (return i))
                     (setf (gethash moon-key seen-moon-zs) t)
                     (apply-gravity-1 z-moons (compute-gravity-1 z-moons))
                     (move-moons-1 z-moons))))
    (lcm x-repeat y-repeat z-repeat)))

(defun move-moons-1 (moons)
  (iter
    (for moon in-vector moons)
    (for v = (aref moon 1))
    (setf (aref moon 0) (+ (aref moon 0) v))))

(defun apply-gravity-1 (moons gravities)
  (iter
    (for moon in-vector moons)
    (for a    in        gravities)
    (setf (aref moon 1) (+ (aref moon 1) a))))

(defun compute-gravity-1 (moons)
  (iter
    (for moon in-vector moons)
    (collect
        (apply #'+
         (iter
           (for other-moon in-vector moons)
           (when (eq moon other-moon)
             (next-iteration))
           (collect (diff-to-one (aref moon 0) (aref other-moon 0))))))))

;; (defun prime-table (x)
;;   (let ((prime-table (make-array (list (1- (floor x 2))))))
;;     (let ((i 0))
;;       (map-into prime-table (lambda (_) (setf (aref prime-table i) (1+ (incf i)))) prime-table ))
;;     (loop
;;       for i from 0 below (length prime-table)
;;       when (not (null (aref prime-table i)))
;;         do (loop
;;              for j from (+ i (aref prime-table i)) below (length prime-table) by (aref prime-table i)
;;              do (setf (aref prime-table j) nil))
;;       finally (return prime-table))))


;; (defun prime-factors (x)
;;   (let ((table     (prime-table x))
;;         (current-x x)
;;         (factors))
;;     (loop
;;       for prime across table
;;       when (not (null prime))
;;         do (loop
;;              while (eq 0 (mod current-x prime))
;;              do (push prime factors)
;;              do (setf current-x (floor current-x prime)))
;;       finally (return (nreverse factors)))))

;; (defun prime-factors-mult (x)
;;   (let ((factors (prime-factors x)))
;;     (labels ((iter (xs y i acc)
;;                (cond
;;                  ((null xs)       (nreverse (cons (list y i) acc)))
;;                  ((eq (car xs) y) (iter (cdr xs) y        (1+ i) acc))
;;                  (t               (iter (cdr xs) (car xs) 1      (cons (list y i) acc))))))
;;       (iter (cdr factors)
;;             (car factors)
;;             1
;;             nil))))

;; (defun lcm (x y z)
;;   (let ((factors-x (prime-factors-mult x))
;;         (factors-y (prime-factors-mult x))
;;         (factors-z (prime-factors-mult x)))
;;     (most-of-each (sort (append factors-x factors-y factors-z)
;;                         #'<
;;                         :key #'car))))

;; (defun most-of-each (xs)
;;   xs)

;; (defun put-trie (moons trie)
;;   (iter
;;     (for moon in-vector moons)
;;     (iter
;;       (for x in-vector moon)
;;       (when (null (gethash x trie))
;;         (setf (gethash x trie) (make-hash-table)))
;;       (setq trie (gethash x trie)))))

;; (defun member-trie (moons trie)
;;   (iter outer
;;     (for moon in-vector moons)
;;     (iter
;;       (for x in-vector moon)
;;       (when (null (gethash x trie))
;;         (return-from outer nil))
;;       (setq trie (gethash x trie)))
;;     (finally (return-from outer t))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("<x=-1, y=0, z=2>"
                   "<x=2, y=-10, z=-7>"
                   "<x=4, y=-8, z=8>"
                   "<x=3, y=5, z=-1>"))
        (expected-1 '())
        (input-2 '("<x=-1, y=0, z=2>"
                   "<x=2, y=-10, z=-7>"
                   "<x=4, y=-8, z=8>"
                   "<x=3, y=5, z=-1>"))
        (expected-2 2772))
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

