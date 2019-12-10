;;; day10 --- My solution to day10 -*-

;;; Commentary:
;; My solution to advent of code: day10

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day10
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day10)

;; # PART 1:

(defun day10-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((grid  (parse-map input-elements))
        (x-dim (length (car input-elements)))
        (y-dim (length input-elements)))
    (iter
      (for y from 0 below y-dim)
      (maximizing
       (iter
         (for x from 0 below x-dim)
         (when (null (aref grid y x))
           (next-iteration))
         (for count-seen = (others-seen  x y grid x-dim y-dim))
         ;(format t "seen: ~a, (~a, ~a)~%~%" count-seen x y)
         (maximizing count-seen into best)
         (finally (return (or best most-negative-fixnum))))))))

(defun others-seen (x y grid x-dim y-dim)
  (iter
    (with angles-seen)
    (for y-other from 0 below y-dim)
    (sum
     (iter
       (for x-other from 0 below x-dim)
       (when (or (not (aref grid y-other x-other))
                 (and (eq x x-other)
                      (eq y y-other)))
         (next-iteration))
       (for angle-other = (angle x y x-other y-other))
       ;(format t "Angle ~a, ~a -> ~a, ~a: ~a~%" x y x-other y-other angle-other)
       (for seen-angle  = (member angle-other angles-seen :test #'equal))
       ;(format t "Seen: ~a~%" seen-angle)
       (push angle-other angles-seen)
       (count (not seen-angle))))))

(defun angle (x y x-other y-other)
  (let ((rise (- y-other y))
        (run  (- x-other x)))
    (cons (if (< rise 0) 'neg 'pos)
          (cons (if (< run 0) 'neg 'pos)
                (if (eq 0 run)
                    'infinity
                    (/ (abs rise)
                       (abs run)))))))

(defun parse-map (lines)
  (let ((grid (make-array (list (length lines) (length (car lines))) :initial-element nil)))
    (iter
      (for y from 0 below (length lines))
      (for line in lines)
      (iter
        (for x from 0 below (length line))
        (for char in-string line)
        (setf (aref grid y x) (case char (#\# t) (#\. nil))))
      (finally (return grid)))))

;; Wrong: 386
;; Wrong: 277

;; # PART 2:

(defun day10-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((grid  (parse-map input-elements))
        (x-dim (length (car input-elements)))
        (y-dim (length input-elements)))
    (destructuring-bind (x-start . y-start) (start-coord grid x-dim y-dim)
      (iter
        (for i from 0)
        (with angle = (atan 0 1))
        (with previous-angle = nil)
        (with x-prev         = x-start)
        (with y-prev         = (+ y-start 1))
        (multiple-value-bind (coord new-angle) (closest-seen previous-angle angle x-start y-start grid x-dim y-dim x-prev y-prev)
          (format t "result ~a ~a" coord new-angle)
          (setf (aref grid (cdr coord) (car coord)) nil)
          (setf previous-angle angle)
          (setf angle new-angle)
          (setf x-prev (car coord))
          (setf y-prev (cdr coord))
          (when (eq i 200)
            (return (+ (* (car coord) 100)
                       (cdr coord)))))))))

(defun closest-seen (previous-angle angle-start x y grid x-dim y-dim x-prev y-prev)
  (iter
    (for y-other from 0 below y-dim)
    (with closest)
    (with closest-test)
    (with hits)
    (iter
      (for x-other from 0 below x-dim)
      (when (or (not (aref grid y-other x-other))
                (and (eq x x-other)
                     (eq y y-other)))
        (next-iteration))
      (format t "~a, ~a   ~a, ~a~%" (- x-prev x) (- y-prev y) (- x x-other) (- y y-other))
      (for angle-other = (angle-between (- x-prev x) (- y-prev y) (- x-other x) (- y-other y)))
      (when (and previous-angle
                 (= angle-other previous-angle))
        (next-iteration))
      (format t "~a, ~a  ~a, ~a (~a): ~a~%" x y x-other y-other angle-start angle-other)
      (for test-angle = (if (> angle-other 0)
                            most-positive-fixnum
                            (abs angle-other)))
      (cond
        ((null closest-test) (progn
                               (setf hits         (list (cons x-other y-other)))
                               (setf closest      angle-other)
                               (setf closest-test test-angle)))
        ((< test-angle  closest-test) (progn
                                        (setf hits         (list (cons x-other y-other)))
                                        (setf closest      angle-other)
                                        (setf closest-test test-angle)))
        ((= angle-other closest) (push (cons x-other y-other) hits))))
    (finally
     (progn
       (format t "closest: ~a~%" closest)
       (format t "~a~%" hits)
       (destructuring-bind (x-res . y-res)
           (iter
             (for (x-res . y-res) in hits)
             (finding (cons x-res y-res) minimizing (sqrt (+ (* (- x-res x) (- x-res x))
                                                             (* (- y-res y) (- y-res y))))))
         (format t "Closest-hit: ~a, ~a" x-res y-res)
         (return (values (cons x-res y-res)
                         (atan (- x x-res) (- y y-res)))))))))

;; dot = x1*x2 + y1*y2      # dot product between [x1, y1] and [x2, y2]
;; det = x1*y2 - y1*x2      # determinant
;; angle = atan2(det, dot)  # atan2(y, x) or atan2(sin, cos)

(defun angle-between (x1 y1 x2 y2)
  (let* ((dot (+ (* x1 x2) (* y1 y2)))
         (det (- (* x1 y2) (* y1 x2)))
         (res (atan det dot)))
    (- res pi)))

(defun start-coord (grid x-dim y-dim)
  (iter
    (for y from 0 below y-dim)
    (for next = (iter
                  (for x from 0 below x-dim)
                  (when (null (aref grid y x))
                    (next-iteration))
                  (for count-seen = (others-seen  x y grid x-dim y-dim))
                  (finding (cons count-seen (cons x y)) maximizing count-seen into best)
                  (finally (return (or best (cons most-negative-fixnum nil))))))
    (finding (cdr next) maximizing (car next))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let (;; (input-1 '(".#..##.###...#######"
        ;;            "##.############..##."
        ;;            ".#.######.########.#"
        ;;            ".###.#######.####.#."
        ;;            "#####.##.#.##.###.##"
        ;;            "..#####..#.#########"
        ;;            "####################"
        ;;            "#.####....###.#.#.##"
        ;;            "##.#################"
        ;;            "#####.##.###..####.."
        ;;            "..######..##.#######"
        ;;            "####.##.####...##..#"
        ;;            ".#####..#.######.###"
        ;;            "##...#.##########..."
        ;;            "#.##########.#######"
        ;;            ".####.#.###.###.#.##"
        ;;            "....##.##.###..#####"
        ;;            ".#.#.###########.###"
        ;;            "#.#.#.#####.####.###"
        ;;            "###.##.####.##.#..##"))
        ;; (expected-1 210)
        (input-1 '("......#.#."
                   "#..#.#...."
                   "..#######."
                   ".#.#.###.."
                   ".#..#....."
                   "..#....#.#"
                   "#..#....#."
                   ".##.#..###"
                   "##...#..#."
                   ".#....####"))
        (expected-1 33)
        ;; (input-1 '(".#..#"
        ;;            "....."
        ;;            "#####"
        ;;            "....#"
        ;;            "...##"))
        ;; (expected-1 8)
        (input-2 '(".#..##.###...#######"
                   "##.############..##."
                   ".#.######.########.#"
                   ".###.#######.####.#."
                   "#####.##.#.##.###.##"
                   "..#####..#.#########"
                   "####################"
                   "#.####....###.#.#.##"
                   "##.#################"
                   "#####.##.###..####.."
                   "..######..##.#######"
                   "####.##.####...##..#"
                   ".#####..#.######.###"
                   "##...#.##########..."
                   "#.##########.#######"
                   ".####.#.###.###.#.##"
                   "....##.##.###..#####"
                   ".#.#.###########.###"
                   "#.#.#.#####.####.###"
                   "###.##.####.##.#..##"))
        (expected-2 210))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day10-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day10-part-2 input-2))))

;; Run the solution:

;; (progn
;;   (print "
;; ********** OUTPUT **********
;; ")
;;   (let ((input-1 (file-lines "day10-part-1"))
;;         (input-2 (file-lines "day10-part-1")))
;;     (format t "
;; Part 1: ~s
;; " (day10-part-1 input-1))
;;     (format t "
;; Part 2: ~s
;; " nil;; (day10-part-2 input-2)
;; )))

