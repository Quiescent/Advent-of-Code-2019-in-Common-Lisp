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
  (let ((rise (- y-other y)) ;; Swapped because of coordinate system
        (run  (- x x-other)))
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
      (let* ((angles (mapcar #'adjust-angle (all-angles grid x-start y-start x-dim y-dim)))
             (angles-closest-first
              (sort angles #'< :key (lambda (x) (sqrt (+ (* (- (caar x)  x-start)
                                                            (- (caar x)  x-start))
                                                         (* (- (cdar x) y-start)
                                                            (- (cdar x) y-start)))))))
             (angles-by-angle (stable-sort angles-closest-first #'>
                                           :key (lambda (x) (cdddr x))))
             (sorted-angles (stable-sort angles-by-angle #'<
                                         :key (lambda (x) (encode (cadr x) (caddr x))))))
        (eat-angles sorted-angles)))))

(defun eat-angles (xs)
  (labels ((iter (rem ys i)
                 (if (eq ys nil)
                     (iter rem rem i)
                     (if (eq i 200)
                         (car ys)
                         (let* ((next       (car ys))
                                (this-angle (cdddr next))
                                (new-rem    (remove next rem :test #'equal))
                                (next-ys    (nthcdr (position-if (lambda (x) (not (= (cdddr x) this-angle)))
                                                                 ys)
                                                    ys)))
                                        ;(format t "this: ~a~%" this-angle)
                                        ;(format t "[~a]: ~a~%" i next)
                           (iter new-rem next-ys (1+ i)))))))
    (iter xs xs 1)))

(defun adjust-angle (angle)
  (destructuring-bind (coord rise-sign run-sign . angle) angle
      (let* ((num-angle (to-number angle))
             (new-angle (if (not (eq rise-sign run-sign))
                            (- most-positive-fixnum num-angle)
                            num-angle)))
        (cons coord (cons rise-sign (cons run-sign new-angle))))))

(defun to-number (x)
  (if (eq x 'infinity) most-positive-fixnum x))

(defun all-angles (grid x-start y-start x-dim y-dim)
  (iter
    (for y from 0 below y-dim)
    (appending
     (iter
       (for x from 0 below x-dim)
       (when (and (aref grid y x)
                  (not (and (eq x x-start)
                            (eq y y-start))))
         (collect (cons (cons x y) (angle x y x-start y-start))))))))

;; The 1st asteroid to be vaporized is at 11,12.
;; The 2nd asteroid to be vaporized is at 12,1.
;; The 3rd asteroid to be vaporized is at 12,2.
;; The 10th asteroid to be vaporized is at 12,8.

;; The 20th asteroid to be vaporized is at 16,0.
;; The 50th asteroid to be vaporized is at 16,9.
;; The 100th asteroid to be vaporized is at 10,16.
;; The 199th asteroid to be vaporized is at 9,6.
;; The 200th asteroid to be vaporized is at 8,2.
;; The 201st asteroid to be vaporized is at 10,9.
;; The 299th and final asteroid to be vaporized is at 11,1.

(defun encode (rise-sign run-sign)
  (cond
    ((and (eq run-sign 'pos) (eq rise-sign 'pos)) 0)
    ((and (eq run-sign 'pos) (eq rise-sign 'neg)) 1)
    ((and (eq run-sign 'neg) (eq rise-sign 'neg)) 2)
    ((and (eq run-sign 'neg) (eq rise-sign 'pos)) 3)))

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
        (expected-2 802))
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

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day10-part-1"))
        (input-2 (file-lines "day10-part-1")))
    (format t "
Part 1: ~s
" (day10-part-1 input-1))
    (format t "
Part 2: ~s
" (day10-part-2 input-2))))

