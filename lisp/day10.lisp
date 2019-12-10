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
      (iter
        (for i from 0)
        (with angle = '(pos pos . infinity))
        (with first = t)
        (multiple-value-bind (coord new-angle) (closest-seen first angle x-start y-start grid x-dim y-dim)
          (format t "result [~a] ~a ~a" i coord new-angle)
          (setf (aref grid (cdr coord) (car coord)) nil)
          (setf first nil)
          (setf angle new-angle)
          (when (eq i 200)
            (return (+ (* (car coord) 100)
                       (cdr coord)))))))))

(defun closest-seen (first angle-start x y grid x-dim y-dim)
  (iter
    (for y-other from 0 below y-dim)
    (with closest)
    (with hits)
    (iter
      (for x-other from 0 below x-dim)
      (when (or (not (aref grid y-other x-other))
                (and (eq x x-other)
                     (eq y y-other)))
        (next-iteration))
      (for angle-other = (angle x-other y-other x y))
      (when (and (not first)
                 (equal angle-other angle-start))
        (next-iteration))
      (format t "~a, ~a  ~a, ~a (~a) [~a]: ~a~%" x y x-other y-other angle-start closest angle-other)
      (cond
        ((null closest) (progn
                          (setf hits    (list (cons x-other y-other))
                                closest angle-other)))
        ((and (angle-closer angle-start angle-other closest)
              (or first
                  (angle-after angle-start angle-other)))
         (progn
           (setf hits    (list (cons x-other y-other))
                 closest angle-other)))
        ((equal angle-other closest) (push (cons x-other y-other) hits))))
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
                         (angle x-res y-res x y))))))))

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

(defun angle-closer (angle-1 angle-2 angle-3)
  "Produce t if ANGLE-2 is closer to ANGLE-1 than ANGLE-3."
  (if (equal angle-2 angle-3)
      nil
      (destructuring-bind (x-sign-1 y-sign-1 . angle-frac-1) angle-1
        (destructuring-bind (x-sign-2 y-sign-2 . angle-frac-2) angle-2
          (destructuring-bind (x-sign-3 y-sign-3 . angle-frac-3) angle-3
            (let* ((encoded-1 (encode x-sign-1 y-sign-1))
                   (encoded-2 (mod (+ encoded-1 (encode x-sign-2 y-sign-2)) 4))
                   (encoded-3 (mod (+ encoded-1 (encode x-sign-3 y-sign-3)) 4)))
              (or (< encoded-2 encoded-3)
                  (and (eq encoded-2 encoded-3)
                       (< (subtract-angle angle-frac-2 angle-frac-1)
                          (subtract-angle angle-frac-3 angle-frac-1))))))))))

(defun subtract-angle (angle-1 angle-2)
  (abs (- (if (eq angle-1 'infinity) most-positive-fixnum angle-1)
          (if (eq angle-2 'infinity) most-positive-fixnum angle-2))))

(defun inf-< (one other)
  (cond
    ((eq one   'infinity) nil)
    ((eq other 'infinity) t)
    ((< one other)        t)))

(defun angle-after (angle-1 angle-2)
  (if (equal angle-1 angle-2)
      nil
      (destructuring-bind (x-sign-1 y-sign-1 . angle-frac-1) angle-1
        (destructuring-bind (x-sign-2 y-sign-2 . angle-frac-2) angle-2
          (let ((signs-1   (list x-sign-1 y-sign-1))
                (encoded-1 (encode x-sign-1 y-sign-1))
                (signs-2   (list x-sign-2 y-sign-2))
                (encoded-2 (encode x-sign-2 y-sign-2)))
           (cond
             ((equal signs-1 signs-2) (inf-< angle-frac-2 angle-frac-1))
             (t                       (inf-< encoded-1 encoded-2))))))))

(defun angle-less (angle-1 angle-2)
  (if (equal angle-1 angle-2)
      nil
      (destructuring-bind (x-sign-1 y-sign-1 . angle-frac-1) angle-1
        (destructuring-bind (x-sign-2 y-sign-2 . angle-frac-2) angle-2
          (let ((signs-1   (list x-sign-1 y-sign-1))
                (encoded-1 (encode x-sign-1 y-sign-1))
                (signs-2   (list x-sign-2 y-sign-2))
                (encoded-2 (encode x-sign-2 y-sign-2)))
           (cond
             ((equal signs-1 signs-2) (inf-< angle-frac-1 angle-frac-2))
             (t                       (inf-< encoded-1 encoded-2))))))))

(defun inf-> (one other)
  (cond
    ((and (eq one 'infinity) (not (eq other 'infinity))) t)
    ((eq other 'infinity)                                nil)
    ((> one other)                                       t)))

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

