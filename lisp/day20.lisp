;;; day20 --- My solution to day20 -*-

;;; Commentary:
;; My solution to advent of code: day20

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")
(ql:quickload "cl-heap")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "hash-table-computer.lisp")

(defpackage :day20
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash-table-computer)
  (:use :hash)
  (:use :iter))

(in-package :day20)

;; # PART 1:

(defun day20-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((grid    (parse-map input-elements))
         (portals (extract-portals grid))
         (start   (find-start portals))
         (end     (find-end portals)))
    ;;(portal-jump-square (find-portal (cons 65 86) portals) (cons 65 86) portals)
    ;;(format t "end: ~a~%" end)
    (shortest-path grid portals start end)))

;; Wrong: 177 (too low)
;; Wrong: 612 (too high)

(defun shortest-path (grid portals start end)
  (let ((seen  (make-hash-table :test #'equal))
        (queue (make-instance 'cl-heap:priority-queue)))
    (cl-heap:enqueue queue (cons 0 start) 0)
    (iter
      (for i from 0 below 10000000)
      (for elem = (cl-heap:dequeue queue))
      (while elem)
      ;;(format t "Current: ~a~%" elem)
      (for (steps . coord) = elem)
      (when (equal coord end)
        (format t "FOUND IT!~%")
        (return steps))
      (setf (gethash coord seen) t)
      (labels ((find-floor (new-coord)
                 (destructuring-bind (coord-x . coord-y) new-coord
                   (let ((east  (gethash (cons (1+ coord-x) coord-y)      grid))
                         (west  (gethash (cons (1- coord-x) coord-y)      grid))
                         (south (gethash (cons coord-x      (1+ coord-y)) grid))
                         (north (gethash (cons coord-x      (1- coord-y)) grid))
                         new-coords)
                       (when (is-floor east)
                          (push (cons (1+ coord-x) coord-y) new-coords))
                        (when (is-floor west)
                          (push (cons (1- coord-x) coord-y) new-coords))
                        (when (is-floor south)
                          (push (cons coord-x      (1+ coord-y)) new-coords))
                        (when (is-floor north)
                          (push (cons coord-x      (1- coord-y)) new-coords))
                        new-coords)))
               (enqueue (new-coord)
                 (let ((portal     (find-portal new-coord portals))
                       (new-coords (find-floor  new-coord)))
                   (when portal
                     (push (portal-jump-square portal new-coord portals) new-coords))
                   (iter
                     (for found-coord in new-coords)
                     (when (null found-coord)
                       (next-iteration))
                     ;;(format t "Found: ~a~%" found-coord)
                     (when (null (gethash found-coord seen))
                       (cl-heap:enqueue queue
                                        (cons (1+ steps) found-coord)
                                        steps))))))
        
        (enqueue coord)))))

(defun portal-jump-square (portal coord portals)
  (car (remove coord (gethash portal portals) :test #'equal)))

(defun find-portal (coord portals)
  (iter
    (for (key value) in-hashtable portals)
    (when (member coord value :test #'equal)
      (return key))))

(defun manhattan-distance (coord-1 coord-2)
  (destructuring-bind (x-1 . y-1) coord-1
    (destructuring-bind (x-2 . y-2) coord-2
      (+ (abs (- x-1 x-2))
         (abs (- y-1 y-2))))))

(defun find-start (portals)
  (car (gethash (list #\A #\A) portals)))

(defun find-end (portals)
  (car (gethash (list #\Z #\Z) portals)))

(defun extract-portals (grid)
  (let ((portals (make-hash-table :test #'equal)))
    (iter
      (for ((x . y) value) in-hashtable grid)
      (when (is-portal-square value)
        (let* ((east  (gethash (cons (1+ x) y)      grid))
               (west  (gethash (cons (1- x) y)      grid))
               (south (gethash (cons x      (1+ y)) grid))
               (north (gethash (cons x      (1- y)) grid)))
          (labels ((find-floor (x y char-1 char-2)
                     (let ((east      (gethash (cons (1+ x) y)      grid))
                           (west      (gethash (cons (1- x) y)      grid))
                           (south     (gethash (cons x      (1+ y)) grid))
                           (north     (gethash (cons x      (1- y)) grid)))
                       (cond
                         ((is-floor east)  (push (cons (1+ x) y)
                                                 (gethash (mapcar #'cdr
                                                                  (sort (list (cons (cons x y)      char-1)
                                                                              (cons (cons (1+ x) y) char-2))
                                                                        #'coord-less-p
                                                                        :key #'car))
                                                          portals)))
                         ((is-floor west)  (push (cons (1- x) y)
                                                 (gethash (mapcar #'cdr
                                                                  (sort (list (cons (cons x y)      char-1)
                                                                              (cons (cons (1- x) y) char-2))
                                                                        #'coord-less-p
                                                                        :key #'car))
                                                          portals)))
                         ((is-floor south) (push (cons x      (1+ y))
                                                 (gethash (mapcar #'cdr
                                                                  (sort (list (cons (cons x y)      char-1)
                                                                              (cons (cons x (1+ y)) char-2))
                                                                        #'coord-less-p
                                                                        :key #'car))
                                                          portals)))
                         ((is-floor north) (push (cons x      (1- y))
                                                 (gethash (mapcar #'cdr
                                                                  (sort (list (cons (cons x y)      char-1)
                                                                              (cons (cons x (1- y)) char-2))
                                                                        #'coord-less-p
                                                                        :key #'car))
                                                          portals)))
                         (t                nil)))))
            (cond
              ((is-portal-square east)  (find-floor x      y value east))
              ((is-portal-square west)  (find-floor x      y value west))
              ((is-portal-square south) (find-floor x      y value south))
              ((is-portal-square north) (find-floor x      y value north))))))
      (finally (return portals)))))

(defun coord-less-p (coord-1 coord-2)
  (destructuring-bind (x-1 . y-1) coord-1
    (destructuring-bind (x-2 . y-2) coord-2
      (or (< y-1 y-2)
          (and (eq y-1 y-2)
               (< x-1 x-2))))))

(defun is-floor (char)
  (eq char #\.))

(defun is-portal-square (char)
  (and char (not (or (eq char #\.)
                     (eq char #\#)
                     (eq char #\space)))))

(defun parse-map (lines)
  (let ((grid (make-hash-table :test #'equal)))
    (iter
      (for y from 0)
      (for line in lines)
      (iter
        (for x from 0)
        (for char in-string line)
        (setf (gethash (cons x y) grid) char))
      (finally (return grid)))))

;; # PART 2:

(defun day20-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((grid      (parse-map input-elements))
         (portals   (extract-portals grid))
         (start     (find-start portals))
         (end       (find-end portals))
         (all-paths (all-paths grid portals)))
    (multiple-value-bind (min-x min-y max-x max-y)
        (iter
          (for (key value) in-hashtable grid)
          (for (x . y) = key)
          (minimizing x into min-x)
          (minimizing y into min-y)
          (maximizing x into max-x)
          (maximizing y into max-y)
          (finally (return (values min-x min-y max-x max-y))))
      ;;(portal-jump-square (find-portal (cons 65 86) portals) (cons 65 86) portals)
      ;;(format t "end: ~a~%" end)
      (shortest-path-2 all-paths portals start end (+ 2 min-x) (+ 2 min-y) (- max-x 2) (- max-y 2)))))

(defun all-paths (grid portals)
  (let ((paths       (make-hash-table :test #'equal))
        (all-targets (iter (for (key values) in-hashtable portals)
                           (unioning values))))
    (format t "All-targets: ~a~%" all-targets)
    (iter
      (for target in all-targets)
      (let ((seen      (make-hash-table :test #'equal))
            (queue     (make-instance 'cl-heap:priority-queue))
            (to-target (make-hash-table :test #'equal)))
        (cl-heap:enqueue queue (cons 0 target) 0)
        (iter
          (for i from 0 below 1000000)
          (for elem = (cl-heap:dequeue queue))
          ;; (format t "Current: ~a~%" elem)
          (while elem)
          (for (steps . coord) = elem)
          (when (member coord all-targets :test #'equal)
            (setf (gethash coord to-target) steps))
          (setf (gethash coord seen) t)
          (labels ((find-floor (new-coord)
                     (destructuring-bind (coord-x . coord-y) new-coord
                       (let ((east  (gethash (cons (1+ coord-x) coord-y)      grid))
                             (west  (gethash (cons (1- coord-x) coord-y)      grid))
                             (south (gethash (cons coord-x      (1+ coord-y)) grid))
                             (north (gethash (cons coord-x      (1- coord-y)) grid))
                             new-coords)
                         (when (is-floor east)
                           (push (cons (1+ coord-x) coord-y) new-coords))
                         (when (is-floor west)
                           (push (cons (1- coord-x) coord-y) new-coords))
                         (when (is-floor south)
                           (push (cons coord-x      (1+ coord-y)) new-coords))
                         (when (is-floor north)
                           (push (cons coord-x      (1- coord-y)) new-coords))
                         new-coords)))
                   (enqueue (new-coord)
                     (let ((new-coords (find-floor  new-coord)))
                       (iter
                         (for found-coord in new-coords)
                         (when (null found-coord)
                           (next-iteration))
                         ;;(format t "Found: ~a~%" found-coord)
                         (when (null (gethash found-coord seen))
                           (cl-heap:enqueue queue
                                            (cons (1+ steps) found-coord)
                                            steps))))))
            (enqueue coord)))
        (setf (gethash target paths) to-target)))
    paths))

(defun shortest-path-2 (all-paths portals start end min-x min-y max-x max-y)
  (let ((seen  (make-hash-table :test #'equal))
        (queue (make-instance 'cl-heap:priority-queue)))
    (cl-heap:enqueue queue (list 0 0 start) 0)
    (iter
      (for i from 0 below 100000)
      (for elem = (cl-heap:dequeue queue))
      ;;(format t "Current: ~a~%" elem)
      (while elem)
      (for (level steps coord) = elem)
      (when (and (eq level 0)
                 (equal coord end))
        (format t "FOUND IT!~%")
        (format t "The thing: ~a~%" elem)
        (format t "The steps: ~a~%" steps)
        (return steps))
      (when (null (gethash level seen))
        (setf (gethash level seen)
              (make-hash-table :test #'equal)))
      (setf (gethash coord (gethash level seen)) t)
      (labels ((not-been-there (level test-coord)
                 (or (null (gethash level seen))
                     (null (gethash test-coord (gethash level seen)))))
               (not-on-outer-ring (new-coord)
                 (destructuring-bind (coord-x . coord-y) new-coord
                   (not (or (eq coord-x min-x)
                            (eq coord-x max-x)
                            (eq coord-y min-y)
                            (eq coord-y max-y))))))
        (let* ((portal      (find-portal coord portals))
               (portal-dest (portal-jump-square portal coord portals)))
          (when (and (not-on-outer-ring coord)
                     (not-been-there (1+ level) portal-dest))
            (cl-heap:enqueue queue
                     (list (1+ level) (1+ steps) portal-dest)
                     (+ steps
                        (* 100 (1+ level)))))
          (when (and (> 0 level)
                     (not (not-on-outer-ring coord))
                     (not-been-there (1- level) portal-dest))
            (cl-heap:enqueue queue
                     (list (1- level) (1+ steps) portal-dest)
                     (+ steps
                        (* 100 (1- level)))))
         (iter
           (for (destination steps-to-take) in-hashtable (gethash coord all-paths))
           (when (not-been-there level destination)
             (cl-heap:enqueue queue
                      (list level (+ steps steps-to-take) destination)
                      (+ steps
                        (* 100 level))))))))))

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("                   A               "
                   "                   A               "
                   "  #################.#############  "
                   "  #.#...#...................#.#.#  "
                   "  #.#.#.###.###.###.#########.#.#  "
                   "  #.#.#.......#...#.....#.#.#...#  "
                   "  #.#########.###.#####.#.#.###.#  "
                   "  #.............#.#.....#.......#  "
                   "  ###.###########.###.#####.#.#.#  "
                   "  #.....#        A   C    #.#.#.#  "
                   "  #######        S   P    #####.#  "
                   "  #.#...#                 #......VT"
                   "  #.#.#.#                 #.#####  "
                   "  #...#.#               YN....#.#  "
                   "  #.###.#                 #####.#  "
                   "DI....#.#                 #.....#  "
                   "  #####.#                 #.###.#  "
                   "ZZ......#               QG....#..AS"
                   "  ###.###                 #######  "
                   "JO..#.#.#                 #.....#  "
                   "  #.#.#.#                 ###.#.#  "
                   "  #...#..DI             BU....#..LF"
                   "  #####.#                 #.#####  "
                   "YN......#               VT..#....QG"
                   "  #.###.#                 #.###.#  "
                   "  #.#...#                 #.....#  "
                   "  ###.###    J L     J    #.#.###  "
                   "  #.....#    O F     P    #.#...#  "
                   "  #.###.#####.#.#####.#####.###.#  "
                   "  #...#.#.#...#.....#.....#.#...#  "
                   "  #.#####.###.###.#.#.#########.#  "
                   "  #...#.#.....#...#.#.#.#.....#.#  "
                   "  #.###.#####.###.###.#.#.#######  "
                   "  #.#.........#...#.............#  "
                   "  #########.###.###.#############  "
                   "           B   J   C               "
                   "           U   P   P               "))
        (expected-1 58)
        (input-2 
         '("             Z L X W       C                 "
           "             Z P Q B       K                 "
           "  ###########.#.#.#.#######.###############  "
           "  #...#.......#.#.......#.#.......#.#.#...#  "
           "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  "
           "  #.#...#.#.#...#.#.#...#...#...#.#.......#  "
           "  #.###.#######.###.###.#.###.###.#.#######  "
           "  #...#.......#.#...#...#.............#...#  "
           "  #.#########.#######.#.#######.#######.###  "
           "  #...#.#    F       R I       Z    #.#.#.#  "
           "  #.###.#    D       E C       H    #.#.#.#  "
           "  #.#...#                           #...#.#  "
           "  #.###.#                           #.###.#  "
           "  #.#....OA                       WB..#.#..ZH"
           "  #.###.#                           #.#.#.#  "
           "CJ......#                           #.....#  "
           "  #######                           #######  "
           "  #.#....CK                         #......IC"
           "  #.###.#                           #.###.#  "
           "  #.....#                           #...#.#  "
           "  ###.###                           #.#.#.#  "
           "XF....#.#                         RF..#.#.#  "
           "  #####.#                           #######  "
           "  #......CJ                       NM..#...#  "
           "  ###.#.#                           #.###.#  "
           "RE....#.#                           #......RF"
           "  ###.###        X   X       L      #.#.#.#  "
           "  #.....#        F   Q       P      #.#.#.#  "
           "  ###.###########.###.#######.#########.###  "
           "  #.....#...#.....#.......#...#.....#.#...#  "
           "  #####.#.###.#######.#######.###.###.#.#.#  "
           "  #.......#.......#.#.#.#.#...#...#...#.#.#  "
           "  #####.###.#####.#.#.#.#.###.###.#.###.###  "
           "  #.......#.....#.#...#...............#...#  "
           "  #############.#.#.###.###################  "
           "               A O F   N                     "
           "               A A D   M                     ")
          )
        (expected-2 396))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day20-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day20-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day20-part-1"))
        (input-2 (file-lines "day20-part-1")))
;;     (format t "
;; Part 1: ~s
;; " (day20-part-1 input-1))
;;     (format t "
;; Part 2: ~s
;; " (day20-part-2 input-2))
    (day20-part-2 input-2)))

