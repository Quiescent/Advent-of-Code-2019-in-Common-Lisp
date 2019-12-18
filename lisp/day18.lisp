;;; day18 --- My solution to day18 -*-

;;; Commentary:
;; My solution to advent of code: day18

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day18
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day18)

;; # PART 1:

(defvar *keys-from*   nil)
(defvar *best-so-far* nil)

(defun day18-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((grid  (parse-map input-elements))
         (keys  (keys-from-grid grid))
         (start (initial-start grid))
         (doors (doors-from-grid grid))
         (*keys-from*   (make-hash-table :test #'equal))
         (*best-so-far* most-positive-fixnum))
    (shortest-path grid keys doors start nil nil 0)
    *best-so-far*))

;; Wrong: 7372
;; Wrong: 7368

(defun shortest-path (grid keys doors start doors-passable keys-found distance)
  (if (and (subsetp keys keys-found)
           (< distance *best-so-far*))
      (progn
        ;; (format t "Found: ~a~%" keys-found)
        (setf *best-so-far* distance)
        (format t "new best: ~a~%" *best-so-far*))
      (when (< distance *best-so-far*)
        (format t "Found: ~a~%" keys-found)
        ;; (format t "Starting at: ~a~%" start)
        ;; (format t "Distance so far: ~a~%" distance)
        (let* ((results         (keys-in-reach grid start keys doors-passable))
               (doors-from-here (mapcar #'char-downcase (doors-in-reach grid start keys doors doors-passable)))
               (destinations    (iter (for (key value) in-hashtable results)
                                      (if (member key doors-from-here)
                                          (collecting (cons key value) into opens-doors)
                                          (collecting (cons key value) into doesnt-open-door))
                                      (finally (return (append opens-doors doesnt-open-door))))))
          ;(format t "Ordered results: ~a~%" destinations)
          (iter
            (for (key . value) in destinations)
            (when (not (member key keys-found))
              (shortest-path grid
                             keys
                             doors
                             (cdr value)
                             (cons (char-upcase key) doors-passable)
                             (cons key keys-found)
                             (+ (car value) distance))))))))

(defun initial-start (grid)
  (iter
    (for (key value) in-hashtable grid)
    (when (eq value #\@)
      (return key))))

(defun keys-in-reach (grid start all-keys doors-passable)
  (let (;(from (make-hash-table :test #'equal))
        (keys (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (queue (list (cons 0 start))))
    (setf (gethash start seen) t)
    (iter
      (for i from 0 below 10000)
      (while queue)
      (for (steps . (x . y)) = (pop queue))
      (for new-this-round = nil)
      (for north = (cons x (1- y)))
      (for north-tile = (gethash north grid))
      (when (and (member north-tile all-keys)
                 (not (gethash north seen)))
        (setf (gethash north-tile keys)
              (cons (1+ steps) north))
        ;;(setf (gethash north from) (cons x y))
        (setf (gethash north seen) t)
        (push (cons (1+ steps) north) new-this-round))
      (when (and (or (eq #\. north-tile)
                     (eq #\@ north-tile)
                     (member north-tile doors-passable :test #'char-equal))
                 (not (gethash north seen)))
        (setf (gethash north seen) t)
        ;;(setf (gethash north from) (cons x y))
        (push (cons (1+ steps) north) new-this-round))
      (for south = (cons x (1+ y)))
      (for south-tile = (gethash south grid))
      (when (and (member south-tile all-keys)
                 (not (gethash south seen)))
        (setf (gethash south-tile keys)
              (cons (1+ steps) south))
        ;;(setf (gethash south from) (cons x y))
        (setf (gethash south seen) t)
        (push (cons (1+ steps) south) new-this-round))
      (when (and (or (eq #\. south-tile)
                     (eq #\@ south-tile)
                     (member south-tile doors-passable :test #'char-equal))
                 (not (gethash south seen)))
        (setf (gethash south seen) t)
        ;;(setf (gethash south from) (cons x y))
        (push (cons (1+ steps) south) new-this-round))
      (for east = (cons (1+ x) y))
      (for east-tile = (gethash east grid))
      (when (and (member east-tile all-keys)
                 (not (gethash east seen)))
        (setf (gethash east-tile keys)
              (cons (1+ steps) east))
        ;;(setf (gethash east from) (cons x y))
        (setf (gethash east seen) t)
        (push (cons (1+ steps) east) new-this-round))
      (when (and (or (eq #\. east-tile)
                     (eq #\@ east-tile)
                     (member east-tile doors-passable :test #'char-equal))
                 (not (gethash east seen)))
        (setf (gethash east seen) t)
        ;;(setf (gethash east from) (cons x y))
        (push (cons (1+ steps) east) new-this-round))
      (for west = (cons (1- x) y))
      (for west-tile = (gethash west grid))
      (when (and (member west-tile all-keys)
                 (not (gethash west seen)))
        (setf (gethash west-tile keys)
              (cons (1+ steps) west))
        ;;(setf (gethash west from) (cons x y))
        (setf (gethash west seen) t)
        (push (cons (1+ steps) west) new-this-round))
      (when (and (or (eq #\. west-tile)
                     (eq #\@ west-tile)
                     (member west-tile doors-passable :test #'char-equal))
                 (not (gethash west seen)))
        (setf (gethash west seen) t)
        ;;(setf (gethash west from) (cons x y))
        (push (cons (1+ steps) west) new-this-round))
      (setf queue (nconc queue new-this-round)))
    keys))

(defun doors-in-reach (grid start all-keys all-doors doors-passable)
  (let ((seen (make-hash-table :test #'equal))
        (queue (list (cons 0 start)))
        doors)
    (setf (gethash start seen) t)
    (iter
      (for i from 0 below 10000)
      (while queue)
      (for (steps . (x . y)) = (pop queue))
      (for new-this-round = nil)
      (for north = (cons x (1- y)))
      (for north-tile = (gethash north grid))
      (when (and (member north-tile all-doors)
                 (not (gethash north seen)))
        (push north-tile doors)
        ;;(setf (gethash north from) (cons x y))
        (setf (gethash north seen) t)
        (when (member north-tile doors-passable)
          (push (cons (1+ steps) north) new-this-round)))
      (when (and (or (eq #\. north-tile)
                     (eq #\@ north-tile)
                     (member north-tile all-keys)
                     (member north-tile doors-passable :test #'char-equal))
                 (not (gethash north seen)))
        (setf (gethash north seen) t)
        ;;(setf (gethash north from) (cons x y))
        (push (cons (1+ steps) north) new-this-round))
      (for south = (cons x (1+ y)))
      (for south-tile = (gethash south grid))
      (when (and (member south-tile all-doors)
                 (not (gethash south seen)))
        (push south-tile doors)
        ;;(setf (gethash south from) (cons x y))
        (setf (gethash south seen) t)
        (when (member south-tile doors-passable)
          (push (cons (1+ steps) south) new-this-round)))
      (when (and (or (eq #\. south-tile)
                     (eq #\@ south-tile)
                     (member south-tile all-keys)
                     (member south-tile doors-passable :test #'char-equal))
                 (not (gethash south seen)))
        (setf (gethash south seen) t)
        ;;(setf (gethash south from) (cons x y))
        (push (cons (1+ steps) south) new-this-round))
      (for east = (cons (1+ x) y))
      (for east-tile = (gethash east grid))
      (when (and (member east-tile all-doors)
                 (not (gethash east seen)))
        (push east-tile doors)
        ;;(setf (gethash east from) (cons x y))
        (setf (gethash east seen) t)
        (when (member east-tile doors-passable)
          (push (cons (1+ steps) east) new-this-round)))
      (when (and (or (eq #\. east-tile)
                     (eq #\@ east-tile)
                     (member east-tile all-keys)
                     (member east-tile doors-passable :test #'char-equal))
                 (not (gethash east seen)))
        (setf (gethash east seen) t)
        ;;(setf (gethash east from) (cons x y))
        (push (cons (1+ steps) east) new-this-round))
      (for west = (cons (1- x) y))
      (for west-tile = (gethash west grid))
      (when (and (member west-tile all-doors)
                 (not (gethash west seen)))
        (push west-tile doors)
        ;;(setf (gethash west from) (cons x y))
        (setf (gethash west seen) t)
        (when (member west-tile doors-passable)
          (push (cons (1+ steps) west) new-this-round)))
      (when (and (or (eq #\. west-tile)
                     (eq #\@ west-tile)
                     (member west-tile all-keys)
                     (member west-tile doors-passable :test #'char-equal))
                 (not (gethash west seen)))
        (setf (gethash west seen) t)
        ;;(setf (gethash west from) (cons x y))
        (push (cons (1+ steps) west) new-this-round))
      (setf queue (nconc queue new-this-round)))
    doors))

(defun doors-from-grid (grid)
  (iter
    (for (key value) in-hashtable grid)
    (when (and (not (or (eq value #\#)
                    (eq value #\.)
                    (eq value #\@)))
               (not (lower-case-p value)))
      (collecting value))))

(defun keys-from-grid (grid)
  (iter
    (for (key value) in-hashtable grid)
    (when (and (not (or (eq value #\#)
                    (eq value #\.)
                    (eq value #\@)))
               (lower-case-p value))
      (collecting value))))

(defun parse-map (lines)
  (iter
    (with grid = (make-hash-table :test #'equal))
    (for line in lines)
    (for y from 0 below (length lines))
    (iter
      (for x from 0 below (length line))
      (for char in-string line)
      (setf (gethash (cons x y) grid) char))
    (finally (return grid))))

;; # PART 2:

(defun day18-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
)

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 ;; '("#########"
                 ;;   "#b.A.@.a#"
                 ;;   "#########")
         ;; '("#################"
         ;;   "#i.G..c...e..H.p#"
         ;;   "########.########"
         ;;   "#j.A..b...f..D.o#"
         ;;   "########@########"
         ;;   "#k.E..a...g..B.n#"
         ;;   "########.########"
         ;;   "#l.F..d...h..C.m#"
         ;;   "#################")
          ;; '("########################"
          ;;   "#...............b.C.D.f#"
          ;;   "#.######################"
          ;;   "#.....@.a.B.c.d.A.e.F.g#"
          ;;   "########################")
          ;; '("########################"
          ;;   "#@..............ac.GI.b#"
          ;;   "###d#e#f################"
          ;;   "###A#B#C################"
          ;;   "###g#h#i################"
          ;;   "########################")
          )
        (expected-1 136)
        (input-2 '())
        (expected-2 '()))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day18-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day18-part-2 input-2))))

;; Run the solution:

;; (progn
;;   (print "
;; ********** OUTPUT **********
;; ")
;;   (let ((input-1 (file-lines "day18-part-1"))
;;         (input-2 (file-lines "day18-part-1")))
;;     (format t "
;; Part 1: ~s
;; " (day18-part-1 input-1))
;;     (format t "
;; Part 2: ~s
;; " (day18-part-2 input-2))))

