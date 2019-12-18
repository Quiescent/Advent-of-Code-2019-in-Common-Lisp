;;; day18 --- My solution to day18 -*-

;;; Commentary:
;; My solution to advent of code: day18

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")
(ql:quickload "cl-heap")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(require :sb-sprof)
(declaim (optimize speed))

(defpackage :day18
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day18)

;; # PART 1:

(defun day18-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((y-dim (length input-elements))
         (x-dim (length (car input-elements)))
         (grid  (parse-map input-elements x-dim y-dim))
         (keys  (keys-from-grid grid x-dim y-dim))
         (start (initial-start grid x-dim y-dim))
         (all-searches (keys-and-requirements grid keys (list start) x-dim y-dim)))
    ;; (format t "~%")
    ;; (print-map grid x-dim y-dim)
    ;; (format t "start: ~a~%" start)
    ;; (format t "keys: ~a~%" keys)
    (shortest-path grid all-searches keys start)))

(defun print-map (grid x-dim y-dim)
  (iter
    (for y from 0 below y-dim)
    (iter
      (for x from 0 below x-dim)
      (format t "~a" (aref grid y x)))
    (format t "~%")))

;; Wrong: 7372
;; Wrong: 7368
;; Wrong: 6590
;; Correct: 6286

(defun shortest-path (grid all-searches keys start)
  (let* ((best  most-positive-fixnum)
         (keys-len (length keys))
         (queue (list (list keys-len 0 start nil nil))))
    (iter
      ;(format t "~a~%" queue)
      (for i from 0 below 100000)
      (while queue)
      (for (priority distance coord keys-found doors-passable) = (pop queue))
      (when (eq (length keys-found) keys-len)
        (when (< distance best)
          (format t "[~a] ~a~%" distance keys-found)
          (setf best distance))
        (next-iteration))
      (when (> distance best)
        (next-iteration))
      (setf queue (sort queue #'< :key #'car))
      ;;(for results = (keys-in-reach grid coord keys-found keys doors-passable))
      (for tile = (aref grid (cdr coord) (car coord)))
      (for results = (keys-in-reach-2 all-searches
                                      keys-found
                                      (if (eq tile #\@) coord tile)
                                      doors-passable))
      (iter
        (for (tile steps . coord) in results)
        (push (list (+ steps (- keys-len (1+ (length keys-found))))
                    (+ steps distance)
                    coord
                    (cons tile keys-found)
                    (cons (char-upcase tile) doors-passable))
              queue)))
    best))

(defun initial-start (grid x-dim y-dim)
  (iter outer
    (for y from 0 below y-dim)
    (iter
      (for x from 0 below x-dim)
      (for value = (aref grid y x))
      (when (eq value #\@)
        (return-from outer (cons x y))))))

(defun initial-starts (grid x-dim y-dim)
  (iter
    (with starts)
    (for y from 0 below y-dim)
    (iter
      (for x from 0 below x-dim)
      (for value = (aref grid y x))
      (when (eq value #\@)
        (push (cons x y) starts)))
    (finally (return starts))))

(defun keys-in-reach (grid start keys-found all-keys doors-passable)
  (let ((seen (make-hash-table :test #'equal))
        (queue (list (cons 0 start)))
        keys)
    (setf (gethash start seen) t)
    (iter
      (while queue)
      (for (steps . (x . y)) = (pop queue))
      (for new-this-round = nil)
      (for north = (cons x (1- y)))
      (for north-tile = (aref grid (1- y) x))
      (when (and (member north-tile all-keys)
                 (not (gethash north seen)))
        (when (not (member north-tile keys-found))
          (push (cons north-tile (cons (1+ steps) north)) keys))
        (setf (gethash north seen) t)
        (push (cons (1+ steps) north) new-this-round))
      (when (and (or (eq #\. north-tile)
                     (eq #\@ north-tile)
                     (member north-tile doors-passable :test #'char-equal))
                 (not (gethash north seen)))
        (setf (gethash north seen) t)
        (push (cons (1+ steps) north) new-this-round))
      (for south = (cons x (1+ y)))
      (for south-tile = (aref grid (1+ y) x))
      (when (and (member south-tile all-keys)
                 (not (gethash south seen)))
        (when (not (member south-tile keys-found))
          (push (cons south-tile (cons (1+ steps) south)) keys))
        (setf (gethash south seen) t)
        (push (cons (1+ steps) south) new-this-round))
      (when (and (or (eq #\. south-tile)
                     (eq #\@ south-tile)
                     (member south-tile doors-passable :test #'char-equal))
                 (not (gethash south seen)))
        (setf (gethash south seen) t)
        (push (cons (1+ steps) south) new-this-round))
      (for east = (cons (1+ x) y))
      (for east-tile = (aref grid y (1+ x)))
      (when (and (member east-tile all-keys)
                 (not (gethash east seen)))
        (when (not (member east-tile keys-found))
          (push (cons east-tile (cons (1+ steps) east)) keys))
        (setf (gethash east seen) t)
        (push (cons (1+ steps) east) new-this-round))
      (when (and (or (eq #\. east-tile)
                     (eq #\@ east-tile)
                     (member east-tile doors-passable :test #'char-equal))
                 (not (gethash east seen)))
        (setf (gethash east seen) t)
        (push (cons (1+ steps) east) new-this-round))
      (for west = (cons (1- x) y))
      (for west-tile = (aref grid y (1- x)))
      (when (and (member west-tile all-keys)
                 (not (gethash west seen)))
        (when (not (member west-tile keys-found))
          (push (cons west-tile (cons (1+ steps) west)) keys))
        (setf (gethash west seen) t)
        (push (cons (1+ steps) west) new-this-round))
      (when (and (or (eq #\. west-tile)
                     (eq #\@ west-tile)
                     (member west-tile doors-passable :test #'char-equal))
                 (not (gethash west seen)))
        (setf (gethash west seen) t)
        (push (cons (1+ steps) west) new-this-round))
      (setf queue (nconc queue new-this-round)))
    keys))

(defun keys-from-grid (grid x-dim y-dim)
  (iter
    (with result)
    (for y from 0 below y-dim)
    (iter
      (for x from 0 below x-dim)
      (for value = (aref grid y x))
      (when (and (not (or (eq value #\#)
                          (eq value #\.)
                          (eq value #\@)))
                 (lower-case-p value))
        (push value result)))
    (finally (return result))))

(defun parse-map (lines x-dim y-dim)
  (iter
    (with grid = (make-array (list y-dim x-dim)))
    (for line in lines)
    (for y from 0)
    (iter
      (for char in-string line)
      (for x from 0)
      (setf (aref grid y x) char))
    (finally (return grid))))

;; # PART 2:

(defun day18-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((y-dim        (length input-elements))
         (x-dim        (length (car input-elements)))
         (grid         (parse-map input-elements x-dim y-dim))
         (keys         (keys-from-grid grid x-dim y-dim))
         (starts       (initial-starts grid x-dim y-dim))
         (all-searches (keys-and-requirements grid keys starts x-dim y-dim)))
    ;; (format t "~%")
    (print-map grid x-dim y-dim)
    ;; (format t "start: ~a~%" start)
    ;; (format t "keys: ~a~%" keys)
    
    (shortest-path-2 grid all-searches keys starts)))

;; Too high: 2216
;; Correct: 2140

(defun shortest-path-2 (grid all-searches keys starts)
  (let* ((best  most-positive-fixnum)
         (keys-len (length keys))
         (queue (make-instance 'cl-heap:priority-queue)))
    (cl-heap:enqueue queue (list 0 (map 'vector #'identity starts) nil nil) (* 100 keys-len))
    (iter
      (for i from 0 below 1000000000)
      ;;(format t "Queue: ~a~%" queue)
      (for elem = (cl-heap:dequeue queue))
      (while elem)
      (for (distance coords keys-found doors-passable) = elem)
      ;; (format t "Found: ~a~%" keys-found)
      (when (eq (length keys-found) keys-len)
        (when (< distance best)
          (format t "[~a] ~a~%" distance keys-found)
          (setf best distance))
        (next-iteration))
      (when (> distance best)
        (next-iteration))
      (iter
        (for coord in-vector coords)
        (for idx from 0)
        (for tile = (aref grid (cdr coord) (car coord)))
        (for results = (keys-in-reach-2 all-searches
                                        keys-found
                                        (if (eq tile #\@) coord tile)
                                        doors-passable))
        ;; (for results = (keys-in-reach grid coord keys-found keys doors-passable))
        (when (not (null results))
          (iter
            (for (tile steps . new-coord) in results)
            (for new-coords = (copy-seq coords))
            (setf (aref new-coords idx) new-coord)
            (cl-heap:enqueue queue
                             (list (+ steps distance)
                                   new-coords
                                   (cons tile keys-found)
                                   (cons (char-upcase tile) doors-passable))
                             (+ steps (* 100 (- keys-len (1+ (length keys-found))))))))))
    best))

(defun keys-in-reach-2 (all-searches keys-found key doors-passable)
  (iter
    (for (destination requirements distance coord) in (gethash key all-searches))
    (when (or (not (null (set-difference requirements doors-passable)))
              (member destination keys-found))
      (next-iteration))
    (collecting (cons destination (cons distance coord)))))

(defun find-key (grid key x-dim y-dim)
  (iter outer
    (for y from 0 below y-dim)
    (iter
      (for x from 0 below x-dim)
      (when (eq (aref grid y x) key)
        (return-from outer (cons x y))))))

(defun keys-and-requirements (grid all-keys starts x-dim y-dim)
  (let ((searches-from (make-hash-table :test #'equal)))
    (iter
      (for key in (append all-keys starts))
      (for start = (if (consp key) key (find-key grid key x-dim y-dim)))
      (for seen  = (make-hash-table :test #'equal))
      (for queue = (list (list 0 start nil)))
      (setf (gethash start seen) t)
      (iter
        (while queue)
        (for (steps (x . y) doors-been-through) = (pop queue))
        (for new-this-round = nil)
        (labels ((push-tile (tile location)
                   (when (and (member tile all-keys)
                              (not (gethash location seen)))
                     (push (cons tile (list doors-been-through (1+ steps) location))
                           (gethash key searches-from))
                     (setf (gethash location seen) t)
                     (push (list (1+ steps) location doors-been-through) new-this-round))
                   (when (and (not (or (eq #\. tile)
                                       (eq #\@ tile)
                                       (eq #\# tile)))
                              (not (lower-case-p tile))
                              (not (gethash location seen)))
                     (setf (gethash location seen) t)
                     (push (list (1+ steps) location (cons tile doors-been-through)) new-this-round))
                   (when (and (or (eq #\. tile)
                                  (eq #\@ tile))
                              (not (gethash location seen)))
                     (setf (gethash location seen) t)
                     (push (list (1+ steps) location doors-been-through) new-this-round))))

          (for north = (cons x (1- y)))
          (for north-tile = (aref grid (1- y) x))
          (push-tile north-tile north)

          (for south = (cons x (1+ y)))
          (for south-tile = (aref grid (1+ y) x))
          (push-tile south-tile south)
          
          (for east = (cons (1+ x) y))
          (for east-tile = (aref grid y (1+ x)))
          (push-tile east-tile east)
          
          (for west = (cons (1- x) y))
          (for west-tile = (aref grid y (1- x)))
          (push-tile west-tile west))

        (setf queue (nconc queue new-this-round))))
    searches-from))

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
        (input-2 ;; '("#######"
                 ;;   "#a.#Cd#"
                 ;;   "##@#@##"
                 ;;   "#######"
                 ;;   "##@#@##"
                 ;;   "#cB#Ab#"
                 ;;   "#######")
         '("#############"
           "#g#f.D#..h#l#"
           "#F###e#E###.#"
           "#dCba@#@BcIJ#"
           "#############"
           "#nK.L@#@G...#"
           "#M###N#H###.#"
           "#o#m..#i#jk.#"
           "#############"))
        (expected-2 8))
;;     (format t "
;; Part 1:
;; Expected: ~s
;;      Got: ~s
;; " expected-1 (day18-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day18-part-2 input-2))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day18-part-1"))
        (input-2 (file-lines "day18-part-2")))
    ;; (format t "
;; Part 1: ~s
;; " (day18-part-1 input-1))
    (format t "
Part 2: ~s
" (day18-part-2 input-2))
    ))

;; (let ((input-2 (file-lines "day18-part-2")))
;;   (sb-sprof:with-profiling (:max-samples 1000
;;                             :report :flat
;;                             :loop t)
;;     (day18-part-2 input-2)))
