;;; day17 --- My solution to day17 -*-

;;; Commentary:
;; My solution to advent of code: day17

;;; Code:

(ql:quickload "iterate")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "halting-computer.lisp")

(defpackage :day17
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :halting-computer)
  (:use :hash)
  (:use :iter))

(in-package :day17)

;; # PART 1:

(defun day17-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((program (parse-computer-registers input-elements))
         (*relative-base* 0)
         (ptr  0))
    (multiple-value-bind (grid new-ptr) (discover-map program ptr)
      (print-grid grid)
      (multiple-value-bind (min-x max-x min-y max-y) (iter
                                                       (for (key value) in-hashtable grid)
                                                       (for (x . y) = key)
                                                       (minimizing x into min-x)
                                                       (maximizing x into max-x)
                                                       (minimizing y into min-y)
                                                       (maximizing y into max-y)
                                                       (finally (return (values min-x max-x min-y max-y))))
        (iter
          (for y from min-y to max-y)
          (summing (iter
                     (for x from min-x to max-x)
                     (when (and (eq (gethash (cons x y)      grid) #\#)
                                (eq (gethash (cons (1+ x) y) grid) #\#)
                                (eq (gethash (cons x (1+ y)) grid) #\#)
                                (eq (gethash (cons (1- x) y) grid) #\#)
                                (eq (gethash (cons x (1- y)) grid) #\#))
                       (summing (* (- x min-x) (- y min-y)))))))))))

(defun discover-map (program ptr)
  (let* ((grid (make-hash-table :test #'equal)))
    (iter
      (for i from 0 below 100000)
      (with x = 0)
      (with y = 0)
      (for current-coord = (cons x y))
      (multiple-value-bind (new-ptr tile halted) (interpret program nil ptr)
        (setf ptr new-ptr)
        (when halted
          (return (values grid ptr)))
        (case tile
          (35 (progn
                (setf (gethash current-coord grid) #\#)
                (incf x)))
          (46 (progn
                (setf (gethash current-coord grid) #\.)
                (incf x)))
          (60 (progn
                (setf (gethash current-coord grid) #\<)
                (incf x)))
          (62 (progn
                (setf (gethash current-coord grid) #\>)
                (incf x)))
          (94 (progn
                (setf (gethash current-coord grid) #\^)
                (incf x)))
          (118 (progn
                (setf (gethash current-coord grid) #\^)
                (incf x)))
          (10 (progn
                (incf y)
                (setf x 0))))))))

(defun print-grid (grid)
  (multiple-value-bind (min-x max-x min-y max-y) (iter
                                                   (for (key value) in-hashtable grid)
                                                   (for (x . y) = key)
                                                   (minimizing x into min-x)
                                                   (maximizing x into max-x)
                                                   (minimizing y into min-y)
                                                   (maximizing y into max-y)
                                                   (finally (return (values min-x max-x min-y max-y))))
    (format t "~a ~a ~a ~a~%" min-x max-x min-y max-y)
    (format t "~%")
    (iter
      (for y from min-y to max-y)
      (iter
        (for x from min-x to max-x)
        (for tile = (gethash (cons x y) grid))
        (format t "~a" tile))
      (format t "~%"))))

;; # PART 2:

(defun day17-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((discover-program (parse-computer-registers input-elements))
         (control-program  (copy-seq discover-program))
         (*relative-base* 0)
         (ptr  0)
         stderr)
    (multiple-value-bind (grid new-ptr) (discover-map discover-program ptr)
      (declare (ignore new-ptr))
      (let* ((route      (find-route grid))
             (vm-program (reverse (car (find-finish route)))))
        (destructuring-bind (functions . encoded-program) (program-and-functions vm-program)
          (setf (aref control-program 0) 2)
          (setf ptr 0)
          (format t "Initialising...~%")
          (iter
            (for output = (multiple-value-bind (new-ptr output halted)
                              (interpret control-program nil ptr)
                            (setf ptr new-ptr)
                            (declare (ignore halted))
                            output))
            (while output))
          (format t "Initialised...~%")
          (iter
            (for char in-string (encode-program encoded-program))
            (multiple-value-bind (new-ptr output halted)
                (interpret control-program (char-code char) ptr)
              (declare (ignore halted))
              (push output stderr)
              (setf ptr new-ptr)))
          ;; (multiple-value-bind (new-ptr output halted)
          ;;     (interpret control-program (char-code #\,) ptr)
          ;;   (declare (ignore halted))
          ;;   (setf ptr new-ptr)
          ;;   (push output stderr))
          (multiple-value-bind (new-ptr output halted)
              (interpret control-program (char-code #\newline) ptr)
            (declare (ignore halted))
            (setf ptr new-ptr)
            (push output stderr))
          (iter
            (iter
              (for output = (multiple-value-bind (new-ptr output halted)
                                (interpret control-program nil ptr)
                              (setf ptr new-ptr)
                              (when halted (return))
                              (push output stderr)
                              output))
              (while output))
            (for (x . routine) in functions)
            (for chars = (encode-routine routine))
            ;; (multiple-value-bind (new-ptr output halted)
            ;;     (interpret control-program (case x (0 (char-code #\A)) (1 (char-code #\B)) (2 (char-code #\C))) ptr)
            ;;   (declare (ignore halted))
            ;;   (push output stderr)
            ;;   (setf ptr new-ptr))
            (iter
              (for char in-string chars)
              (multiple-value-bind (new-ptr output halted)
                  (interpret control-program (char-code char) ptr)
                (declare (ignore halted))
                (setf ptr new-ptr)
                (push output stderr)))
            (multiple-value-bind (new-ptr output halted)
                (interpret control-program (char-code #\newline) ptr)
              (declare (ignore halted))
              (push output stderr)
              (setf ptr new-ptr)))
          (iter
            (for output = (multiple-value-bind (new-ptr output halted)
                              (interpret control-program nil ptr)
                            (setf ptr new-ptr)
                            (when halted (return))
                            (push output stderr)
                            output))
            (while output))
          (multiple-value-bind (new-ptr output halted)
              (interpret control-program (char-code #\n) ptr)
            (declare (ignore halted))
            (setf ptr new-ptr)
            (push output stderr))
          (multiple-value-bind (new-ptr output halted)
              (interpret control-program (char-code #\newline) ptr)
            (declare (ignore halted))
            (setf ptr new-ptr)
            (push output stderr)
            output)
          (iter
            (with last-output)
            (for output = (multiple-value-bind (new-ptr output halted)
                              (interpret control-program nil ptr)
                            (setf ptr new-ptr)
                            (when halted (return))
                            (push output stderr)
                            output))
            (setf last-output output)
            (while output)
            (finally (return last-output)))
          (format t "~a" (map 'string (lambda (c) (code-char c)) (reverse (remove nil stderr))))
          (car stderr))))))

(defun encode-program (encoded-program)
  (let ((result (iter
                  (for code in encoded-program)
                  (for char = (case code
                                (0 #\A)
                                (1 #\B)
                                (2 #\C)))
                  (collect char :result-type 'string)
                  (collect #\, :result-type 'string))))
    (subseq result 0 (1- (length result)))))

;; Wrong: 46

(defun grid-from-lines (lines)
  (iter
    (with grid = (make-hash-table :test #'equal))
    (for y from 0)
    (for line in lines)
    (iter
      (for x from 0)
      (for char in-string line)
      (setf (gethash (cons x y) grid) char))
    (finally (return grid))))

(defun find-finish (route)
  (let (routes)
   (labels ((travel (sub-route available taken)
              (if (null sub-route)
                  taken
                  (let ((applicable (remove-if-not
                                     (lambda (routine)
                                       (and (>= (length sub-route) (length routine))
                                            (search routine sub-route :end2 (length routine))))
                                     available)))
                    (append
                     (iter
                       (for applicable-routine in applicable)
                       (for route-home = (travel (nthcdr (length applicable-routine) sub-route)
                                                 available
                                                 (cons applicable-routine taken)))
                       (when route-home
                         (push route-home routes)))
                     (when (< (length available) 3)
                       (iter
                         (for routine in (find-single-routines-from sub-route))
                         (travel sub-route (cons routine available) taken))))))))
     (travel route nil nil)
     (remove-duplicates routes :test #'equal))))

(defun can-get-to-finish (routine-group route)
  (if (null route)
      t
      (iter
        (for routine in routine-group)
        (when (search routine route :end2 (length routine))
          (format t "~%Gets from: ~a~%to: ~a~%with: ~a~%"
                  (encode-routine route)
                  (encode-routine (nthcdr (length routine) route))
                  (encode-routine routine))
          (return (can-get-to-finish routine-group
                                     (nthcdr (length routine)
                                             route))))
        (finally (return nil)))))

(defun find-all-routines (route)
  (let ((all-possible-routines
         (remove-duplicates (iter
                              (for xs on route)
                              (appending (find-single-routines-from xs)))
                            :test #'equal)))
    (remove-duplicates (combinations-3 all-possible-routines)
                       :test #'equal)))

(defun combinations-3 (xs)
  (labels ((comb (ys c)
             (cond
               ((null ys) nil)
               ((eq 3 c)  (list nil))
               (t
                (iter
                  (with result)
                  (for rest in (comb (cdr ys) (1+ c)))
                  (push (cons (car ys) rest) result)
                  (finally (return (append result (comb (cdr ys) c)))))))))
    (comb xs 0)))

(defun find-single-routines-from (xs)
  (labels ((iter (ys len acc)
                 (cond
                   ((null ys)   (list (reverse acc)))
                   ((> len 20)  (list (reverse (cdr acc))))
                   ((eq len 20) (list (reverse acc)))
                   (t           (cons (reverse acc)
                                      (iter (cdr ys)
                                            (+ len
                                               1 ; comma
                                               (char-len (car ys)))
                                            (cons (car ys) acc)))))))
    (remove-duplicates (iter (cdr xs) (char-len (car xs)) (list (car xs)))
                       :test #'equal)))

(defun encode-routine (routine)
  (let ((result (iter
                  (for symbol in routine)
                  (case symbol
                    (right (progn
                             (collecting #\R :result-type 'string)
                             (collecting #\, :result-type 'string)
                             (next-iteration)))
                    (left  (progn
                             (collecting #\L :result-type 'string)
                             (collecting #\, :result-type 'string)
                             (next-iteration))))
                  (dolist (next-char (map 'list #'identity (format nil "~a" symbol)))
                    (collecting next-char :result-type 'string))
                  (collecting #\, :result-type 'string))))
    (subseq result 0 (1- (length result)))))

(defun char-len (symbol)
  (if (symbolp symbol) 1 (length (format nil "~a" symbol))))

(defun program-and-functions (route)
  (let ((functions        (make-hash-table :test #'equal))
        (current-function 0)
        program)
    (iter
      (for routine in route)
      (for symbol =
           (if (null (gethash routine functions))
               (progn
                 (setf (gethash routine functions) current-function)
                 (incf current-function)
                 (1- current-function))
               (gethash routine functions)))
      (push symbol program)
      (finally (return (cons (iter (for (key value) in-hashtable functions)
                                   (collecting (cons value key)))
                             (reverse program)))))))

(defun find-route (grid)
  (iter
    (with route)
    (for i from 0 below 100000)
    (with coord     = (locate-bot grid))
    (with direction = (bot-direction grid))
    ;;(format t "~a ~a~%" coord direction)
    (for available-directions = (available coord grid))
    (for new-direction = (if (member direction available-directions)
                             direction
                             (car (remove-opposite direction available-directions))))
    ;;(format t "new-dir: ~a~%" new-direction)
    (when (null new-direction)
      (return (compress (reverse route))))
    (when (not (eq direction new-direction))
      ;;(format t "new-dir: ~a~%" new-direction)
      (push (turn direction new-direction) route)
      (setf direction new-direction)
      (next-iteration))
    (push 1 route)
    ;;(format t "FORWARD!~%")
    (setf coord (move direction coord))))

(defun compress (xs)
  (labels ((iter (ys count)
                 (if (null ys)
                     (list count)
                     (case (car ys)
                       (right (cons count (cons 'right (iter (cdr ys) 0))))
                       (left  (cons count (cons 'left (iter (cdr ys) 0))))
                       (1     (iter (cdr ys) (1+ count)))))))
    (cdr (iter xs 0))))

(defun turn (direction new-direction)
  (case direction
    (north (case new-direction
             (north (error "turning same direction"))
             (south (error "can't turn 180"))
             (west  'left)
             (east  'right)))
    (south (case new-direction
             (north (error "can't turn 180"))
             (south (error "turning same direction"))
             (west  'right)
             (east  'left)))
    (west  (case new-direction
             (north 'right)
             (south 'left)
             (west  (error "turning same direction"))
             (east  (error "can't turn 180"))))
    (east  (case new-direction
             (north 'left)
             (south 'right)
             (west  (error "can't turn 180"))
             (east  (error "turning same direction"))))))

(defun move (direction coord)
  (destructuring-bind (x . y) coord
    (case direction
      (north (cons x (1- y)))
      (south (cons x (1+ y)))
      (west  (cons (1- x) y))
      (east  (cons (1+ x) y)))))

(defun remove-opposite (direction directions)
  (let ((opposite (case direction
                    (north 'south)
                    (south 'north)
                    (west  'east)
                    (east  'west))))
    (remove opposite directions)))

(defun available (coord grid)
  (destructuring-bind (x . y) coord
    (remove nil
            (list (when (equal (gethash (cons x (1- y)) grid) #\#)
                    'north)
                  (when (equal (gethash (cons x (1+ y)) grid) #\#)
                    'south)
                  (when (equal (gethash (cons (1- x) y) grid) #\#)
                    'west)
                  (when (equal (gethash (cons (1+ x) y) grid) #\#)
                    'east)))))

(defun bot-direction (grid)
  (iter
    (for (key value) in-hashtable grid)
    (when (char-equal value #\^)
      (return 'north))
    (when (char-equal value #\v)
      (return 'south))
    (when (char-equal value #\>)
      (return 'east))
    (when (char-equal value #\<)
      (return 'west))))

(defun locate-bot (grid)
  (iter
    (for (key value) in-hashtable grid)
    (when (or (char-equal value #\^)
              (char-equal value #\v)
              (char-equal value #\>)
              (char-equal value #\<))
      (return key))))

;; Scratch area:

;; (progn
;;   (print "********** SCRATCH **********
;; ")
;;   (let ((input-2 '("#######...#####"
;;                    "#.....#...#...#"
;;                    "#.....#...#...#"
;;                    "......#...#...#"
;;                    "......#...###.#"
;;                    "......#.....#.#"
;;                    "^########...#.#"
;;                    "......#.#...#.#"
;;                    "......#########"
;;                    "........#...#.."
;;                    "....#########.."
;;                    "....#...#......"
;;                    "....#...#......"
;;                    "....#...#......"
;;                    "....#####......"))
;;         (expected-2 '()))
;;     (format t "
;; Part 2:
;; Expected: ~s
;;      Got: ~s
;; " expected-2 (let* ((grid (grid-from-lines input-2))
;;                     (route (find-route grid))
;;                     )
;;                (print-grid (grid-from-lines input-2))
;;                (encode-routine route)
;;                (find-finish route)))))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day17-part-1"))
        (input-2 (file-lines "day17-part-1")))
    (format t "
Part 1: ~s
" (day17-part-1 input-1))
    (format t "
Part 2: ~s
" (day17-part-2 input-2))))

