;;; day22 --- My solution to day22 -*-

;;; Commentary:
;; My solution to advent of code: day22

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-heap")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "hash-table-computer.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")

(defpackage :day22
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :hash-table-computer)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day22)

;; # PART 1:

(declaim (optimize speed))

(defun day22-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((cards (make-array (list 10007) :initial-element 0))
        (tmp (make-array (list 10007) :initial-element 0)))
    (iter
      (for i from 0 below 10007)
      (setf (aref cards i) i))
    (iter
      (for line in input-elements)
      (for is-cuts      = (ppcre:all-matches-as-strings "cut" line))
      (for is-increment = (ppcre:all-matches-as-strings "increment" line))
      (for is-reverse   = (ppcre:all-matches-as-strings "stack" line))
      (cond
        ((not (null is-cuts))
         (let* ((cut-num (read-from-string (car (ppcre:all-matches-as-strings "[-0-9]+" line))))
                (initial-start (if (< cut-num 0) (+ 10007 cut-num) cut-num))
                (new-start (iter
                             (for i from initial-start below 10007)
                             (for pos from 0)
                             (setf (aref tmp pos) (aref cards i))
                             (finally (return pos)))))
           (iter
             (for i from 0 below initial-start)
             (for pos from (1+ new-start))
             (setf (aref tmp pos) (aref cards i)))
           (let ((old-cards cards))
             (setq cards tmp)
             (setq tmp old-cards))))
        ((not (null is-increment))
         (let ((inc-num (read-from-string (car (ppcre:all-matches-as-strings "[-0-9]+" line)))))
           (iter
             (for pos from 0 below 10007)
             (with i = 0)
             (setf (aref tmp i) (aref cards pos))
             (incf i inc-num)
             (setf i (mod i 10007)))
           (let ((old-cards cards))
             (setq cards tmp)
             (setq tmp old-cards))))
        ((not (null is-reverse)) (setq cards (nreverse cards)))))
    (format t "cards: ~a~%" (length (remove-duplicates cards)))
    (format t "Card at 2020: ~a~%" (aref cards 2020))
    (position 2019 cards)))

;; Wrong: 90

;; # PART 2:

(defun day22-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((cards (make-array (list 10007) :initial-element 0))
         (tmp (make-array (list 10007) :initial-element 0))
         (all-instructions (map 'vector (lambda (line) (parse-line line 10007)) input-elements))
         (instructions-length (length all-instructions)))
    (iter
      (for i from 0 below 10007)
      (setf (aref cards i) i))
    (labels
        ((solve (x i)
           (if (eq i -1)
               x
               (let* ((idx (mod i instructions-length))
                      (looking-for (funcall (aref all-instructions idx) x)))
                 ;; (format t "looking-for: ~a~%" x)
                 ;; (format t "looking-for-next: ~a~%" looking-for)
                 (solve looking-for (1- i))))))
      (solve 2020 (1- instructions-length)))
    ;; (iter
    ;;   (for cnt from 0 below 1)
    ;;   (with seen = (make-hash-table))
    
    ;;   (for 2020-card = (aref cards 2020))
    ;;   (when (gethash 2020-card seen)
    ;;     (format t "Repeat at: ~a~%" cnt)
    ;;     (return))
    ;;   (setf (gethash 2020-card seen) t)
    ;;   ;;(format t "Card at 2020: ~a~%" (aref cards 2020))
    ;;   )
    ;; (aref cards 2020)
    ))

(defun parse-line (line len)
  (let ((number (car (mapcar #'read-from-string (ppcre:all-matches-as-strings "[-0-9]+" line)))))
    (cond
      ((ppcre:all-matches-as-strings "cut" line)
       (lambda (idx) (let ((cut (if (< number 0)
                                    (+ len number)
                                    number)))
                       ;; (format t "Cutting: Cut: ~a, idx: ~a, len: ~a~%"
                       ;;         cut idx len)
                       (mod (+ idx cut) len))))
      ((ppcre:all-matches-as-strings "increment" line)
       (lambda (idx) (progn
                       ;; (format t "Incing: len: ~a, num: ~a, mod: ~a~%"
                       ;;         len idx number)
                       (solve-modulo len idx number))))
      ((ppcre:all-matches-as-strings "stack" line)
       (lambda (idx) (progn
                       ;; (format t "Reving: rev: ~a, idx: ~a~%" idx len)
                       (- (1- len) idx)))))))

(defun solve-modulo (len res grp)
  (iter
    (for i from 0 below len)
    (when (eq res (mod (* i grp) len))
      (return i))))

;; Scratch area:

;; (progn
;;   (format t "~%********** SCRATCH **********~%")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "~%Part 1:~%Expected: ~s~%     Got: ~s~%" expected-1 (day22-part-1 input-1))
;;     (format t "~%Part 2:~%Expected: ~s~%     Got: ~s~%" expected-2 (day22-part-2 input-2))))

;; Run the solution:

(progn
  (format t "~%********** OUTPUT **********~%")
  (let ((input-1 (file-lines "day22-part-1"))
        (input-2 (file-lines "day22-part-1")))
    (format t "~%Part 1: ~s~%" (day22-part-1 input-1))
    (format t "~%Part 2: ~s~%" (day22-part-2 input-2))))

