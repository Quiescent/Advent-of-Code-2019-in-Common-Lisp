;;; day7 --- My solution to day7 -*-

;;; Commentary:
;; My solution to advent of code: day7

;;; Code:

(ql:quickload "iterate")
(ql:quickload "str")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "computer.lisp")

(defpackage :day7
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash)
  (:use :iter))

(in-package :day7)

;; # PART 1:

(defun day7-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((program (map 'vector
                      #'read-from-string
                      (str:words (ppcre:regex-replace-all ","
                                                          (car input-elements)
                                                          " ")))))
    (iter
      (for (phase-one phase-two phase-three phase-four phase-five) in (combinations '(0 1 2 3 4)))
      (maximizing (let ((*original-stdout* *standard-output*)
                        (*standard-output* (make-string-output-stream)))
                    (with-input-from-string (*standard-input* (format nil "~a~%0" phase-one))
                      (computer:interpret (copy-seq program)))
                    (let ((input-two (read-from-string (get-output-stream-string *standard-output*))))
                      (with-input-from-string (*standard-input* (format nil "~a~%~a" phase-two input-two))
                        (computer:interpret (copy-seq program)))
                      (let ((input-three (read-from-string (get-output-stream-string *standard-output*))))
                        (with-input-from-string (*standard-input* (format nil "~a~%~a" phase-three input-three))
                          (computer:interpret (copy-seq program)))
                        (let ((input-four (read-from-string (get-output-stream-string *standard-output*))))
                          (with-input-from-string (*standard-input* (format nil "~a~%~a" phase-four input-four))
                            (computer:interpret (copy-seq program)))
                          (let ((input-five (read-from-string (get-output-stream-string *standard-output*))))
                            (with-input-from-string (*standard-input* (format nil "~a~%~a" phase-five input-five))
                              (computer:interpret (copy-seq program)))
                            (let ((output (get-output-stream-string *standard-output*)))
                              (read-from-string output)))))))))))

;; Wrong: 1916

(defun combinations (xs)
  (if (null xs)
      (list nil)
      (apply #'append
             (mapcar (lambda (x)
                       (mapcar (lambda (other) (cons x other))
                               (combinations (remove x xs))))
                     xs))))

;; # PART 2:

;; FUNKY COMPUTER

(defun arg-1-mode (x)
  (if (eq (floor (mod x 1000) 100) 1)
      'immediate
      'position))

(defun arg-2-mode (x)
  (if (eq (floor (mod x 10000) 1000) 1)
      'immediate
      'position))

(defun arg-3-mode (x)
  (if (eq (floor x 10000) 1)
      'immediate
      'position))

(defun get-arg-1 (ptr program)
  (let ((op (aref program ptr))
        (p1 (aref program (+ ptr 1))))
    (if (eq (arg-1-mode op) 'immediate) p1 (aref program p1))))

(defun get-arg-2 (ptr program)
  (let ((op (aref program ptr))
        (p2 (aref program (+ ptr 2))))
    (if (eq (arg-2-mode op) 'immediate) p2 (aref program p2))))

(defun get-arg-3 (ptr program)
  (let ((op (aref program ptr))
        (p3 (aref program (+ ptr 3))))
    (if (eq (arg-2-mode op) 'immediate) p3 (aref program p3))))

;; Uses anaphors for p1..p3, d1..d3 program and ptr.
;; Only valid in this package!!
(defmacro defcpu (ops)
  `(let ((p1 (let ((res)) (lambda () (or res (setf res (get-arg-1 ptr program))))))
         (p2 (let ((res)) (lambda () (or res (setf res (get-arg-2 ptr program))))))
         (p3 (let ((res)) (lambda () (or res (setf res (get-arg-3 ptr program))))))
         (d1 (let ((res)) (lambda () (or res (setf res (aref program (+ ptr 1)))))))
         (d2 (let ((res)) (lambda () (or res (setf res (aref program (+ ptr 2)))))))
         (d3 (let ((res)) (lambda () (or res (setf res (aref program (+ ptr 3))))))))
     (case (mod (aref program ptr) 10)
       ,@(mapcar (lambda (op) (list (car op) (replace-anaphors (cadr op)))) ops))))

(defun replace-anaphors (xs)
  (cond
    ((null  xs)       nil)
    ((listp (car xs)) (cons (replace-anaphors (car xs))
                            (replace-anaphors (cdr xs))))
    ((eq (car xs) 'p1) (cons '(funcall p1) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'p2) (cons '(funcall p2) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'p3) (cons '(funcall p3) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'd1) (cons '(funcall d1) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'd2) (cons '(funcall d2) (replace-anaphors (cdr xs))))
    ((eq (car xs) 'd3) (cons '(funcall d3) (replace-anaphors (cdr xs))))
    (t                 (cons (car xs) (replace-anaphors (cdr xs))))))

(defvar *original-stdout* *standard-output*)

(defun interpret (program input ptr)
  (iter
    (for op = (aref program ptr))
    (when (eq op 99)
      (format *original-stdout* "BAILING!~%")
      (return (values ptr nil t)))
    ;; (format *original-stdout* "~a~%" program)
    ;; (format *original-stdout* "Opcode: ~a~%" op)
    ;; (format *original-stdout* "ptr: ~a~%" ptr)
    (with current-input = input)
    (defcpu ((1 (setf (aref program d3) (+ p1 p2)
                      ptr               (+ ptr 4)))
             (2 (setf (aref program d3) (* p1 p2)
                      ptr               (+ ptr 4)))
             (3 (setf (aref program d1) (if (null current-input)
                                            (return (values ptr nil nil))
                                            (prog1
                                              input
                                              (setq current-input nil)))
                      ptr               (+ ptr 2)))
             (4 (progn
                  (return (values (+ ptr 2) p1 nil))))
             (5 (if (not (eq 0 p1))
                    (setf ptr p2)
                    (setf ptr (+ ptr 3))))
             (6 (if (eq 0 p1)
                    (setf ptr p2)
                    (setf ptr (+ ptr 3))))
             (7 (setf (aref program d3) (if (< p1 p2) 1 0)
                      ptr               (+ ptr 4)))
             (8 (setf (aref program d3) (if (eql p1 p2) 1 0)
                      ptr               (+ ptr 4)))))
    (finally (return nil))))

(defun day7-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (iter
    (for current-combination in (combinations '(5 6 7 8 9)))
    (let* ((program (map 'vector
                         #'read-from-string
                         (str:words (ppcre:regex-replace-all ","
                                                             (car input-elements)
                                                             " "))))
           (input-1 0)
           (input-2 nil)
           (input-3 nil)
           (input-4 nil)
           (input-5 nil)
           (amp-1 (copy-seq program))
           (ptr-1 0)
           (amp-1-not-halted t)
           (amp-2 (copy-seq program))
           (ptr-2 0)
           (amp-2-not-halted t)
           (amp-3 (copy-seq program))
           (ptr-3 0)
           (amp-3-not-halted t)
           (amp-4 (copy-seq program))
           (ptr-4 0)
           (amp-4-not-halted t)
           (amp-5 (copy-seq program))
           (ptr-5 0)
           (amp-5-not-halted t))
      (maximizing
       (iter
         (format t "Current combination: ~a.  Amps: ~a ~a ~a ~a ~a~%" current-combination amp-1-not-halted amp-2-not-halted amp-3-not-halted amp-4-not-halted amp-5-not-halted)
         (for (phase-1 phase-2 phase-3 phase-4 phase-5) = current-combination)
         (initially
          (destructuring-bind (phase-1 phase-2 phase-3 phase-4 phase-5) current-combination
            (multiple-value-bind (new-ptr output halted) (interpret amp-1 phase-1 ptr-1)
              (setq amp-1-not-halted (not halted))
              (when (not (null output))
                (setq input-2 output))
              (setq ptr-1 new-ptr))
            (format t "ptr-1: ~a~%" ptr-1)
            (multiple-value-bind (new-ptr output halted) (interpret amp-2 phase-2 ptr-2)
              (setq amp-2-not-halted (not halted))
              (when (not (null output))
                (setq input-3 output))
              (setq ptr-2 new-ptr))
            (multiple-value-bind (new-ptr output halted) (interpret amp-3 phase-3 ptr-3)
              (setq amp-3-not-halted (not halted))
              (when (not (null output))
                (setq input-4 output))
              (setq ptr-3 new-ptr))
            (multiple-value-bind (new-ptr output halted) (interpret amp-4 phase-4 ptr-4)
              (setq amp-4-not-halted (not halted))
              (when (not (null output))
                (setq input-5 output))
              (setq ptr-4 new-ptr))
            (multiple-value-bind (new-ptr output halted) (interpret amp-5 phase-5 ptr-5)
              (setq amp-5-not-halted (not halted))
              (when (not (null output))
                (setq input-1 output))
              (setq ptr-5 new-ptr))))
         (while (or amp-1-not-halted amp-2-not-halted amp-3-not-halted amp-4-not-halted amp-5-not-halted))
         (when amp-1-not-halted
           (format t "input-1: ~a ptr-1: ~a~%" input-1 ptr-1)
           (multiple-value-bind (new-ptr output halted) (interpret amp-1 input-1 ptr-1)
             (setq amp-1-not-halted (not halted))
             (when (not (null output))
               (setq input-2 output))
             (setq ptr-1 new-ptr))
           (format t "ptr-1: ~a~%" ptr-1))
         (when amp-2-not-halted
           (multiple-value-bind (new-ptr output halted) (interpret amp-2 input-2 ptr-2)
             (setq amp-2-not-halted (not halted))
             (when (not (null output))
               (setq input-3 output))
             (setq ptr-2 new-ptr)))
         (when amp-3-not-halted
           (multiple-value-bind (new-ptr output halted) (interpret amp-3 input-3 ptr-3)
             (setq amp-3-not-halted (not halted))
             (when (not (null output))
               (setq input-4 output))
             (setq ptr-3 new-ptr)))
         (when amp-4-not-halted
           (multiple-value-bind (new-ptr output halted) (interpret amp-4 input-4 ptr-4)
             (setq amp-4-not-halted (not halted))
             (when (not (null output))
               (setq input-5 output))
             (setq ptr-4 new-ptr)))
         (when amp-5-not-halted
           (multiple-value-bind (new-ptr output halted) (interpret amp-5 input-5 ptr-5)
             (setq amp-5-not-halted (not halted))
             (when (not (null output))
               (setq input-1 output))
             (setq ptr-5 new-ptr)))
         (finally (return input-1)))))))

;; Wrong: 220368024735001439955553462308893926500880811458961720286487807810820918101593844984415875275146995712549906622451595534689521619598499397046544

;; Scratch area:

(progn
  (print "********** SCRATCH **********
")
  (let ((input-1 '("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
        (expected-1 43210)
        (input-2 '("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
        (expected-2 139629729))
    (format t "
Part 1:
Expected: ~s
     Got: ~s
" expected-1 (day7-part-1 input-1))
    (format t "
Part 2:
Expected: ~s
     Got: ~s
" expected-2 (day7-part-2 input-2))
    ))

;; Run the solution:

(progn
  (print "
********** OUTPUT **********
")
  (let ((input-1 (file-lines "day7-part-1"))
        (input-2 (file-lines "day7-part-1")))
    (print (day7-part-1 input-1))
    (print (day7-part-2 input-2))))

