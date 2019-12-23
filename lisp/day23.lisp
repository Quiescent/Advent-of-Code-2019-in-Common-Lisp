;;; day23 --- My solution to day23 -*-

;;; Commentary:
;; My solution to advent of code: day23

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "stepping-computer.lisp")

(defpackage :day23
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :stepping-computer)
  (:use :hash)
  (:use :iter))

(in-package :day23)

;; # PART 1:

(defun day23-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((computers     (iter (for i from 0 below 50)
                             (with result = (make-hash-table))
                             (setf (gethash i result) (parse-computer-registers input-elements))
                             (finally (return result))))
        (input-queues  (make-hash-table))
        (offsets       (make-array '(50) :initial-element 0))
        (ptrs          (make-array '(50) :initial-element 0))
        (output-queues (make-hash-table)))
    (iter
      (for i from 0 below 50)
      (push i (gethash i input-queues)))
    ;; Assume: that the computers don't need to operate in lock-step
    (iter outer
      (for loop-counter from 0 below 1000000)
      ;;(format t "Ptrs: ~a~%" ptrs)
      (iter
        (for i from 0 below 50)
        (let ((*relative-base* (aref offsets i))
              (ptr             (aref ptrs i))
              (program         (gethash i computers))
              (input-queue     (gethash i input-queues))
              (output-queue    (gethash i output-queues)))
          (multiple-value-bind (new-ptr output halted read-value)
              (interpret program (or (car input-queue) -1) ptr)
            (when halted
              (error (format nil "Computer ~a halted!" i)))
            (when output
              (format t "Producing output (~a)...~%" output)
              (push output output-queue)
              (setf (gethash i output-queues) output-queue))
            (setf ptr new-ptr)
            (when read-value
              (format t "Read a value (~a)...~%" (or (car input-queue) -1))
              (setf (gethash i input-queues) (cdr input-queue))))
          (when (eq (length output-queue) 3)
            (let* ((destination (car (last output-queue)))
                   (dest-queue  (gethash destination input-queues)))
              (format t "[~a] Sending: ~a~%" i (reverse output-queue))
              (when (eq destination 255)
                (return-from outer (car output-queue)))
              (setf dest-queue (nconc dest-queue (list (cadr output-queue)
                                                       (car  output-queue))))
              (setf (gethash destination input-queues) dest-queue))
            (setf (gethash i output-queues) nil))
          (setf (aref offsets i)          *relative-base*
                (aref ptrs    i)          ptr))))))

;; # PART 2:

(defun day23-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let ((computers     (iter (for i from 0 below 50)
                             (with result = (make-hash-table))
                             (setf (gethash i result) (parse-computer-registers input-elements))
                             (finally (return result))))
        (input-queues  (make-hash-table))
        (offsets       (make-array '(50) :initial-element 0))
        (ptrs          (make-array '(50) :initial-element 0))
        (output-queues (make-hash-table))
        (nat-x         nil)
        (nat-y         nil)
        (last-y-0      nil)
        (current-y-0   nil))
    (iter
      (for i from 0 below 50)
      (push i (gethash i input-queues)))
    ;; Assume: that the computers don't need to operate in lock-step
    (iter outer
          (for loop-counter from 0 below 1000000)
          ;;(format t "Ptrs: ~a~%" ptrs)
          (iter
            (for i from 0 below 50)
            (let ((*relative-base* (aref offsets i))
                  (ptr             (aref ptrs i))
                  (program         (gethash i computers))
                  (input-queue     (gethash i input-queues))
                  (output-queue    (gethash i output-queues)))
              (multiple-value-bind (new-ptr output halted read-value)
                  (interpret program (or (car input-queue) -1) ptr)
                (when halted
                  (error (format nil "Computer ~a halted!" i)))
                (when output
                  (format t "Producing output (~a)...~%" output)
                  (push output output-queue)
                  (setf (gethash i output-queues) output-queue))
                (setf ptr new-ptr)
                (when read-value
                  (format t "Read a value (~a)...~%" (or (car input-queue) -1))
                  (setf (gethash i input-queues) (cdr input-queue))))
              (when (eq (length output-queue) 3)
                (let* ((destination (car (last output-queue)))
                       (dest-queue  (gethash destination input-queues)))
                  (format t "[~a] Sending: ~a~%" i (reverse output-queue))
                  (when (eq destination 255)
                    (format t "Sending ~a for Y to NAT~%" (car output-queue))
                    (setf nat-x (cadr output-queue)
                          nat-y (car  output-queue)))
                  (when (not (eq destination 255))
                    (setf dest-queue (nconc dest-queue (list (cadr output-queue)
                                                             (car  output-queue))))
                    (setf (gethash destination input-queues) dest-queue)))
                (when (eq 50 (iter (for (key value) in-hashtable input-queues)
                                   (counting (null value))))
                  (format t "Network is idle.  Sending (~a, ~a) to 0~%" nat-x nat-y)
                  (let ((dest-queue (gethash 0 input-queues)))
                    (setf last-y-0    current-y-0
                          current-y-0 nat-y)
                    (when (eq last-y-0 current-y-0)
                      (return-from outer current-y-0))
                    (setf dest-queue (nconc dest-queue (list nat-x nat-y)))
                    (setf (gethash 0 input-queues) dest-queue)))
                (setf (gethash i output-queues) nil))
              (setf (aref offsets i)          *relative-base*
                    (aref ptrs    i)          ptr))))))

;; Scratch area:

;; (progn
;;   (format t "~%********** SCRATCH **********~%")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "~%Part 1:~%Expected: ~s~%     Got: ~s~%" expected-1 (day23-part-1 input-1))
;;     (format t "~%Part 2:~%Expected: ~s~%     Got: ~s~%" expected-2 (day23-part-2 input-2))))

;; Run the solution:

(progn
  (format t "~%********** OUTPUT **********~%")
  (let ((input-1 (file-lines "day23-part-1"))
        (input-2 (file-lines "day23-part-1")))
    (format t "~%Part 1: ~s~%" (day23-part-1 input-1))
    (format t "~%Part 2: ~s~%" (day23-part-2 input-2))))

