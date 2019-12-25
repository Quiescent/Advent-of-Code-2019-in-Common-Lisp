;;; day25 --- My solution to day25 -*-

;;; Commentary:
;; My solution to advent of code: day25

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-heap")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "hash-table-computer.lisp")

(defpackage :day25
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash-table-computer)
  (:use :hash)
  (:use :iter))

(in-package :day25)

;; # PART 1:

(defun day25-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let ((program         (parse-computer-registers input-elements))
        (*relative-base* 0)
        (ptr             0)
        (stdout          nil)
        (stderr          nil)
        (trying-to-read  nil))
    (labels
        ((read-output ()
           (iter
             (while (not trying-to-read))
             (multiple-value-bind (new-ptr output halted read-value)
                 (interpret program nil ptr)
               (when halted
                 (format t "~a~%" (map 'string #'identity (reverse stdout)))
                 (error (format nil "Computer halted!")))
               (when output
                 (push (code-char output) stdout))
               (setf ptr new-ptr)
               (when read-value
                 (setf trying-to-read t)))
             (finally
              (progn
                (format t "~a~%" (map 'string #'identity (reverse stdout)))
                (setf stdout nil)
                (setf trying-to-read nil)))))
         (input-command ()
           (let ((command (concatenate 'string (nstring-downcase (format nil "~a" (read-line))) (string #\newline))))
             (format t "command: ~a~%" command)
             (iter
               (for char in-string command)
               (multiple-value-bind (new-ptr output halted read-value)
                   (interpret program (char-code char) ptr)
                 (when halted
                   (error (format nil "Computer halted!")))
                 (when output
                   (push (code-char output) stderr))
                 (setf ptr new-ptr))))))
      (iter
        (read-output)
        (input-command)))))

;; # PART 2:

(defun day25-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  )

;; Scratch area:

;; (progn
;;   (format t "~%********** SCRATCH **********~%")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "~%Part 1:~%Expected: ~s~%     Got: ~s~%" expected-1 (day25-part-1 input-1))
;;     (format t "~%Part 2:~%Expected: ~s~%     Got: ~s~%" expected-2 (day25-part-2 input-2))))

;; Run the solution:

(progn
  (format t "~%********** OUTPUT **********~%")
  (let ((input-1 (file-lines "day25-part-1"))
        (input-2 (file-lines "day25-part-1")))
    (format t "~%Part 1: ~s~%" (day25-part-1 input-1))
    (format t "~%Part 2: ~s~%" (day25-part-2 input-2))))

