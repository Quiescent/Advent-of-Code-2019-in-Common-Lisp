(ql:quickload "iterate")

(defpackage :halting-computer
  (:use :common-lisp)
  (:use :iter)
  (:export interpret)
  (:export parse-computer-registers)
  (:export *relative-base*))

(in-package :halting-computer)

(defun parse-computer-registers (input-elements)
  (let ((program (map 'vector
                      #'read-from-string
                      (str:words (ppcre:regex-replace-all ","
                                                          (car input-elements)
                                                          " "))))
        (bigger-program (make-array '(10000000) :initial-element 0)))
    (iter (for i from 0 below (length program))
          (declare (type fixnum i))
          (for register-value = (aref program i))
          (declare (type fixnum register-value))
          (setf (aref bigger-program i) register-value)
          (finally (return bigger-program)))))

(defun arg-1-mode (x)
  (declare (type fixnum x))
  (case (floor (mod x 1000) 100)
    (2 'relative)
    (1 'immediate)
    (0 'position)))

(defun arg-2-mode (x)
  (declare (type fixnum x))
  (case (floor (mod x 10000) 1000)
    (2 'relative)
    (1 'immediate)
    (0 'position)))

(defun arg-3-mode (x)
  (declare (type fixnum x))
  (case (floor x 10000)
    (2 'relative)
    (1 'immediate)
    (0 'position)))

(defun get-arg-1 (ptr program)
  (declare (type fixnum ptr))
  (let ((op (aref program ptr))
        (p1 (aref program (+ ptr 1))))
    (declare (type fixnum op))
    (declare (type fixnum p1))
    (case (arg-1-mode op)
      (immediate p1)
      (position  (aref program p1))
      (relative  (aref program (+ p1 *relative-base*))))))

(defun get-arg-2 (ptr program)
  (declare (type fixnum ptr))
  (let ((op (aref program ptr))
        (p2 (aref program (+ ptr 2))))
    (declare (type fixnum op))
    (declare (type fixnum p2))
    (case (arg-2-mode op)
      (immediate p2)
      (position  (aref program p2))
      (relative  (aref program (+ p2 *relative-base*))))))

(defun get-arg-3 (ptr program)
  (declare (type fixnum ptr))
  (let ((op (aref program ptr))
        (p3 (aref program (+ ptr 3))))
    (declare (type fixnum op))
    (declare (type fixnum p3))
    (case (arg-3-mode op)
      (immediate p3)
      (position  (aref program p3))
      (relative  (aref program (+ p3 *relative-base*))))))

(defun get-dest-1 (ptr program)
  (declare (type fixnum ptr))
  (let ((op (aref program ptr))
        (p1 (aref program (+ ptr 1))))
    (declare (type fixnum op))
    (declare (type fixnum p1))
    (or (case (arg-1-mode op)
          (position  p1)
          (relative  (+ p1 *relative-base*)))
        p1)))

(defun get-dest-2 (ptr program)
  (declare (type fixnum ptr))
  (let ((op (aref program ptr))
        (p2 (aref program (+ ptr 2))))
    (declare (type fixnum op))
    (declare (type fixnum p2))
    (or (case (arg-2-mode op)
          (position  p2)
          (relative  (+ p2 *relative-base*)))
        p2)))

(defun get-dest-3 (ptr program)
  (declare (type fixnum ptr))
  (let ((op (aref program ptr))
        (p3 (aref program (+ ptr 3))))
    (declare (type fixnum op))
    (declare (type fixnum p3))
    (or (case (arg-3-mode op)
          (position  p3)
          (relative  (+ p3 *relative-base*)))
        p3)))

;; Uses anaphors for p1..p3, d1..d3 program and ptr.
;; Only valid in this package!!
(defmacro defcpu (ops)
  `(let ((p1 (let ((res)) (lambda () (or res (setf res (get-arg-1 ptr program))))))
         (p2 (let ((res)) (lambda () (or res (setf res (get-arg-2 ptr program))))))
         (p3 (let ((res)) (lambda () (or res (setf res (get-arg-3 ptr program))))))
         (d1 (let ((res)) (lambda () (or res (setf res (get-dest-1 ptr program))))))
         (d2 (let ((res)) (lambda () (or res (setf res (get-dest-2 ptr program))))))
         (d3 (let ((res)) (lambda () (or res (setf res (get-dest-3 ptr program)))))))
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

(defvar *relative-base*)

(defun interpret (program input ptr)
  (iter
    (for op = (aref program ptr))
    (declare (type fixnum op))
    (when (eq op 99)
      (format t "BAILING!~%")
      (return (values ptr nil t nil)))
    (with current-input = input)
    (defcpu ((1 (setf (aref program d3) (+ p1 p2)
                      ptr               (+ ptr 4)))
             (2 (setf (aref program d3) (* p1 p2)
                      ptr               (+ ptr 4)))
             (3 (setf (aref program d1) (if (null current-input)
                                            (progn
                                              (format t "Reading...~%")
                                              (return (values ptr nil nil t)))
                                            (prog1
                                              input
                                              (setq current-input nil)))
                      ptr               (+ ptr 2)))
             (4 (return (values (+ ptr 2) p1 nil nil)))
             (5 (if (not (eq 0 p1))
                    (setf ptr p2)
                    (setf ptr (+ ptr 3))))
             (6 (if (eq 0 p1)
                    (setf ptr p2)
                    (setf ptr (+ ptr 3))))
             (7 (setf (aref program d3) (if (< p1 p2) 1 0)
                      ptr               (+ ptr 4)))
             (8 (setf (aref program d3) (if (eql p1 p2) 1 0)
                      ptr               (+ ptr 4)))
             (9 (setf *relative-base* (+ *relative-base* p1)
                      ptr             (+ ptr 2)))))
    (finally (return nil))))
