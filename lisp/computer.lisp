(ql:quickload "iterate")

(defpackage :computer
  (:use :common-lisp)
  (:use :iter)
  (:export interpret)
  (:export parse-computer-registers))

(in-package :computer)

(defun parse-computer-registers (lines-numbers)
  (map 'vector #'identity (car lines-numbers)))

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

(defun interpret (program)
  (iter
    (with ptr = 0)
    (when (or (> ptr (length program))
              (< ptr 0))
      (error (format nil "Pointer: ~a is out of bounds!" ptr)))
    (for op = (aref program ptr))
    (for op-type = (mod op 10))
    (when (eq op 99)
      (return))
    (for p1 = (let ((res)) (lambda () (or res (setf res (get-arg-1 ptr program))))))
    (for p2 = (let ((res)) (lambda () (or res (setf res (get-arg-2 ptr program))))))
    (for p3 = (let ((res)) (lambda () (or res (setf res (get-arg-3 ptr program))))))
    (for d1 = (let ((res)) (lambda () (or res (setf res (aref program (+ ptr 1)))))))
    (for d2 = (let ((res)) (lambda () (or res (setf res (aref program (+ ptr 2)))))))
    (for d3 = (let ((res)) (lambda () (or res (setf res (aref program (+ ptr 3)))))))
    (case op-type
      (1 (setf (aref program (funcall d3)) (+ (funcall p1) (funcall p2))
                ptr                        (+ ptr 4)))
      (2 (setf (aref program (funcall d3)) (* (funcall p1) (funcall p2))
               ptr                         (+ ptr 4)))
      (3 (setf (aref program (funcall d3)) (progn (format t "Please input a number: ") (read))
               ptr                         (+ ptr 2)))
      (4 (progn
           (print (funcall p1))
           (setf ptr (+ ptr 2))))
      (5 (if (not (eq 0 (funcall p1)))
             (setf ptr (funcall p2))
             (setf ptr (+ ptr 3))))
      (6 (if (eq 0 (funcall p1))
             (setf ptr (funcall p2))
             (setf ptr (+ ptr 3))))
      (7 (setf (aref program (funcall d3)) (if (< (funcall p1) (funcall p2)) 1 0)
               ptr                         (+ ptr 4)))
      (8 (setf (aref program (funcall d3)) (if (eql (funcall p1) (funcall p2)) 1 0)
               ptr                         (+ ptr 4))))))
