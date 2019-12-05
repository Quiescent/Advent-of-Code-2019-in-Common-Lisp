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
       ,@(mapcar (lambda (op) (list (car op) (print (replace-anaphors (cadr op))))) ops))))

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

(defun interpret (program)
  (iter
    (with ptr = 0)
    (for op = (aref program ptr))
    (when (eq op 99)
      (return))
    (defcpu ((1 (setf (aref program d3) (+ p1 p2)
                      ptr               (+ ptr 4)))
             (2 (setf (aref program d3) (* p1 p2)
                      ptr               (+ ptr 4)))
             (3 (setf (aref program d3) (progn (format t "Please input a number: ") (read))
                      ptr               (+ ptr 2)))
             (4 (progn
                  (print p1)
                  (setf ptr (+ ptr 2))))
             (5 (if (not (eq 0 p1))
                    (setf ptr p2)
                    (setf ptr (+ ptr 3))))
             (6 (if (eq 0 p1)
                    (setf ptr p2)
                    (setf ptr (+ ptr 3))))
             (7 (setf (aref program d3) (if (< p1 p2) 1 0)
                      ptr               (+ ptr 4)))
             (8 (setf (aref program d3) (if (eql p1 p2) 1 0)
                      ptr               (+ ptr 4)))))))
