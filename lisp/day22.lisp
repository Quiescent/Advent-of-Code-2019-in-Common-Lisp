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
  ;; Remember the plus one for the amount of cards!!!
  (let* ((number-of-cards
          ;;10007
          119315717514047
           )
         (all-instructions (map 'vector #'parse-line-1 input-elements))
         (instructions-length (length all-instructions))
         (ticks 0)
         (required-applications (iter (for i from 0 below 64)
                                      (when (logbitp i 101741582076661)
                                        (collecting i)))
           ))
    (declare (type unsigned-byte number-of-cards))
    (declare (type unsigned-byte ticks))
    (declare (type unsigned-byte instructions-length))
    (labels
        ((cut (number idx)
           (declare (type fixnum idx))
           (declare (type fixnum number))
           (let* ((cut (if (< number 0)
                           (+ number-of-cards number)
                           number))
                  (cut-idx (+ idx cut)))
             ;; (format t "Cutting: Cut: ~a, idx: ~a, number-of-cards: ~a~%"
             ;;         cut idx number-of-cards)
             (declare (type unsigned-byte cut))
             (declare (type unsigned-byte cut-idx))
             (mod cut-idx number-of-cards)))
         (inc (number idx)
           (declare (type fixnum idx))
           (declare (type fixnum number))
           ;;(mod (* number idx) number-of-cards)
           (solve-modulo number-of-cards idx number)
           )
         (rev (idx)
           (declare (type fixnum idx))
           (- (1- number-of-cards) idx))
         (solve (x i)
           (declare (type fixnum i))
           (if (eq i -1)
               x
               (let* ((idx (mod i instructions-length))
                      (looking-for (destructuring-bind (op . number) (aref all-instructions idx)
                                     (case op
                                       (cut (cut number x))
                                       (inc (inc number x))
                                       (rev (rev x))))))
                 ;;(format t "idx: ~a~%" idx)
                 (when (eq idx 0)
                   (incf ticks)
                   (format t "looking-for: ~a~%" looking-for)
                   ;;(format t "ticks: ~a~%" ticks)
                   )
                 ;; (format t "looking-for: ~a~%" x)
                 ;; (format t "looking-for-next: ~a~%" looking-for);
                 (solve looking-for (1- i))))))
      (format t "Initially: ~s~%" (solve 2020 (1- instructions-length)))
      (iter
        (with poly = (cons 0 1))
        (for i from 0 below instructions-length)
        (for (c . x) = poly)
        (destructuring-bind (op . number) (aref all-instructions i)
          (case op
            (cut (setf poly (cons (- c number
                                     ;; (if (< number 0)
                                       ;;     (+ number-of-cards number)
                                       ;;     number)
                                     )
                                  x)))
            (inc (setf poly (cons (* c number)
                                  (* x number))))
            (rev (setf poly (cons (- (1- number-of-cards) c) x)))))
        (finally
         (return
           (destructuring-bind (c . x) poly
             ;; (format t "new sol: ~a~%" (mod (+ c (* x 2020)) number-of-cards))
             ;;(mod (+ c (* 2019 x)) number-of-cards)
             ;; (iter
             ;;   (for i from 0 below 1000000000)
             ;;   (when (eq 1 (mod (* i x) number-of-cards))
             ;;     (return (mod (* i (mod (- 2020 c) number-of-cards)) number-of-cards))))
             (let ((power-applications (make-array '(64) :initial-element nil))
                   (inverse (multiplicative-inverse x number-of-cards)))
               (setf (aref power-applications 0)
                     (lambda (x _)
                       (mod (* inverse (mod (- x c) number-of-cards)) number-of-cards)))
               (iter
                 (for i from 1 below 64)
                 ;;(format t "Setting up ~a to call ~a~%" i (1- i))
                 (setf (aref power-applications i)
                       (lambda (x i)
                         (let* ((f (aref power-applications (1- i))))
                           (funcall f (funcall f x (1- i)) (1- i))))))
               (iter
                 (with ans = 2020)
                 (for idx in required-applications)
                 (format t "idx: ~a~%" idx)
                 (setf ans (funcall (aref power-applications idx) ans idx))
                 (finally (return ans))))))))

      ;; (solve 2020 (1- (* 1 instructions-length))
      ;;        ;;(* 101741582076661 (1- instructions-length))
      ;;        ;;(* 1 (1- instructions-length))
      ;;        )
      )
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

;; https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
;; function inverse(a, n)
;;     t := 0;     newt := 1;
;;     r := n;     newr := a;
;;     while newr ≠ 0
;;         quotient := r div newr
;;         (t, newt) := (newt, t - quotient * newt)
;;         (r, newr) := (newr, r - quotient * newr)
;;     if r > 1 then return "a is not invertible"
;;     if t < 0 then t := t + n
;;     return t

(defun multiplicative-inverse (x y)
  (let ((z     0)
        (new_z 1)
        (r     y)
        (new_r x))
    (iter
      (while (not (eq 0 new_r)))
      (for q = (floor r new_r))
      (for temp_r = new_r)
      (for temp_z = new_z)
      (setf new_z (- z (* q new_z))
            new_r (- r (* q new_r)))
      (setf r temp_r
            z temp_z))
    (when (> r 1)
      (error "not divisible"))
    (if (< z 0)
        (+ z y)
        z)))

;; Too low: 44082672264648
;;
;; Got to tick: 527600000 before deciding that it wont make it before
;; christmas(!)

(defun parse-line-1 (line)
  (let ((number (car (mapcar #'read-from-string (ppcre:all-matches-as-strings "[-0-9]+" line)))))
    (cond
      ((ppcre:all-matches-as-strings "cut" line)
       (cons 'cut number))
      ((ppcre:all-matches-as-strings "increment" line)
       (cons 'inc number))
      ((ppcre:all-matches-as-strings "stack" line)
       (cons 'rev nil)))))

(defun parse-line (line len)
  (let ((number (car (mapcar #'read-from-string (ppcre:all-matches-as-strings "[-0-9]+" line)))))
    (declare (type fixnum len))
    (cond
      ((ppcre:all-matches-as-strings "cut" line)
       (lambda (idx) (let ((cut (if (< number 0)
                                    (+ len number)
                                    number)))
                       ;; (format t "Cutting: Cut: ~a, idx: ~a, len: ~a~%"
                       ;;         cut idx len)
                       (declare (type fixnum idx))
                       (declare (type fixnum cut))
                       (declare (type fixnum number))
                       (mod (+ idx cut) len))))
      ((ppcre:all-matches-as-strings "increment" line)
       (lambda (idx) (let ((idx-2 idx))
                       ;; (format t "Incing: len: ~a, num: ~a, mod: ~a~%"
                       ;;         len idx number)
                       (declare (type fixnum idx-2))
                       (declare (type fixnum number))
                       (solve-modulo len idx-2 number))))
      ((ppcre:all-matches-as-strings "stack" line)
       (lambda (idx) (let ((idx-2 idx))
                       ;; (format t "Reving: rev: ~a, idx: ~a~%" idx len)
                       (declare (type fixnum idx-2))
                       (- (1- len) idx-2)))))))


(defun solve-modulo (len res grp)
  (declare (type fixnum res))
  (iter
    (with pos = 0)
    (declare (type fixnum pos))
    (with i = 0)
    (declare (type fixnum i))
    (while (< pos len))
    ;; Is there an even multiple of grp to get to res
    (multiple-value-bind (d m) (floor (- res i) grp)
      (declare (type fixnum d))
      (declare (type fixnum m))
     (when (eq 0 m)
       (return (+ pos d))))
    (multiple-value-bind (d) (floor (- len i) grp)
      (declare (type fixnum d))
      (incf pos (1+ d))
      (incf i (* (1+ d) grp))
      (setf i (mod i len))))
  ;; (iter
  ;;   (with i = 0)
  ;;   (while (< i len))
  ;;   (when (eq 0 (mod i res)))
  ;;   (when (eq res (mod (* i grp) len))
  ;;     (return i)))
  )

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
