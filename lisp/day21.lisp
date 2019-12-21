;;; day21 --- My solution to day21 -*-

;;; Commentary:
;; My solution to advent of code: day21

;;; Code:

(ql:quickload "iterate")
(ql:quickload "cl-heap")
(ql:quickload "cl-ppcre")

(load "read-file.lisp")
(load "graph.lisp")
(load "debug.lisp")
(load "hash.lisp")
(load "hash-table-computer.lisp")

(defpackage :day21
  (:use :common-lisp)
  (:use :debug)
  (:use :graph)
  (:use :read-file)
  (:use :hash-table-computer)
  (:use :hash)
  (:use :iter))

(in-package :day21)

;; # PART 1:

(defun day21-part-1 (input-elements)
  "Run my solution to part one of the problem on the input in INPUT-ELEMENTS."
  (let* ((program (parse-computer-registers input-elements))
         (*relative-base* 0)
         program-copy
         (ptr 0)
         stderr)
    (labels ((read-output ()
               (progn
                 (setf stderr nil)
                 (iter
                   (multiple-value-bind (new-ptr output halted) (interpret program-copy nil ptr)
                     (when halted
                       (setf ptr new-ptr)
                       (format t "bailing!~%")
                       (return))
                     (when output (push (code-char output) stderr))
                     (setf ptr new-ptr)
                     (while output)))
                 (format t ">~%~a~%" (map 'string #'identity (reverse stderr)))))
             (attempt (instructions)
               (progn
                 (setf program-copy (copy-computer program))
                 (setf ptr 0)
                 (setf *relative-base* 0)
                 (read-output)
                 (iter
                   outer
                   (for instruction in (append instructions (list "WALK")))
                   (iter
                     (for char in-string instruction)
                     (format t "~a" char)
                     (multiple-value-bind (new-ptr output halted) (interpret program-copy (char-code char) ptr)
                       (when halted
                         (format t "Bailing!~%")
                         ;; (read-output)
                         (return-from outer nil))
                       (setf ptr new-ptr)))
                   (multiple-value-bind (new-ptr output halted reading run-count)
                       (interpret program-copy (char-code #\newline) ptr)
                     (when halted
                       (format t "Bailing!~%")
                       ;; (read-output)
                       (return-from outer nil))
                     (setf ptr new-ptr))
                   ;; (format t "~%")
                   ;; (format t "Instruction input~%")
                   ;; (read-output)
                   )
                 (multiple-value-bind (new-ptr output halted reading run-count)
                     (interpret program-copy (char-code #\newline) ptr)
                   (setf ptr new-ptr)
                   ;;(read-output)
                   (print "HERE")
                   (format t "run-count: ~a~%" run-count)
                   (if (and output (> output 1114112))
                       (progn
                         (format t "FOUND IT!")
                         (values t output))
                       (values nil run-count))))))
      (attempt '(;; Fourth is free and any prior are taken
                 "NOT A T"
                 "NOT B J"
                 "OR J T"
                 "NOT C J"
                 "OR T J"
                 "AND D J"
                 ))
      (read-output))))

;; Answer: 19356081 (from error back trace when trying to read XD)

(defun all-instructions ()
  (iter
    (with all-instructions = '())
    (for instruction in '("AND" "OR" "NOT"))
    (iter
      (for result in '("J" "T"))
      (iter
        (for argument in '("A" "B" "C" "D" "J" "T"))
        (push (concatenate 'string instruction " " argument " " result)
              all-instructions)))
    (finally (return all-instructions))))

#+nil
(multiple-value-bind (new-ptr ??? halted) (interpret program ??? ptr)
  (setf ptr new-ptr))

;; # PART 2:

(defun day21-part-2 (input-elements)
  "Run my solution to part two of the problem on the input in INPUT-ELEMENTS."
  (let* ((program (parse-computer-registers input-elements))
         (*relative-base* 0)
         program-copy
         (ptr 0)
         stderr)
    (labels ((read-output ()
               (progn
                 (setf stderr nil)
                 (iter
                   (multiple-value-bind (new-ptr output halted) (interpret program-copy nil ptr)
                     (when halted
                       (setf ptr new-ptr)
                       (format t "bailing!~%")
                       (return))
                     (when output (push (code-char output) stderr))
                     (setf ptr new-ptr)
                     (while output)))
                 (format t ">~%~a~%" (map 'string #'identity (reverse stderr)))))
             (attempt (instructions)
               (progn
                 (setf program-copy (copy-computer program))
                 (setf ptr 0)
                 (setf *relative-base* 0)
                 (read-output)
                 (iter
                   outer
                   (for instruction in (append instructions (list "RUN")))
                   (iter
                     (for char in-string instruction)
                     (format t "~a" char)
                     (multiple-value-bind (new-ptr output halted) (interpret program-copy (char-code char) ptr)
                       (when halted
                         (format t "Bailing!~%")
                         ;; (read-output)
                         (return-from outer nil))
                       (setf ptr new-ptr)))
                   (multiple-value-bind (new-ptr output halted reading run-count)
                       (interpret program-copy (char-code #\newline) ptr)
                     (when halted
                       (format t "Bailing!~%")
                       ;; (read-output)
                       (return-from outer nil))
                     (setf ptr new-ptr))
                   ;; (format t "~%")
                   ;; (format t "Instruction input~%")
                   ;; (read-output)
                   )
                 (multiple-value-bind (new-ptr output halted reading run-count)
                     (interpret program-copy (char-code #\newline) ptr)
                   (setf ptr new-ptr)
                   ;;(read-output)
                   (print "HERE")
                   (format t "run-count: ~a~%" run-count)
                   (if (and output (> output 1114112))
                       (progn
                         (format t "FOUND IT!")
                         (values t output))
                       (values nil run-count))))))
      (attempt
       '("NOT A T"
         "OR T J"
         "NOT B T"
         "OR T J"
         "NOT C T"
         "OR T J"
         "AND D J"

         "NOT A T"
         "AND A T"

         "OR E T"
         "OR F T"
         "AND E T"
         "OR H T"

         "AND T J"
))
      (read-output))))

;; 1141901823 (got from backtrace when it couldn't read it as a char)

;; ABCDEFGHI
;; #..##.##. => JUMP
;; ABCDEFGHI
;; ##.##.### => JUMP
;; ABCDEFGHI
;; ##.###### => JUMP
;; ABCDEFGHI
;; ##.#..### => JUMP
;; ABCDEFGHI
;; #..###### => JUMP
;; ABCDEFGHI
;; .######## => JUMP
;; ABCDEFGHI
;; ##.#.#..# => DON'T JUMP

;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########
;; ##########

(let (t-val j-val)
  )

;; Scratch area:

;; (progn
;;   (format t "~%********** SCRATCH **********~%")
;;   (let ((input-1 '())
;;         (expected-1 '())
;;         (input-2 '())
;;         (expected-2 '()))
;;     (format t "~%Part 1:~%Expected: ~s~%     Got: ~s~%" expected-1 (day21-part-1 input-1))
;;     (format t "~%Part 2:~%Expected: ~s~%     Got: ~s~%" expected-2 (day21-part-2 input-2))))

;; Run the solution:

(progn
  (format t "~%********** OUTPUT **********~%")
  (let ((input-1 (file-lines "day21-part-1"))
        (input-2 (file-lines "day21-part-1")))
    ;(format t "~%Part 1: ~s~%" (day21-part-1 input-1))
    (format t "~%Part 2: ~s~%" (day21-part-2 input-2))))
