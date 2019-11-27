(ql:quickload "iterate")

(defpackage :graph
  (:use :common-lisp)
  (:use :iter)
  (:export tuples-to-graph)
  (:export reverse-graph))

(in-package :graph)

(defun tuples-to-graph (tuples)
  (let ((graph (make-hash-table :test #'equal)))
    (dolist (tuple tuples graph)
      (when (consp (cdr tuple))
        (error "Expected tuple."))
      (let ((from (car tuple))
            (to   (cdr tuple)))
        (setf (gethash from graph)
              (delete-duplicates (push to (gethash from graph))))))))

(defun reverse-graph (graph)
  (let ((reversed (make-hash-table :test #'equal)))
    (iter
      (for (from tos) in-hashtable graph)
      (iter
        (for to in tos)
        (setf (gethash to reversed)
              (adjoin from (gethash to reversed))))
      (finally (return reversed)))))

(in-package :iter)

(defvar *dfs-bailout-count* 100000)


;; Graph

(defmacro-driver (FOR var DFS-ACROSS-GRAPH graph &sequence)
  "Search GRAPH depth-first binding VAR to the current node.

Start search at the value of the FROM keyword unless it's not
specified.  If FROM isn't specified then consider all nodes for the
search.

Supports the BY keyword which should be an expression which selects a
node from the list CANDIDATE-NODES.  You should avoid modifying the
list because it's used internally by the search."
  (let ((g                  (gensym))
        (count              (gensym))
        (kwd                (if generate 'generate 'for))
        (non-default-search (not (eq 1 by))))
    (cond
      ((not (null upfrom))     (error "DFS-ACROSS-GRAPH doesn't support UPFROM"))
      ((not (null downfrom))   (error "DFS-ACROSS-GRAPH doesn't support DOWNFROM"))
      ((not (null to))         (error "DFS-ACROSS-GRAPH doesn't support TO"))
      ((not (null downto))     (error "DFS-ACROSS-GRAPH doesn't support DOWNTO"))
      ((not (null above))      (error "DFS-ACROSS-GRAPH doesn't support ABOVE"))
      ((not (null below))      (error "DFS-ACROSS-GRAPH doesn't support BELOW"))
      ((not (null with-index)) (error "DFS-ACROSS-GRAPH doesn't support WITH-INDEX")))
    `(progn
       (with ,count          = 0)
       (with ,g              = ,graph)
       (with candidate-nodes = (if (null ,from)
                                   (iter (for (from to) in-hashtable ,g)
                                         (adjoining from))
                                   (list ,from)))
       (incf ,count)
       (when (> ,count ,*dfs-bailout-count*)
         (format t "DFS continued for more than ,*dfs-bailout-count*.  This is probably an error.")
         (finish))
       (while candidate-nodes)
       (initially (setq ,var (if ,non-default-search
                                 ,by
                                 (car candidate-nodes))))
       (,kwd ,var next (if ,non-default-search
                           ,by
                           (car candidate-nodes)))
       (setq candidate-nodes (delete ,var candidate-nodes))
       (setq candidate-nodes (append (gethash ,var ,g) candidate-nodes)))))

(defmacro-driver (FOR var DFS-ACROSS-GRAPH-WITHOUT-DUPLICATES graph &sequence)
  "Search GRAPH depth-first, starting at the value of the FROM keyword binding VAR.

Supports the BY keyword which should be an expression which selects a
node from the list CANDIDATE-NODES.  You should avoid modifying the
list because it's used internally by the search."
  (let ((g                  (gensym))
        (count              (gensym))
        (visited            (gensym))
        (kwd                (if generate 'generate 'for))
        (non-default-search (not (eq 1 by))))
    (cond
      ((not (null upfrom))     (error "DFS-ACROSS-GRAPH doesn't support UPFROM"))
      ((not (null downfrom))   (error "DFS-ACROSS-GRAPH doesn't support DOWNFROM"))
      ((not (null to))         (error "DFS-ACROSS-GRAPH doesn't support TO"))
      ((not (null downto))     (error "DFS-ACROSS-GRAPH doesn't support DOWNTO"))
      ((not (null above))      (error "DFS-ACROSS-GRAPH doesn't support ABOVE"))
      ((not (null below))      (error "DFS-ACROSS-GRAPH doesn't support BELOW"))
      ((not (null with-index)) (error "DFS-ACROSS-GRAPH doesn't support WITH-INDEX")))
    `(progn
       (with ,count          = 0)
       (with ,visited        = (make-hash-table :test #'equal))
       (with ,g              = ,graph)
       (with candidate-nodes = (if (null ,from)
                                   (iter (for (from to) in-hashtable ,g)
                                         (adjoining from))
                                   (list ,from)))
       (incf ,count)
       (when (> ,count ,*dfs-bailout-count*)
         (format t "DFS continued for more than ,*dfs-bailout-count*.  This is probably an error.")
         (finish))
       (while candidate-nodes)
       (initially (setq ,var (if ,non-default-search
                                 ,by
                                 (car candidate-nodes))))
       (,kwd ,var next (if ,non-default-search
                           ,by
                           (car candidate-nodes)))
       (setq candidate-nodes (delete ,var candidate-nodes))
       (when (gethash ,var ,visited)
         (next-iteration))
       (setf (gethash ,var ,visited) t)
       (setq candidate-nodes (union (gethash ,var ,g) candidate-nodes)))))

;; Scratch

#+nil
(progn
  (defvar temp-graph)
  (setq temp-graph (graph:tuples-to-graph '((#\a . #\b) (#\a . #\c) (#\c . #\d) (#\b . #\f) (#\b . #\c))))

  (print "Standard:")

  (iter
    (for x dfs-across-graph temp-graph from #\a)
    (print x))

  (print "")

  (iter
    (for x dfs-across-graph temp-graph from #\a by (car (last candidate-nodes)))
    (print x))

  (print "No duplicates:")

  (iter
    (for x dfs-across-graph-without-duplicates temp-graph from #\a)
    (print x))

  (print "with any start")

  (iter
    (for x dfs-across-graph temp-graph)
    (print x))

  (print "with any start and no duplicates")

  (iter
    (for x dfs-across-graph-without-duplicates temp-graph)
    (print x)))


;; Function
(defmacro-driver (FOR var DFS-ACROSS-FUNCTION f &sequence)
  "Search function f depth-first, starting at the value of the FROM keyword binding VAR.

Supports the BY keyword which should be an expression which selects a
node from the list CANDIDATE-NODES.  You should avoid modifying the
list because it's used internally by the search."
  (let ((count              (gensym))
        (kwd                (if generate 'generate 'for))
        (non-default-search (not (eq 1 by))))
    (cond
      ((not (null upfrom))     (error "DFS-ACROSS-FUNCTION doesn't support UPFROM"))
      ((not (null downfrom))   (error "DFS-ACROSS-FUNCTION doesn't support DOWNFROM"))
      ((not (null to))         (error "DFS-ACROSS-FUNCTION doesn't support TO"))
      ((not (null downto))     (error "DFS-ACROSS-FUNCTION doesn't support DOWNTO"))
      ((not (null above))      (error "DFS-ACROSS-FUNCTION doesn't support ABOVE"))
      ((not (null below))      (error "DFS-ACROSS-FUNCTION doesn't support BELOW"))
      ((not (null with-index)) (error "DFS-ACROSS-FUNCTION doesn't support WITH-INDEX"))
      ((not (functionp f))     (error "You can't DFS across a function which isn't a function!"))
      ((null from)             (error "you must specify where the search should starte with FROM")))
    `(progn
       (with ,count          = 0)
       (with candidate-nodes = (list ,from))
       (incf ,count)
       (when (> ,count ,*dfs-bailout-count*)
         (format t "DFS continued for more than ,*dfs-bailout-count*.  This is probably an error.")
         (finish))
       (while candidate-nodes)
       (initially (setq ,var (if ,non-default-search
                                 ,by
                                 (car candidate-nodes))))
       (,kwd ,var next (if ,non-default-search
                           ,by
                           (car candidate-nodes)))
       (setq candidate-nodes (delete ,var candidate-nodes))
       (setq candidate-nodes (append (funcall ,f ,var) candidate-nodes)))))

(defmacro-driver (FOR var DFS-ACROSS-FUNCTION-WITHOUT-DUPLICATES f &sequence)
  "Search function F depth-first, starting at the value of the FROM keyword binding VAR.

Supports the BY keyword which should be an expression which selects a
node from the list CANDIDATE-NODES.  You should avoid modifying the
list because it's used internally by the search."
  (let ((count              (gensym))
        (visited            (gensym))
        (kwd                (if generate 'generate 'for))
        (non-default-search (not (eq 1 by))))
    (cond
      ((not (null upfrom))     (error "DFS-ACROSS-FUNCTION doesn't support UPFROM"))
      ((not (null downfrom))   (error "DFS-ACROSS-FUNCTION doesn't support DOWNFROM"))
      ((not (null to))         (error "DFS-ACROSS-FUNCTION doesn't support TO"))
      ((not (null downto))     (error "DFS-ACROSS-FUNCTION doesn't support DOWNTO"))
      ((not (null above))      (error "DFS-ACROSS-FUNCTION doesn't support ABOVE"))
      ((not (null below))      (error "DFS-ACROSS-FUNCTION doesn't support BELOW"))
      ((not (null with-index)) (error "DFS-ACROSS-FUNCTION doesn't support WITH-INDEX"))
      ((not (functionp f))     (error "You can't DFS across a function which isn't a function!"))
      ((null from)             (error "you must specify where the search should starte with FROM")))
    `(progn
       (with ,count          = 0)
       (with ,visited        = (make-hash-table :test #'equal))
       (with candidate-nodes = (list ,from))
       (incf ,count)
       (when (> ,count ,*dfs-bailout-count*)
         (format t "DFS continued for more than ,*dfs-bailout-count*.  This is probably an error.")
         (finish))
       (while candidate-nodes)
       (initially (setq ,var (if ,non-default-search
                                 ,by
                                 (car candidate-nodes))))
       (,kwd ,var next (if ,non-default-search
                           ,by
                           (car candidate-nodes)))
       (setq candidate-nodes (delete ,var candidate-nodes))
       (when (gethash ,var ,visited)
         (next-iteration))
       (setf (gethash ,var ,visited) t)
       (setq candidate-nodes (union (funcall ,f ,var) candidate-nodes)))))

;#+nil
;; (progn
;;   (defun search-function (x)
;;     (if (or (> x 5)
;;             (< x 0))
;;         nil
;;         (list (1+ x) (+ x 2))))

;;   (print "Standard:")

;;   (iter
;;     (for x dfs-across-function #'search-function from 1)
;;     (print x))

;;   (print "")

;;   (iter
;;     (for x dfs-across-function #'search-function from 1 by (car (last candidate-nodes)))
;;     (print x))

;;   (print "No duplicates:")

;;   (iter
;;     (for x dfs-across-function-without-duplicates #'search-function from 1)
;;     (print x)))
