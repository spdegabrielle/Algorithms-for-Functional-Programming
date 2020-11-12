;;; Breadth-first traversal

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 6, 2000
;;; last revised January 9, 2017

(define-library (afp breadth-first-traversal)
  (export breadth-first-visit breadth-first-traversal)
  (import (afp primitives)
          (only (afp constant-procedures) create)
          (only (afp sets) fast-put-into-set set fold-set member?)
          (only (afp buffers)
                buffer put-into-buffer take-from-buffer empty-buffer?)
          (only (afp graphs) vertices neighbors))
  (begin

    ;; ===== breadth-first-visit ==========================================
    ;; (alpha -> set(alpha)), (alpha, beta ... -> beta ...) ->
    ;; nabes                  operate
    ;;                            (alpha, set(alpha), beta ... -> beta ...)
    ;;                             vertex visited     so-far

    ;; The breadth-first-visit procedure constructs a procedure that
    ;; traverses part of a graph by visiting in succession each of the
    ;; vertices that is reachable from vertex, starting with vertex itself
    ;; and proceeding in breadth-first order through its neighbors (as
    ;; determined by nabes), the neighbors of those neighbors, and so on.
    ;; If vertex is a member of visited, the constructed procedure returns
    ;; visited and the elements of so-far; otherwise, it invokes operate
    ;; (giving it vertex and the results of computations undertaken in
    ;; previous visits).


    ;; Preconditions:
    ;;   nabes can receive vertex.
    ;;   nabes can receive any element of any result of an invocation of
    ;;     nabes.
    ;;   operate can receive vertex and the elements of so-far.
    ;;   operate can receive any element of any result of an invocation
    ;;     of nabes, along with the results of any invocation of operate. 

    (define (breadth-first-visit nabes operate)
      (lambda (vertex visited . so-far)
        (apply (rec (visit visited waiting . so-far)
                 (if (empty-buffer? waiting)
                     (apply values visited so-far)
                     (receive (chosen others) (take-from-buffer waiting)
                       (if (member? chosen visited)
                           (apply visit visited others so-far)
                           (receive new-so-far (apply operate chosen so-far)
                             (apply visit
                                    (fast-put-into-set chosen visited)
                                    ((fold-set (create others)
                                               put-into-buffer)
                                     (nabes chosen))
                                    new-so-far))))))
               visited (buffer vertex) so-far)))

    ;; ===== breadth-first-traversal ======================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            operate
    ;;                                          (graph(beta, any) -> alpha)
    ;;                                           graph

    ;; The breadth-first-traversal procedure constructs a procedure that
    ;; traverses graph in breadth-first order, starting from an arbitrarily
    ;; chosen vertex, returning the results obtained by invoking base to
    ;; obtain some initial values, and then transforming these values by
    ;; applying operate to each vertex and the results of previous such
    ;; operations.

    ;; Preconditions:
    ;;   operate can receive any vertex of graph and the results of any
    ;;     invocation of base.
    ;;   operate can receive any vertex of graph and the results of any
    ;;     invocation of operate.

    (define (breadth-first-traversal base operate)
      (let ((new-base (receive starters (base)
                        (apply create (set) starters))))
        (lambda (graph)
          (receive (visited . finals)
                   ((fold-set new-base (breadth-first-visit (neighbors graph)
                                                            operate))
                    (vertices graph))
            (delist finals)))))))

;;; copyright (C) 2011, 2017 John David Stone

;;; This program is free software.  You may redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation -- either version 3 of the License, or (at
;;; your option) any later version.  A copy of the GNU General Public
;;; License is available on the World Wide Web at
;;;
;;;                http://www.gnu.org/licenses/gpl.html
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY -- without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
