;;; The Bellman-Ford algorithm

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created October 2, 2003
;;; last revised January 9, 2017

(define-library (afp shortest-paths Bellman-Ford)
  (export shortest-paths-from safe-shortest-paths-from)
  (import (afp primitives)
          (only (afp arithmetic) sub1)
          (only (afp constant-procedures) create)
          (only (afp natural-numbers) fold-natural)
          (only (afp sets) fold-set cardinality exists-in-set?)
          (only (afp tables) table)
          (only (afp graphs) arcs vertices)
          (only (afp paths) relaxable? relax))
  (begin

    ;; ===== shortest-paths-from ==========================================
    ;; graph(alpha, number), alpha ->
    ;; graph                 start
    ;;                              table(alpha, pair(number, list(alpha)))

    ;; The shortest-paths-from procedure constructs a path table giving the
    ;; shortest path from start to every vertex in graph that is reachable
    ;; from start.

    ;; Preconditions:
    ;;   graph contains no cycle of arcs with a negative path sum
    ;;     containing a vertex that is reachable from start.
    ;;   start is a vertex of graph.

    (define (shortest-paths-from graph start)
      (let ((sagaro (arcs graph))
            (start-table (table (cons start (cons 0 (list start)))))) 
        ((fold-natural (create start-table)
                       (lambda (path-table)
                         ((fold-set (create path-table) relax) sagaro)))
         (sub1 (cardinality (vertices graph))))))

    ;; ===== safe-shortest-paths-from =====================================
    ;; graph(alpha, number), alpha ->
    ;; graph                 start
    ;;                    table(alpha, pair(number, list(alpha))) | Boolean

    ;; The safe-shortest-paths-from procedure constructs a path table
    ;; giving the shortest path from start to every vertex in graph that is
    ;; reachable from start; return #f if there is a cycle of arcs with a
    ;; negative path sum containing a vertex that is reachable from start.

    ;; Precondition:
    ;;   start is a vertex of graph.

    (define (safe-shortest-paths-from graph start)
      (let ((path-table (shortest-paths-from graph start)))
        (if (exists-in-set? (sect relaxable? <> path-table) (arcs graph))
            #f
            path-table)))))

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
