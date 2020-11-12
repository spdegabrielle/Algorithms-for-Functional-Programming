;;; The Dijkstra shortest-path algorithm

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 28, 2005
;;; last revised January 9, 2017

(define-library (afp shortest-paths Dijkstra)
  (export shortest-paths-from all-shortest-paths)
  (import (afp primitives)
          (only (afp constant-procedures) create)
          (only (afp couplers) pipe)
          (only (afp adapters) >initial >next)
          (only (afp recursion-managers) iterate)
          (only (afp sets)
                empty-set? set fold-set intersection remove-from-set)
          (only (afp tables) put-into-table table table-keys)
          (only (afp graphs) vertices arcs-leaving)
          (only (afp paths) relax nearest))
  (begin

    ;; ===== shortest-paths-from ==========================================
    ;; graph(alpha, number), alpha ->
    ;; graph                 start
    ;;                              table(alpha, pair(number, list(alpha)))

    ;; The shortest-path-from procedure constructs a path table giving the
    ;; shortest path from start to every vertex in graph.

    ;; Preconditions:
    ;;   Every arc in graph has a positive label.
    ;;   start is a vertex of graph.

    (define (shortest-paths-from graph start)
      ((pipe (iterate
               (pipe >initial empty-set?)
               (lambda (candidates tab remaining)
                 (let ((chosen (nearest candidates tab)))
                   (let ((new-tab ((fold-set (create tab) relax)
                                   (arcs-leaving chosen graph)))
                         (new-remaining (remove-from-set chosen
                                                         remaining)))
                     (values (intersection new-remaining
                                           (table-keys new-tab))
                             new-tab
                             new-remaining)))))
             >next)
       (set start)
       (table (cons start (cons 0 (list start))))
       (vertices graph)))

    ;; ===== all-shortest-paths ===========================================
    ;; graph(alpha, number) ->
    ;; graph
    ;;                table(alpha, table(alpha, pair(number, list(alpha))))

    ;; The all-shortest-paths procedure constructs a two-level path table
    ;; giving the shortest path from any vertex to any vertex in graph.

    ;; Precondition:
    ;;   Every arc in graph has a positive label.

    (define (all-shortest-paths graph)
      ((fold-set table
                 (lambda (vertex tab)
                   (put-into-table vertex
                                   (shortest-paths-from graph vertex)
                                   tab)))
       (vertices graph)))))

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
