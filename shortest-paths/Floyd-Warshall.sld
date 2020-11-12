;;; The Floyd-Warshall algorithm

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created October 10, 2005
;;; last revised January 9, 2017

(define-library (afp shortest-paths Floyd-Warshall)
  (export trivial-table test-connection all-shortest-paths)
  (import (afp primitives)
          (only (afp constant-procedures) create)
          (only (afp sets) fold-set)
          (only (afp tables) put-into-table lookup table table-update)
          (only (afp graphs) vertices dearc arcs)
          (only (afp paths) splice-paths))
  (begin

    ;; ===== trivial-table ================================================
    ;; graph(alpha, number) ->
    ;; graph
    ;;                table(alpha, table(alpha, pair(number, list(alpha))))

    ;; The trivial-table procedure constructs a two-level path table
    ;; containing the trivial (one-vertex and two-vertex) paths for graph,
    ;; along with their path sums.

    (define (trivial-table graph)
      ((fold-set (create ((fold-set table
                                    (lambda (vertex tab)
                                      (put-into-table
                                        vertex
                                        (table (cons vertex
                                                     (cons 0
                                                           (list vertex))))
                                        tab)))
                          (vertices graph)))
                 (lambda (arc tab)
                   (receive (tail head label) (dearc arc)
                     (if (equal? tail head)
                         tab
                         (table-update tail
                                       (sect put-into-table
                                             head
                                             (cons label (list head tail))
                                             <>)
                                       tab)))))
       (arcs graph)))

    ;; ===== test-connection ==============================================
    ;; (pair(number, list(alpha)) | Boolean),
    ;; fore
    ;;     (pair(number, list(alpha)) | Boolean),
    ;;     aft
    ;;         (pair(number, list(alpha)) | Boolean),
    ;;         direct
    ;;             alpha, table(alpha, pair(number, list(alpha))) ->
    ;;             goal   tab
    ;;                              table(alpha, pair(number, list(alpha)))

    ;; The test-connection procedure returns a path table similar to tab,
    ;; but updated using information about three potential paths within a
    ;; graph: one (fore) connecting an "origin" vertex to some intermediate
    ;; vertex, a second (aft) connecting the intermediate vertex to a
    ;; "goal" vertex (goal), and a third (direct) connecting the origin to
    ;; the goal directly.  In place of any or all of these paths, the value
    ;; #f, signifying the absence of a path, may be received.

    ;; Preconditions:
    ;;   If neither fore nor direct is #f, then the origin of fore is the
    ;;     origin of direct.
    ;;   If neither fore nor aft is #f, then the destination of fore is
    ;;     the origin of aft. 
    ;;   If aft is not #f, then the destination of aft is goal.
    ;;   If direct is not #f, then the destination of direct is goal.
    ;;   goal is a key in tab.

    (define (test-connection fore aft direct goal tab)
      (if (and (pair? fore) (pair? aft))
          (put-into-table goal
                          (if (and (pair? direct)
                                   (< (car direct)
                                      (+ (car fore) (car aft))))
                              direct
                              (splice-paths fore aft))
                          tab)
          (if (pair? direct)
              (put-into-table goal direct tab)
              tab)))

    ;; ===== all-shortest-paths ===========================================
    ;; graph(alpha, number) ->
    ;; graph
    ;;                table(alpha, table(alpha, pair(number, list(alpha))))

    ;; The all-shortest-paths procedure constructs a two-level path table
    ;; giving the shortest path from any vertex to any vertex in graph.

    ;; Precondition:
    ;;   Every arc in graph has a positive label.

    (define (all-shortest-paths graph)
      (let ((verts (vertices graph)))
        ((fold-set
           (create (trivial-table graph))
           (lambda (intermediate tab)
             (let ((mid-paths (lookup intermediate tab)))
               ((fold-set
                  table
                  (lambda (origin outer)
                    (let ((origin-paths (lookup origin tab)))
                      (let ((forepath (lookup intermediate origin-paths)))
                        (put-into-table
                          origin
                          ((fold-set
                             table
                             (lambda (goal new-tab)
                               (test-connection forepath
                                                (lookup goal mid-paths)
                                                (lookup goal origin-paths)
                                                goal
                                                new-tab)))
                           verts)
                          outer)))))
                verts))))
         verts)))))

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
