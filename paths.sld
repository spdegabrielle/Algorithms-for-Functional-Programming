;;; Paths

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 23, 2001
;;; last revised January 9, 2017

(define-library (afp paths)
  (export path-finder path-connected? connected-components relaxable? relax
          nearest splice-paths)
  (import (afp primitives)
          (only (afp couplers) pipe)
          (only (afp adapters) >initial >next compare-by)
          (only (afp predicate-operations) ^if)
          (only (afp pairs) decons)
          (only (afp lists) prepend catenate)
          (only (afp sets)
                empty-set? fast-put-into-set take-from-set set subset?
                set-difference member?)
          (only (afp tables) put-into-table lookup)
          (only (afp ordering-relations) extreme-in-set)
          (only (afp graphs) vertices dearc restriction neighbors
                reachables)
          (afp depth-first-traversal))
  (begin

    ;; ===== path-finder ==================================================
    ;; graph(alpha, any) -> (alpha, alpha       -> list(alpha) | Boolean)
    ;; graph                 origin destination

    ;; The path-finder procedure constructs a predicate that attempts to
    ;; find a path from origin to destination in graph.  If the constructed
    ;; procedure is successful, it returns the path; otherwise, it returns
    ;; #f.

    ;; Preconditions:
    ;;   origin is a vertex of graph.
    ;;   destination is a vertex of graph.

    (define (path-finder graph)
      (let ((nabes (neighbors graph)))
        (lambda (origin destination)
          ((pipe (rec (visit vertex visited path-so-far)
                   (if (member? vertex visited)
                       (values #f visited)
                       (if (equal? vertex destination)
                           (values #t (reverse path-so-far))
                           ((rec (try-neighbors alternatives visited)
                              (if (empty-set? alternatives)
                                  (values #f visited)
                                  (receive (chosen others)
                                           (take-from-set alternatives)
                                    (receive (found result)
                                             (visit chosen
                                                    visited
                                                    (prepend chosen
                                                             path-so-far))
                                      (if found
                                          (values found result)
                                          (try-neighbors others result))))))
                            (nabes vertex)
                            (fast-put-into-set vertex visited)))))
                 (^if >initial >next >initial))
           origin (set) (list origin)))))

    ;; ===== path-connected? ==============================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The path-connected? predicate determines whether there is a path
    ;; from every vertex in graph to every other vertex in graph.

    ;; Precondition:
    ;;   graph is undirected.

    (define (path-connected? graph)
      (let ((verts (vertices graph))
            (reach (reachables graph)))
        (or (empty-set? verts)
            (receive (chosen ignored) (take-from-set verts)
              (subset? verts (reach chosen))))))

    ;; ===== connected-components =========================================
    ;; graph(alpha, beta) -> set(graph(alpha, beta))
    ;; graph

    ;; The connected-components procedure constructs a set containing the
    ;; connected components of graph.

    ;; Precondition:
    ;;   graph is undirected.

    (define (connected-components graph)
      (let ((reach (reachables graph)))
        ((rec (detach verts)
           (if (empty-set? verts)
               (set)
               (receive (chosen others) (take-from-set verts)
                 (let ((in-reach (reach chosen)))
                   (fast-put-into-set
                     (restriction in-reach graph)
                     (detach (set-difference verts in-reach)))))))
         (vertices graph))))

    ;; ===== relaxable? ===================================================
    ;; arc(alpha, number), table(alpha, pair(number, list(alpha))) ->
    ;; sago                path-table
    ;;                                                              Boolean

    ;; The relaxable? procedure determines whether sago can be relaxed,
    ;; relative to path-table.

    (define (relaxable? sago path-table)
      (receive (tail head label) (dearc sago)
        (let ((path-to-tail (lookup tail path-table))
              (path-to-head (lookup head path-table)))
          (and (pair? path-to-tail)
               (or (not path-to-head)
                   (< (+ (car path-to-tail) label) (car path-to-head)))))))

    ;; ===== relax ========================================================
    ;; arc(alpha, number), table(alpha, pair(number, list(alpha))) ->
    ;; sago                path-table
    ;;                              table(alpha, pair(number, list(alpha)))

    ;; The relax procedure constructs a path table similar to path-table,
    ;; but with the entry for the head of sago revised if sago is
    ;; relaxable.

    (define (relax sago path-table)
      (if (relaxable? sago path-table)
          (receive (tail head label) (dearc sago)
            (receive (path-sum path-vertices)
                     (decons (lookup tail path-table))
              (put-into-table head
                              (cons (+ path-sum label)
                                    (prepend head path-vertices))
                              path-table)))
            path-table))

    ;; ===== nearest ======================================================
    ;; set(alpha), table(alpha, pair(number, list(alpha))) -> alpha
    ;; aro         tab

    ;; The nearest procedure returns the element of aro that has the least
    ;; path sum (as reported by tab).

    ;; Preconditions:
    ;;   aro is not empty.
    ;;   Every member of aro is a key in tab.

    (define (nearest aro tab)
      ((extreme-in-set (compare-by (pipe (sect lookup <> tab) car) <=))
       aro))

    ;; ===== splice-paths =================================================
    ;; pair(number, list(alpha)), pair(number, list(alpha) ->
    ;; left                       right
    ;;                                            pair(number, list(alpha))

    ;; The splice-path procedure combines the path-table entries left and
    ;; right, returning a path-table entry that describes a path leading
    ;; from the origin of left to the destination of right.

    ;; Precondition:
    ;;   The destination of left is the origin of right.

    (define (splice-paths left right)
      (cons (+ (car left) (car right))
            (catenate (cdr right) (cdr (cdr left)))))))

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
