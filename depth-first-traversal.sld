;;; Depth-first traversal

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 28, 1999
;;; last revised January 6, 2017

(define-library (afp depth-first-traversal)
  (export depth-first-visit depth-first-traversal topological-sort)
  (import (afp primitives)
          (only (afp constant-procedures) create)
          (only (afp adapters) >all-but-initial)
          (only (afp lists) prepend)
          (only (afp sets) set fast-put-into-set fold-set member?)
          (only (afp graphs) vertices neighbors))
  (begin

    ;; ===== depth-first-visit ============================================
    ;; (alpha -> set(alpha)), (alpha, beta ... -> beta ...),
    ;; nabes                  arrive
    ;;     (alpha, beta ... -> beta ...) ->
    ;;     depart
    ;;                            (alpha, set(alpha), beta ... -> beta ...)
    ;;                             vertex visited     so-far

    ;; The depth-first-visit procedure constructs a procedure that visits
    ;; vertex, receiving as additional arguments the set of previously
    ;; visited vertices (visited) and the results of the computations
    ;; undertaken during those previous visits (so-far).  If vertex is a
    ;; member of visited, the constructed procedure returns visited and the
    ;; elements of so-far; otherwise, the constructed procedure invokes
    ;; arrive (giving it vertex and the elements of so-far as arguments), a
    ;; recursive invocation of the constructed procedure for each of the
    ;; neighbors of the vertex, as determined by nabes (giving the
    ;; constructed procedure the neighbor, the result of adding vertex to
    ;; visited, and the results of the most recent visit as arguments), and
    ;; an invocation of depart (giving it vertex and the results of the
    ;; most recent visit as arguments).  The constructed procedure returns
    ;; the results of the final visit.

    ;; Preconditions:
    ;;   nabes can receive vertex.
    ;;   nabes can receive any element of any result of nabes.
    ;;   arrive can receive vertex and the elements of so-far.
    ;;   arrive can receive any element of any result of nabes and the
    ;;     elements of so-far.
    ;;   arrive can receive any element of any result of nabes and the
    ;;     results of any invocation of depart.
    ;;   depart can receive vertex and the elements of so-far.
    ;;   depart can receive any element of any result of nabes and the
    ;;     elements of so-far.
    ;;   depart can receive any element of any result of nabes and the
    ;;     results of any invocation of depart.

    (define (depth-first-visit nabes arrive depart)
      (rec (visit vertex visited . so-far)
        (if (member? vertex visited)
            (apply values visited so-far)
            (receive after-arrival (apply arrive vertex so-far)
              (receive (new-visited . new-so-far)
                       ((fold-set (apply create (fast-put-into-set vertex 
                                                                   visited)
                                                after-arrival)
                                  visit)
                        (nabes vertex))
                (receive after-departure (apply depart vertex new-so-far)
                  (apply values new-visited after-departure)))))))

    ;; ===== depth-first-traversal ========================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...),
    ;; base            arrive
    ;;    (beta, alpha ... -> alpha ...) -> (graph(beta, any) -> alpha ...)
    ;;    depart                             graph

    ;; The depth-first-traversal procedure constructs a procedure that
    ;; traverses graph in depth-first order, starting from an arbitrarily
    ;; chosen vertex, returning the results obtained by invoking a given
    ;; base procedure to obtain some initial values, and then transforming
    ;; these values by applying an "arrival procedure" to each vertex and
    ;; the incoming values as the vertex is reached, and a "departure
    ;; procedure" to the vertex and the derived values after all of the
    ;; vertex's neighbors have been similarly visited.

    ;; Preconditions:
    ;;   arrive can receive any vertex of graph and the results of any
    ;;     invocation of base.
    ;;   arrive can receive any vertex of graph and the results of any
    ;;     invocation of depart.
    ;;   depart can receive any vertex of graph and the results of any
    ;;     invocation of base.
    ;;   depart can receive any vertex of graph and the results of any
    ;;     invocation of depart.

    (define (depth-first-traversal base arrive depart)
      (let ((new-base (receive starters (base)
                        (apply create (set) starters))))
        (lambda (graph)
          (receive (visited . finals)
                   ((fold-set new-base (depth-first-visit (neighbors graph)
                                                          arrive
                                                          depart))
                    (vertices graph))
            (delist finals)))))

    ;; ===== topological-sort =============================================
    ;; graph(alpha, any) -> list(alpha)
    ;; graph

    ;; The topological-sort procedure constructs a list of the vertices of
    ;; graph such that there is no arc in graph in which the head precedes
    ;; (in the list) the tail.

    ;; Preconditions:
    ;;   graph is acyclic.

    (define topological-sort
      (depth-first-traversal list >all-but-initial prepend))))

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
