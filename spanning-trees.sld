;;; Spanning trees

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 6, 2001
;;; last revised January 9, 2017

(define-library (afp spanning-trees)
  (export spanning-tree minimum-spanning-tree crossings light-edge
          alternative-minimum-spanning-tree)
  (import (afp primitives)
          (only (afp arithmetic) sub1)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) >initial >next ~initial compare-by)
          (only (afp recursion-managers) iterate)
          (only (afp lists) deprepend run)
          (only (afp sets)
                deset take-from-set empty-set? remp-set cardinality
                remove-from-set member? set->bag)
          (only (afp ordering-relations) extreme-in-set)
          (only (afp sorting mergesort) sort)
          (only (afp graphs)
                vertices empty-graph add-vertex arcless-graph edge-label
                add-labeled-edge deedge ends edges)
          (only (afp paths) path-finder))
  (begin

    ;; ===== spanning-tree ================================================
    ;; graph(alpha, beta) -> graph(alpha, beta)
    ;; graph

    ;; The spanning-tree procedure constructs a spanning tree for graph.

    ;; Preconditions:
    ;;   graph is undirected.
    ;;   graph is path-connected.

    (define spanning-tree
      (run (dispatch
             (run vertices cardinality sub1 (sect max <> 0))
             (pipe vertices arcless-graph)
             edges)
           (iterate
             (pipe >initial zero?)
             (lambda (countdown spanner rest-of-edges)
               (receive (chosen others) (take-from-set rest-of-edges)
                 (receive (termini label) (deedge chosen)
                   (receive (end-0 end-1) (deset termini)
                     (if (list? ((path-finder spanner) end-0 end-1))
                         (values countdown spanner others)
                         (values (sub1 countdown)
                                 (add-labeled-edge end-0 end-1 label
                                                   spanner)
                                 others)))))))
           >next))

    ;; ===== minimum-spanning-tree ========================================
    ;; graph(alpha, number) -> graph(alpha, number)
    ;; graph

    ;; The minimum-spanning-tree procedure constructs a minimum-weight
    ;; spanning tree for graph.

    ;; Preconditions:
    ;;   graph is undirected.
    ;;   graph is path-connected.

    (define minimum-spanning-tree
      (run (dispatch
             (run vertices cardinality sub1 (sect max <> 0))
             (pipe vertices arcless-graph)
             (run edges
                  set->bag
                  (sect sort (compare-by edge-label <=) <>)))
           (iterate
             (pipe >initial zero?)
             (lambda (countdown spanner rest-of-edges)
               (receive (chosen others) (deprepend rest-of-edges)
                 (receive (termini label) (deedge chosen)
                   (receive (end-0 end-1) (deset termini)
                     (if (list? ((path-finder spanner) end-0 end-1))
                         (values countdown spanner others)
                         (values (sub1 countdown)
                                 (add-labeled-edge end-0 end-1 label
                                                   spanner)
                                 others)))))))
           >next))

    ;; ===== crossings ====================================================
    ;; graph(alpha, beta) -> (set(alpha) -> set(edge(alpha, beta)))
    ;; graph                  cut

    ;; The crossings procedure constructs a procedure that, in turn,
    ;; constructs the set of edges of graph that have exactly one endpoint
    ;; that is a member of cut.

    ;; Precondition:
    ;;   graph is undirected.

    (define (crossings graph)
      (let ((edge-set (edges graph)))
        (lambda (cut)
          (remp-set (pipe ends
                          (compare-by (sect member? <> cut) boolean=?))
                    edge-set))))

    ;; ===== light-edge ===================================================
    ;; graph(alpha, number) -> (set(alpha) -> edge(alpha, number))
    ;; graph                    cut

    ;; The light-edge procedure constructs a procedure that finds the edge
    ;; in graph with the least label among those that have exactly one
    ;; endpoint that is a member of cut.

    ;; Precondition:
    ;;   graph is undirected.

    (define (light-edge graph)
      (pipe (crossings graph) (extreme-in-set (compare-by edge-label <=))))

    ;; ===== alternative-minimum-spanning-tree ============================
    ;; graph(alpha, number) -> graph(alpha, number)
    ;; graph

    ;; The alternative-minimum-spanning-tree procedure constructs a
    ;; minimum-weight spanning tree for graph.

    ;; Preconditions:
    ;;   graph is undirected.
    ;;   graph is path-connected.

    (define (alternative-minimum-spanning-tree graph)
      (let ((verts (vertices graph))
            (light-edge-finder (light-edge graph)))
        (if (empty-set? verts)
            (empty-graph)
            ((run take-from-set
                  (~initial (sect add-vertex <> (empty-graph)))
                  (iterate
                    (pipe >next empty-set?)
                    (lambda (spanner outs)
                      (let ((connector (light-edge-finder outs)))
                        (receive (termini label) (deedge connector)
                          (receive (end-0 end-1) (deset termini)
                            (let ((ligand (if (member? end-0 outs)
                                              end-0
                                              end-1)))
                              (values (add-labeled-edge
                                        end-0
                                        end-1
                                        label
                                        (add-vertex ligand spanner))
                                      (remove-from-set ligand outs))))))))
                  >initial)
             verts))))))

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
