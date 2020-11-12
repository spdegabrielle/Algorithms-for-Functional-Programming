;;; Flow networks

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created October 31, 2005
;;; last revised January 9, 2017

(define-library (afp flow-networks)
  (export capacity-graph? flow-in-range? net-flow flow-conserved?
          flow-graph? flow-value zero-flow residual-network augment-flow
          maximum-network-flow)
  (import (afp primitives)
          (only (afp constant-procedures) create)
          (only (afp procedure-sections) equal-to)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) ~initial ~each)
          (only (afp predicate-operations) ^et)
          (only (afp pairs) decons)
          (only (afp lists) fold-list ^and ^or run adjacent-pairs)
          (only (afp sets)
                set=? set-of= fold-set cardinality for-all-in-set? member?)
          (only (afp ordering-relations) extreme-in-list)
          (only (afp graphs)
                vertices arcs arc-tail arc-head arc-label dearc
                same-endpoints? arcless-graph add-labeled-arc replace-arc
                asymmetric? arcs-leaving arcs-arriving label-lookup)
          (only (afp paths) path-finder))
  (begin
    
    ;; ===== capacity-graph? ==============================================
    ;; graph(alpha, any) -> Boolean
    ;; graph

    ;; The capacity-graph? procedure determines whether graph is a capacity
    ;; graph.  Such a graph is asymmetric, and each of its arcs is labeled
    ;; with a positive number.  It also contains at least two vertices.

    (define capacity-graph?
      (let ((positive-label? (pipe arc-label (^et number? positive?))))
        (^and asymmetric?
              (run vertices cardinality (sect <= 2 <>))
              (pipe arcs (sect for-all-in-set? positive-label? <>)))))

    ;; ===== flow-in-range? ===============================================
    ;; graph(alpha, any), graph(alpha, number) -> Boolean
    ;; graph              capacity-graph

    ;; The flow-in-range? predicate determines whether each of the arcs in
    ;; graph has a label that is a number in the appropriate range, as
    ;; specified by capacity-graph.

    ;; Preconditions:
    ;;   The vertices of graph are the vertices of capacity-graph.
    ;;   Each arc in graph has the same tail and the same head as some arc
    ;;     in capacity-graph.
    ;;   capacity-graph is a capacity graph.

    (define (flow-in-range? graph capacity-graph)
      (let ((capacity (label-lookup capacity-graph 0)))
        (for-all-in-set? (lambda (arc)
                           (receive (tail head label) (dearc arc)
                             (and (number? label)
                                  (<= 0 label (capacity tail head)))))
                         (arcs graph))))

    ;; ===== net-flow =====================================================
    ;; alpha, graph(alpha, number) -> number
    ;; vert   graph

    ;; The net-flow procedure computes the net flow through vert in graph,
    ;; that is, the result of subtracting the sum of the labels of the arcs
    ;; in graph with vert as their head from the sum of the labels of the
    ;; arcs in graph with vert as their tail.

    ;; Precondition:
    ;;   vert is a vertex of graph.

    (define net-flow
      (run (dispatch arcs-leaving arcs-arriving)
           (~each (fold-set (create 0) (pipe (~initial arc-label) +)))
           -))

    ;; ===== flow-conserved? ==============================================
    ;; graph(alpha, number), alpha, alpha       -> Boolean
    ;; graph                 origin destination

    ;; The flow-conserved? predicate determines whether the net flow in
    ;; graph is zero at every vertex except origin and destination.

    (define (flow-conserved? graph origin destination)
      (for-all-in-set? (^or (equal-to origin)
                            (equal-to destination)
                            (pipe (sect net-flow <> graph) zero?))
                       (vertices graph)))

    ;; ===== flow-graph? ==================================================
    ;; graph(alpha, any), graph(alpha, number), alpha, alpha       ->
    ;; graph              capacity-graph        origin destination 
    ;;                                                              Boolean

    ;; The flow-graph? predicate determines whether graph is a flow graph,
    ;; relative to capacity-graph, with zero net flow at every vertex
    ;; except origin and destination.

    ;; Preconditions:
    ;;   capacity-graph is a capacity graph.

    (define (flow-graph? candidate capacity-graph origin destination)
      (let ((verts (vertices candidate)))
        (and (set=? verts (vertices capacity-graph))
             ((set-of= same-endpoints?) (arcs candidate)
                                        (arcs capacity-graph))
             (flow-in-range? candidate capacity-graph)
             (member? origin verts)
             (member? destination verts)
             (not (equal? origin destination))
             (flow-conserved? candidate origin destination))))

    ;; ===== flow-value ===================================================
    ;; alpha, graph(alpha, number) -> number
    ;; origin graph

    ;; The flow-value procedure computes the value of the flow in graph,
    ;; taking origin as its origin.

    ;; Preconditions:
    ;;   graph is a flow graph with origin as its origin.

    (define flow-value net-flow)

    ;; ===== zero-flow ====================================================
    ;; graph(alpha, number) -> graph(alpha, number)
    ;; graph

    ;; The zero-flow procedure constructs a flow graph of value 0 relative
    ;; to graph.

    ;; Precondition:
    ;;   graph is a capacity graph.

    (define (zero-flow graph)
      ((fold-set (create (arcless-graph (vertices graph)))
                 (lambda (arc flow-graph)
                   (add-labeled-arc (arc-tail arc) (arc-head arc) 0
                                    flow-graph)))
       (arcs graph)))

    ;; ===== residual-network =============================================
    ;; graph(alpha, number), graph(alpha, number) -> graph(alpha, number)
    ;; capacity-graph        flow-graph

    ;; The residual-network procedure computes the graph of the residual
    ;; network of flow-graph relative to capacity-graph.

    ;; Preconditions:
    ;;   capacity-graph is a capacity graph.
    ;;   flow-graph is a flow graph relative to capacity-graph.

    (define (residual-network capacity-graph flow-graph)
      (let ((capacity (label-lookup capacity-graph 0)))
        ((fold-set (create (arcless-graph (vertices flow-graph)))
                   (lambda (arc residual)
                     (receive (tail head label) (dearc arc)
                       (let ((cap (capacity tail head)))
                         (if (zero? label)
                             (add-labeled-arc tail head cap residual)
                             (add-labeled-arc head tail label
                               (if (= label cap)
                                   residual
                                   (add-labeled-arc tail head (- cap label)
                                                    residual))))))))
         (arcs flow-graph))))

    ;; ===== augment-flow =================================================
    ;; graph(alpha, number), graph(alpha, number), list(alpha) ->
    ;; flow-graph            residual              path
    ;;                                                 graph(alpha, number)

    ;; The augment-flow procedure constructs a flow graph similar to
    ;; flow-graph, except that the value of the flow from its origin to its
    ;; destination is greater, as a result of adjustments in the arc labels
    ;; in flow-graph along path in residual.

    ;; Preconditions:
    ;;   residual is the residual network for flow-graph relative to some
    ;;     capacity graph.
    ;;   path is an augmenting path in residual.

    (define (augment-flow flow-graph residual path)
      (let ((adjs (adjacent-pairs path))
            (flow (label-lookup flow-graph)))
        (let ((delta ((extreme-in-list <=)
                      (map (pipe decons (label-lookup residual 0)) adjs))))
          ((fold-list
             (create flow-graph)
             (lambda (adj new-flow-graph)
               (receive (tail head) (decons adj)
                 (let ((old-adj-flow (flow tail head)))
                   (if (number? old-adj-flow)
                       (replace-arc tail head (+ old-adj-flow delta)
                                    new-flow-graph)
                       (replace-arc head tail (- (flow head tail) delta)
                                    new-flow-graph))))))
           adjs))))

    ;; ===== maximum-network-flow =========================================
    ;; graph(alpha, number), alpha, alpha       -> graph(alpha, number)
    ;; capacity-graph        origin destination

    ;; The maximum-network-flow procedure constructs a flow graph of
    ;; maximum value in the network described by capacity-graph, with
    ;; origin as its origin and destination as its destination.

    ;; Preconditions:
    ;;   capacity-graph is a capacity graph.
    ;;   origin and destination are distinct vertices in capacity-graph.

    (define (maximum-network-flow capacity-graph origin destination)
      ((rec (augment flow-graph)
         (let ((residual (residual-network capacity-graph flow-graph)))
           (let ((augmenting-path
                  ((path-finder residual) origin destination)))
             (if (not augmenting-path)
                 flow-graph
                 (augment (augment-flow
                            flow-graph residual augmenting-path))))))
       (zero-flow capacity-graph)))))

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
