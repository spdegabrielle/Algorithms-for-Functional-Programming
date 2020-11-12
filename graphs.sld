;;; Graphs

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 22, 1999
;;; last revised January 9, 2017

(define-library (afp graphs)
  (export make-graph vertices arcs make-arc arc-tail arc-head arc-label
          dearc arc? arc=? same-endpoints? reverse-arc graph? graph=?
          empty-graph add-vertex arcless-graph add-labeled-arc add-arc
          delete-arc replace-arc delete-vertex restriction relation-graph
          adjacency-representation related-by complete-graph graph-converse
          graph-complement graph-product reflexive? irreflexive? symmetric?
          asymmetric? connected? transitive? intransitive?
          equivalence-relation? ordering-relation? arcs-leaving
          arcs-arriving neighbors label-structure label-lookup out-degree
          in-degree undirected? add-labeled-edge add-edge delete-edge
          make-edge edge-endpoints edge-label deedge edge? edge=? ends
          edges degree reachables)
  (import (afp primitives)
          (only (afp arithmetic) add1)
          (only (afp constant-procedures) values? create)
          (only (afp procedure-sections) equal-to)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) >next >exch compare-by)
          (only (afp predicate-operations)
                ^not ^et ^vel conditionally-combine)
          (only (afp lists) ^and remp run adapter)
          (only (afp tables) fast-put-into-table table lookup table-update)
          (only (afp sets)
                set? set-of fast-put-into-set empty-set? set-maker set
                deset take-from-set set=? set-of= fold-set map-set
                fast-map-set filter-set remp-set cardinality set-subsethood
                subset? for-all-in-set? exists-in-set? member?
                set-disjointness set-difference))
  (begin

    ;; ===== make-graph ===================================================
    ;; set(alpha), set(arc(alpha, beta)) -> graph(alpha, beta)
    ;; verts       sagaro

    ;; The make-graph procedure constructs a graph with the members of
    ;; verts as its vertices and the members of sagaro as its arcs.

    ;; Precondition:
    ;;   The endpoints of every member of sagaro are members of verts.

    (define make-graph cons)

    ;; ===== vertices =====================================================
    ;; graph(alpha, any) -> set(alpha)
    ;; graph

    ;; The vertices procedure returns a set containing the vertices of
    ;; graph.

    (define vertices car)

    ;; ===== arcs =========================================================
    ;; graph(alpha, beta) -> set(arc(alpha, beta))
    ;; graph

    ;; The arcs procedure returns a set containing the arcs of graph.

    (define arcs cdr)

    ;; ===== arc ==========================================================

    ;; An arc is a tuple comprising a tail vertex, a head vertex, and a
    ;; label.  The nature of these components is unspecified, but we assume
    ;; that the equal? predicate is an appropriate criterion of sameness for
    ;; vertices and for labels.

    (define-record-type arc
      (make-arc tail head label)
      arc?
      (tail arc-tail)
      (head arc-head)
      (label arc-label))

    (define dearc (dispatch arc-tail arc-head arc-label))

    (define arc=? (^and (compare-by arc-tail equal?)
                        (compare-by arc-head equal?)
                        (compare-by arc-label equal?)))

    ;; ===== same-endpoints? ==============================================
    ;; arc(alpha, beta), arc(alpha, beta) -> Boolean
    ;; left              right

    ;; The same-endpoints? predicate determines whether left and right have
    ;; the same head and the same tail.

    (define same-endpoints? (^et (compare-by arc-head equal?)
                                 (compare-by arc-tail equal?))) 

    ;; ===== reverse-arc ==================================================
    ;; arc(alpha, beta) -> arc(alpha, beta)
    ;; revertend

    ;; The reverse-arc procedure constructs an arc similar to revertend,
    ;; but with its endpoints reversed.

    (define reverse-arc (run dearc >exch make-arc))

    ;; ===== graph? =======================================================
    ;; any       -> Boolean
    ;; something

    ;; The graph predicate determines whether something is a graph.

    (define (graph? something)
      (and (pair? something)
           (let ((vertices-candidate (car something))
                 (arcs-candidate (cdr something)))
             (and (set? vertices-candidate)
                  ((set-of arc? same-endpoints?) arcs-candidate)
                  (subset? (map-set arc-tail arcs-candidate)
                           vertices-candidate)
                  (subset? (map-set arc-head arcs-candidate)
                           vertices-candidate)))))

    ;; ===== graph=? ======================================================
    ;; graph(any, any), graph(any, any) -> Boolean
    ;; left             right

    ;; The graph=? predicate determines whether left and right have the
    ;; same vertices and arcs (including matching labels on corresponding
    ;; arcs).

    (define graph=? (^et (compare-by vertices set=?)
                         (compare-by arcs (set-of= arc=?))))

    ;; ===== empty-graph ==================================================
    ;; -> graph(any, any)

    ;; The empty-graph procedure constructs a graph with no vertices and no
    ;; arcs.

    (define (empty-graph)
      (make-graph (set) ((set-maker same-endpoints?))))

    ;; ===== add-vertex ===================================================
    ;; alpha,     graph(alpha, beta) -> graph(alpha, beta)
    ;; new-vertex graph

    ;; The add-vertex procedure constructs a graph similar to graph,
    ;; except that it has new-vertex as a vertex.

    ;; Precondition:
    ;;   new-vertex is not a vertex of graph.

    (define (add-vertex new-vertex graph)
      (make-graph (fast-put-into-set new-vertex (vertices graph))
                  (arcs graph)))

    ;; ===== arcless-graph ================================================
    ;; set(alpha) -> graph(alpha, any)
    ;; verts

    ;; The arcless-graph procedure constructs a graph with the members of
    ;; verts as its vertices and no arcs.

    (define arcless-graph
      (sect make-graph <> ((set-maker same-endpoints?))))

    ;; ===== add-labeled-arc ==============================================
    ;; alpha, alpha, beta, graph(alpha, beta) -> graph(alpha, beta)
    ;; tail   head   label graph

    ;; The add-labeled-arc procedure constructs a graph similar to graph,
    ;; except that it includes an arc with tail, head, and label as its
    ;; components.

    ;; Preconditions:
    ;;   tail is a vertex of graph.
    ;;   head is a vertex of graph.
    ;;   graph contains no arc with tail as its tail and head as its head.

    (define (add-labeled-arc tail head label graph)
      (make-graph (vertices graph)
                  (fast-put-into-set (make-arc tail head label)
                                     (arcs graph))))

    ;; ===== add-arc ======================================================
    ;; alpha, alpha, graph(alpha, null) -> graph(alpha, null)
    ;; tail   head   graph

    ;; The add-arc procedure constructs a graph similar to graph,
    ;; except that it includes an unlabeled arc with tail as its tail and
    ;; head as its head.

    ;; Preconditions:
    ;;   tail is a vertex of graph.
    ;;   head is a vertex of graph.
    ;;   graph contains no arc with tail as its tail and head as its head.

    (define add-arc (sect add-labeled-arc <> <> null <>))

    ;; ===== delete-arc ===================================================
    ;; alpha, alpha, graph(alpha, beta) -> graph(alpha, beta)
    ;; tail   head   graph

    ;; The delete-arc procedure constructs a graph similar to graph, but
    ;; containing no arc with tail as its tail and head as its head.

    (define (delete-arc tail head graph)
      (make-graph (vertices graph)
                  (remp-set (^et (pipe arc-tail (equal-to tail))
                                 (pipe arc-head (equal-to head)))
                            (arcs graph))))

    ;; ===== replace-arc ==================================================
    ;; alpha, alpha, beta, graph(alpha, beta) -> graph(alpha, beta)
    ;; tail   head   label graph

    ;; The replace-arc procedure constructs a graph similar to graph, but
    ;; containing an arc with tail, head, and label as its components, and
    ;; no other arc with tail as its tail and head as its head.

    ;; Preconditions:
    ;;   tail is a vertex of graph.
    ;;   head is a vertex of graph.

    (define (replace-arc tail head label graph)
      (add-labeled-arc tail head label (delete-arc tail head graph)))

    ;; ===== delete-vertex ================================================
    ;; alpha, graph(alpha, beta) -> graph(alpha, beta)
    ;; delend graph

    ;; The delete-vertex procedure constructs a graph similar to graph, but
    ;; not containing delend as a vertex, nor any arcs with delend as an
    ;; endpoint.

    (define (delete-vertex delend graph)
      (let ((is-delend? (equal-to delend)))
        (make-graph (remp-set is-delend? (vertices graph))
                    (remp-set (^vel (pipe arc-tail is-delend?)
                                    (pipe arc-head is-delend?))
                              (arcs graph)))))

    ;; ===== restriction ==================================================
    ;; set(alpha), graph(alpha, beta) -> graph(alpha, beta)
    ;; keepers     graph

    ;; The restriction procedure constructs a graph similar to graph, but
    ;; not containing any vertex that is not a member of keepers, nor any
    ;; arc with an endpoint that is not a member of keepers.

    (define (restriction keepers graph)
      ((fold-set (create graph) delete-vertex)
       (set-difference (vertices graph) keepers)))

    ;; ===== relation-graph ===============================================
    ;; set(alpha), (alpha, alpha -> Boolean) -> graph(alpha, null)
    ;; domain      relation

    ;; The relation-graph procedure constructs a graph with the members of
    ;; domain as its vertices, having an unlabeled arc from one vertex to
    ;; another if, and only if, those vertices (in that order) satisfy
    ;; relation.

    ;; Precondition:
    ;;   relation can accept any members of domain.

    (define (relation-graph domain relation)
      (let ((maybe-add-arc (lambda (tail head aro)
                             (if (relation tail head)
                                 (fast-put-into-set
                                   (make-arc tail head null)
                                   aro)
                                 aro))))
        (make-graph domain
                    ((fold-set set
                               (lambda (tail arcs-so-far)
                                 ((fold-set (create arcs-so-far)
                                            (sect maybe-add-arc tail
                                                                <>
                                                                <>))
                                  domain)))
                     domain))))

    ;; ===== adjacency-representation =====================================
    ;; graph(alpha, beta) -> table(alpha, set(alpha))
    ;; graph

    ;; The adjacency-representation constructs the adjacency representation
    ;; of the arcs of graph -- in other words, a table, in which the keys
    ;; are graph's vertices and the entry with which a vertex is associated
    ;; is a set of the heads of the arcs in graph with that vertex as their
    ;; tail.

    (define (adjacency-representation graph)
      ((fold-set (create ((fold-set table
                                    (sect fast-put-into-table <> (set) <>))
                          (vertices graph)))
                 (lambda (arc tab)
                   (table-update (arc-tail arc)
                                 (sect fast-put-into-set (arc-head arc) <>)
                                 tab)))
       (arcs graph)))

    ;; ===== related-by ===================================================
    ;; graph(alpha, beta) -> (alpha, alpha -> Boolean)
    ;; graph                  tail   head

    ;; The related-by procedure constructs a predicate that determines
    ;; whether there is an arc from tail to head in graph.

    (define (related-by graph)
      (let ((adjacency (adjacency-representation graph)))
        (lambda (tail head)
          (member? head (lookup tail adjacency)))))

    ;; ===== complete-graph ===============================================
    ;; set(alpha) -> graph(alpha, null)
    ;; aro

    ;; The complete-graph procedure constructs the complete graph on aro,
    ;; that is, the graph with the members of aro as its vertices and an
    ;; unlabeled arc from each vertex to each vertex.

    (define complete-graph (sect relation-graph <> values?))

    ;; ===== graph-converse ===============================================
    ;; graph(alpha, beta) -> graph(alpha, beta)
    ;; graph

    ;; The graph-converse procedure constructs a graph similar to graph,
    ;; but with all of its arcs reversed.

    (define (graph-converse graph)
      (make-graph (vertices graph)
                  (fast-map-set reverse-arc (arcs graph))))

    ;; ===== graph-complement =============================================
    ;; graph(alpha, beta) -> graph(alpha, null)
    ;; graph

    ;; The graph-complement procedure computes the complement of graph,
    ;; that is, the graph with the vertices of graph as its vertices, but
    ;; having an unlabeled arc from a vertex, tail, to a vertex, head, if,
    ;; and only if, graph contains no arc from tail to head.

    (define (graph-complement graph)
      (relation-graph (vertices graph) (^not (related-by graph))))

    ;; ===== graph-product ================================================
    ;; graph(alpha, any), graph(alpha, any) -> graph(alpha, null)
    ;; left               right

    ;; The graph-product procedure computes the graph of the relational
    ;; product of left and right -- that is, the graph with the vertices of
    ;; left and right as its vertices, but having an unlabeled arc from a
    ;; vertex, tail, to a vertex, head, if, and only if, there is some
    ;; vertex, mid, such that left contains an arc from tail to mid and
    ;; right contains an arc from mid to head.

    ;; Preconditions:
    ;;   The vertices of left and the vertices of right are the same set.

    (define (graph-product left right)
      (let ((left? (related-by left))
            (right? (related-by right))
            (domain (vertices left)))
        (relation-graph domain
                        (lambda (tail head)
                          (exists-in-set? (^et (sect left? tail <>)
                                               (sect right? <> head))
                                          domain)))))

    ;; ===== reflexive? ===================================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The reflexive? predicate determines whether graph expresses a
    ;; reflexive relation -- in other words, whether it contains an arc
    ;; from each of its vertices to itself.

    (define (reflexive? graph)
      (for-all-in-set? (pipe (adapter 0 0) (related-by graph))
                       (vertices graph)))

    ;; ===== irreflexive? =================================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The irreflexive? predicate determines whether graph expresses an
    ;; irreflexive relation, that is, whether it contains no arc from any
    ;; of its vertices to itself.

    (define (irreflexive? graph)
      (not (exists-in-set? (pipe (dispatch arc-tail arc-head) equal?)
                           (arcs graph))))

    ;; ===== symmetric? ===================================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The symmetric? predicate determines whether graph expresses a
    ;; symmetric relation, that is, whether it contains the reverse arc of
    ;; each of its arcs (requiring equal labels).

    (define (symmetric? graph)
      (let ((connections (arcs graph)))
        ((set-of= arc=?) connections
                         (fast-map-set reverse-arc connections))))

    ;; ===== asymmetric? ==================================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The asymmetric? predicate determines whether graph expresses an
    ;; asymmetric relation, that is, whether no two of its arcs have the
    ;; same endpoints.

    (define (asymmetric? graph)
      (let ((connections (arcs graph)))
        ((set-disjointness same-endpoints?) connections
                                            (fast-map-set reverse-arc
                                                          connections))))

    ;; ===== connected? ===================================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The connected? predicate determines whether graph expresses a
    ;; connected relation.

    (define connected? (pipe graph-complement asymmetric?))

    ;; ===== transitive? ==================================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The transitive? predicate determines whether graph expresses a
    ;; transitive relation.

    (define (transitive? graph)
      ((set-subsethood same-endpoints?) (arcs (graph-product graph graph))
                                        (arcs graph)))

    ;; ===== intransitive? ================================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The intransitive? predicate determines whether graph expresses an
    ;; intransitive relation.

    (define (intransitive? graph)
      ((set-disjointness same-endpoints?)
         (arcs (graph-product graph graph))
         (arcs graph)))

    ;; ===== equivalence-relation? ========================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The equivalence-relation? predicate determines whether graph
    ;; expresses an equivalence relation.

    (define equivalence-relation?
      (^and reflexive? symmetric? transitive?))

    ;; ===== ordering-relation? ===========================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The ordering-relation? predicate determines whether graph expresses
    ;; an ordering relation on its vertices.

    (define ordering-relation? (^et connected? transitive?))

    ;; ===== arcs-leaving =================================================
    ;; alpha, graph(alpha, beta) -> set(arc(alpha, beta))
    ;; tail   graph

    ;; The arcs-leaving procedure constructs the set of arcs in graph
    ;; originating at tail.

    (define (arcs-leaving tail graph)
      (filter-set (pipe arc-tail (equal-to tail)) (arcs graph)))

    ;; ===== arcs-arriving ================================================
    ;; alpha, graph(alpha, beta) -> set(arc(alpha, beta))
    ;; head   graph

    ;; The arcs-arriving procedure constructs the set of arcs in graph
    ;; terminating at head.

    (define (arcs-arriving head graph)
      (filter-set (pipe arc-head (equal-to head)) (arcs graph)))

    ;; ===== neighbors ====================================================
    ;; graph(alpha, beta) -> (alpha -> set(alpha))
    ;; graph                  tail

    ;; The neighbors procedure constructs a procedure that, in turn,
    ;; constructs the set of neighbors of tail in graph -- that is, the set
    ;; of heads of arcs in graph that have tail as their tail.

    ;; Precondition:
    ;;   tail is a vertex of graph.

    (define (neighbors graph)
      (let ((adjacency (adjacency-representation graph)))
        (sect lookup <> adjacency)))

    ;; ===== label-structure ==============================================
    ;; graph(alpha, beta) -> table(alpha, table(alpha, beta))
    ;; graph

    ;; The label-structure procedure constructs a table in which the keys
    ;; are the vertices of graph, considered as tails of arcs, and the
    ;; entries are themselves tables in which the keys are again the
    ;; vertices of graph, this time considered as heads of arcs, and the
    ;; entries are the labels on the arcs in graph from the specified tail
    ;; to the specified head.

    (define (label-structure graph)
      ((fold-set (create
                   ((fold-set table
                              (sect fast-put-into-table <> (table) <>))
                    (vertices graph)))
                 (lambda (arc tab)
                   (table-update (arc-tail arc)
                                 (sect fast-put-into-table (arc-head arc)
                                                           (arc-label arc)
                                                           <>)
                                 tab)))
       (arcs graph)))

    ;; ===== label-lookup =================================================
    ;; graph(alpha, beta), beta ... -> (alpha, alpha -> beta | Boolean)
    ;; graph               optional     tail   head

    ;; The label-lookup procedure constructs a procedure that searches for
    ;; the label of an arc in graph that has tail as its tail and head as
    ;; its head.  If the search is successful, the procedure returns the
    ;; label; otherwise, it returns the initial element of optional, or #f
    ;; if optional is empty.

    ;; Precondition:
    ;;   tail is a vertex of graph.
    ;;   head is a vertex of graph.

    (define (label-lookup graph . optional)
      (let ((label-table (label-structure graph)))
        (lambda (tail head)
          (apply lookup head (lookup tail label-table) optional))))

    ;; ===== out-degree ===================================================
    ;; graph(alpha, any) -> (alpha -> natural-number)
    ;; graph                 tail

    ;; The out-degree procedure constructs a procedure that computes the
    ;; number of arcs in graph having tail as their tail.

    ;; Precondition:
    ;;   tail is a vertex of graph.

    (define (out-degree graph)
      (pipe (neighbors graph) cardinality))

    ;; ===== in-degree ====================================================
    ;; graph(alpha, any) -> (alpha -> natural-number)
    ;; graph                 head

    ;; The in-degree procedure constructs a procedure that computes the
    ;; number of arcs in graph having head as their head.

    ;; Precondition:
    ;;   head is a vertex of graph.

    (define (in-degree graph)
      (lambda (head)
        ((fold-set (create 0)
                   (conditionally-combine (pipe arc-head (equal-to head))
                                          (pipe >next add1)))
         (arcs graph))))

    ;; ===== undirected? ==================================================
    ;; graph(any, any) -> Boolean
    ;; graph

    ;; The undirected? predicate determines whether graph is undirected.

    (define undirected? (^et irreflexive? symmetric?))

    ;; ===== add-labeled-edge =============================================
    ;; alpha, alpha, beta, graph(alpha, beta) -> graph(alpha, beta)
    ;; end-0  end-1  label graph

    ;; The add-labeled-edge procedure constructs an undirected graph
    ;; similar to graph, but containing an edge between end-0 and end-1,
    ;; with label as its label.

    ;; Preconditions:
    ;;   end-0 is a vertex of graph.
    ;;   end-1 is a vertex of graph.
    ;;   graph is undirected.
    ;;   graph contains no edge with end-0 and end-1 as its endpoints.

    (define (add-labeled-edge end-0 end-1 label graph)
      (add-labeled-arc end-0
                       end-1
                       label
                       (add-labeled-arc end-1 end-0 label graph)))

    ;; ===== add-edge =====================================================
    ;; alpha, alpha, graph(alpha, null) -> graph(alpha, null)
    ;; end-0  end-1  graph

    ;; The add-edge procedure constructs an undirected graph similar to
    ;; graph, but containing an unlabeled edge between end-0 and end-1.

    ;; Preconditions:
    ;;   end-0 is a vertex of graph.
    ;;   end-1 is a vertex of graph.
    ;;   graph is undirected.
    ;;   graph contains no edge with end-0 and end-1 as its endpoints.

    (define add-edge (sect add-labeled-edge <> <> null <>))

    ;; ===== delete-edge ==================================================
    ;; alpha, alpha, graph(alpha, beta) -> graph(alpha, beta)
    ;; end-0  end-1  graph

    ;; The delete-edge procedure constructs an undirected graph similar to
    ;; graph, but without any edge between end-0 and end-1.

    ;; Preconditions:
    ;;   graph is undirected.

    (define (delete-edge end-0 end-1 graph)
      (delete-arc end-0 end-1 (delete-arc end-1 end-0 graph)))

    ;; ===== edge =========================================================

    ;; We implement edges as tuples, each comprising a set of exactly two
    ;; endpoints and a label.

    (define-record-type edge
      (make-edge endpoints label)
      proto-edge?
      (endpoints edge-endpoints)
      (label edge-label))

    (define deedge (dispatch edge-endpoints edge-label))

    (define edge?
      (^et proto-edge?
           (pipe edge-endpoints 
                 (^et set? (pipe cardinality (sect = <> 2))))))

    (define edge=?
      (^et (compare-by edge-endpoints set=?)
           (compare-by edge-label equal?)))

    ;; ===== ends =========================================================
    ;; edge(alpha, any) -> alpha, alpha
    ;; edge

    ;; The ends procedure returns the endpoints of edge, in either order.

    (define ends (pipe edge-endpoints deset))

    ;; ===== edges ========================================================
    ;; graph(alpha, beta) -> set(edge(alpha, beta))

    ;; The edges procedure returns a set containing the edges of graph.

    ;; Precondition:
    ;;   graph is undirected.

    (define (edges graph)
      ((rec (edger aro)
         (if (empty-set? aro)
             (set)
             (receive (chosen others) (take-from-set aro)
               (receive (end-0 end-1 label) (dearc chosen)
                 (fast-put-into-set
                   (make-edge (set end-0 end-1) label)
                   (edger (remp (^et (pipe arc-tail (equal-to end-1))
                                     (pipe arc-head (equal-to end-0)))
                                others)))))))
       (arcs graph)))

    ;; ===== degree =======================================================
    ;; graph(alpha, any) -> (alpha -> natural-number)
    ;; graph                 vert

    ;; The degree procedure constructs a procedure that computes the degree
    ;; of vert in graph.

    ;; Preconditions:
    ;;   graph is undirected.
    ;;   vert is a vertex of graph.

    (define degree out-degree)

    ;; ===== reachables ===================================================
    ;; graph(alpha, any) -> (alpha -> set(alpha))
    ;; graph                  vert

    ;; The reachables procedure constructs a procedure that, in turn,
    ;; constructs the set of vertices that can be reached from vert by
    ;; following arcs in graph.

    (define (reachables graph)
      (let ((nabes (neighbors graph)))
        (let ((visit (rec (visit current visited)
                       (if (member? current visited)
                           visited
                           ((fold-set (create
                                        (fast-put-into-set current
                                                           visited))
                                      visit)
                            (nabes current))))))
          (sect visit <> (set)))))))

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
