;;; Heaps

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 12, 1999
;;; last revised January 9, 2017

(define-library (afp heaps)
  (export empty-heap empty-heap? satisfies-heap-invariants? merge-heaps
          heap-of heap-adjoiner heap-list-merger heap-extractor heap-of=
          fold-heap unfold-heap process-heap)
  (import (afp primitives)
          (only (afp constant-procedures) constant)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) ~initial)
          (only (afp recursion-managers) build)
          (only (afp predicate-operations) ^not ^vel)
          (only (afp lists) first rest empty-list? ^and for-all?)
          (only (afp bushes)
                bush-root bush-children bush empty-bush? bush-of
                for-all-in-bush?)
          (afp bags))
  (begin

    ;; ===== empty-heap ===================================================
    ;; -> bush(any)

    ;; The empty-heap procedure constructs a heap with no elements.

    (define (empty-heap)
      (bush))

    ;; ===== empty-heap? ==================================================
    ;; bush(any) -> Boolean
    ;; amaso

    ;; The empty-heap? procedure determines whether amaso is empty.

    (define empty-heap? empty-bush?)

    ;; ===== satisfies-heap-invariants? ===================================
    ;; (alpha, alpha -> Boolean) -> (bush(alpha) -> Boolean)
    ;; may-precede?                  arbusto

    ;; The satisfies-heap-invariants? procedure constructs a predicate that
    ;; determines whether arbusto satisfies the heap invariants with
    ;; respect to may-precede?: specifically, whether all of the children
    ;; of arbusto are non-empty bushes that likewise satisfy the heap
    ;; invariant, and whether its root bears may-precede? to all of the
    ;; values in arbusto.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any elements of arbusto.

    (define (satisfies-heap-invariants? may-precede?)
      (rec (ok? arbusto)
        (let ((root (bush-root arbusto)))
          (for-all? (^and (^not empty-bush?)
                          (pipe bush-root (sect may-precede? root <>))
                          ok?)
                    (bush-children arbusto)))))

    ;; ===== merge-heaps ==================================================
    ;; (alpha, alpha -> Boolean) ->
    ;; may-precede?
    ;;                            (bush(alpha), bush(alpha) -> bush(alpha))
    ;;                             left         right

    ;; The merge-heaps procedure constructs a procedure that, in turn,
    ;; constructs a bush that satisfies the heap invariants with respect to
    ;; may-precede? and contains every element of left and every element of
    ;; right.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any element of left and any element of
    ;;     right.
    ;;   left satisfies the heap invariants with respect to may-precede?.
    ;;   right satisfies the heap invariants with respect to may-precede?.

    (define (merge-heaps may-precede?)
      (lambda (left right)
        (if (empty-bush? left)
            right
            (if (empty-bush? right)
                left
                (let ((lroot (bush-root left))
                      (lchildren (bush-children left))
                      (rroot (bush-root right))
                      (rchildren (bush-children right)))
                  (if (may-precede? lroot rroot)
                      (apply bush lroot right lchildren)
                      (apply bush rroot left rchildren)))))))

    ;; ===== heap-of ======================================================
    ;; (alpha, alpha -> Boolean), (any -> Boolean) ->
    ;; may-precede?                in-domain
    ;;                                               (any       -> Boolean)
    ;;                                                something

    ;; The heap-of procedure constructs a predicate that determines whether
    ;; something is a bush in which every element satisfies in-domain? and
    ;; the entire bush satisfies the heap invariants with respect to
    ;; may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values that satisfy in-domain?.

    (define (heap-of may-precede? in-domain?)
      (^and (bush-of in-domain?)
            (sect for-all-in-bush? in-domain? <>)
            (^vel empty-bush? (satisfies-heap-invariants? may-precede?))))

    ;; ===== heap-adjoiner ================================================
    ;; (alpha, alpha -> Boolean) -> (alpha, bush(alpha) -> bush(alpha))
    ;; may-precede?                  new    amaso

    ;; The heap-adjoiner procedure constructs a procedure that, in turn,
    ;; constructs a bush similar to amaso, containing new and all of the
    ;; elements of amaso and satisfying the heap invariants with respect to
    ;; may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive new and any element of amaso.
    ;;   amaso satisfies the heap invariants with respect to may-precede?.

    (define (heap-adjoiner may-precede?)
      (pipe (~initial bush) (merge-heaps may-precede?)))

    ;; ===== heap-list-merger =============================================
    ;; (alpha, alpha -> Boolean) -> (list(bush(alpha)) -> bush(alpha))
    ;; may-precede?                  heap-list

    ;; The heap-list-merger procedure constructs a procedure that, in turn,
    ;; constructs a bush that contains all of the elements of elements of
    ;; heap-list and satisfies the heap invariants with respect to
    ;; may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any elements of elements of heap-list.
    ;;   Every element of heap-list satisfies the heap invariants with
    ;;     respect to may-precede?.

    (define (heap-list-merger may-precede?)
      (let ((merge (merge-heaps may-precede?)))
        (rec (pairwise ls)
          (if (empty-list? ls)
              (empty-heap)
              (if (empty-list? (rest ls))
                  (first ls)
                  (merge (merge (first ls) (first (rest ls)))
                         (pairwise (rest (rest ls)))))))))

    ;; ===== heap-extractor ===============================================
    ;; (alpha, alpha -> Boolean) -> (bush(alpha) -> alpha, bush(alpha))
    ;; may-precede?                  amaso

    ;; The heap-extractor procedure constructs a procedure that returns an
    ;; element of amaso that bears may-precede? to every element of amaso,
    ;; along with a bush that contains all of the other elements of amaso
    ;; and satisfies the heap invariants with respect to may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? accepts any elements of amaso.
    ;;   amaso is not empty.
    ;;   amaso satisfies the heap invariants with respect to may-precede?.

    (define (heap-extractor may-precede?)
      (dispatch bush-root
                (pipe bush-children (heap-list-merger may-precede?))))

    ;; ===== heap-of= ====================================================
    ;; (alpha, alpha -> Boolean) -> (bush(alpha), bush(alpha) -> Boolean)
    ;; may-precede?                  left         right

    ;; The heap-of= procedure constructs a predicate that determines
    ;; whether left and right are equivalent as heaps, that is, whether
    ;; they are equal in size and contain elements that correspond, in the
    ;; sense that each bears may-precede? to the other.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any element of left and any element of
    ;;     right, in either order.
    ;;   left satisfies the heap invariants with respect to may-precede?.
    ;;   right satisfies the heap invariants with respect to may-precede?.

    (define (heap-of= may-precede?)
      (let ((extract-from-heap (heap-extractor may-precede?)))
        (rec (equivalent? left right)
          (or (and (empty-heap? left)
                   (empty-heap? right))
              (and (not (empty-heap? left))
                   (not (empty-heap? right))
                   (receive (lchosen lothers) (extract-from-heap left)
                     (receive (rchosen rothers) (extract-from-heap right)
                       (and (may-precede? lchosen rchosen)
                            (may-precede? rchosen lchosen)
                            (equivalent? lothers rothers)))))))))

    ;; ===== fold-heap ====================================================
    ;; (alpha, alpha -> Boolean), (-> beta ...),
    ;; may-precede?               base
    ;;           (alpha, beta ... -> beta ...) -> (bush(alpha) -> beta ...)
    ;;           combiner                          amaso

    ;; The fold-heap procedure constructs a procedure that returns the
    ;; results of invoking base if amaso is empty.  If amaso is not empty,
    ;; the constructed procedure extracts an element of amaso that bears
    ;; may-precede? to every element of amaso and applies itself
    ;; recursively to a bush that contains the remaining elements of amaso
    ;; and satisfies the heap invariants with respect to may-precede?,
    ;; returning the results of applying combiner to the extracted value
    ;; and the results of the recursive invocation.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any elements of amaso.
    ;;   combiner can receive any element of amaso and the results of an
    ;;     invocation of base.
    ;;   combiner can receive any element of amaso and the results of any
    ;;     invocation of combiner.
    ;;   amaso satisfies the heap invariants with respect to may-precede?.

    (define (fold-heap may-precede? base combiner)
      (let ((extract-from-heap (heap-extractor may-precede?)))
        (rec (folder amaso)
          (if (empty-heap? amaso)
              (base)
              (receive (chosen others) (extract-from-heap amaso)
                (receive recursive-results (folder others)
                  (apply combiner chosen recursive-results)))))))

    ;; ===== unfold-heap ==================================================
    ;; (alpha, alpha -> Boolean), (beta ... -> Boolean),
    ;; may-precede?               final?
    ;;      (beta ... -> alpha), (beta ... -> beta ...) -> 
    ;;      producer             step
    ;;                                           (beta ...  -> bush(alpha))
    ;;                                            arguments

    ;; The unfold-heap procedure constructs a procedure that first
    ;; determines whether the elements of arguments satisfy final?.  If so,
    ;; the constructed procedure returns an empty bush.  Otherwise, it
    ;; returns a bush that satisfies the heap invariants with respect to
    ;; may-precede?  and contains the result of applying producer to the
    ;; elements of arguments, as well as the elements of the the result of
    ;; first applying step to the elements of arguments and then applying
    ;; the constructed procedure recursively to the results.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any results of producer.
    ;;   final? can receive the elements of arguments.
    ;;   final? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy final?, then producer
    ;;     can receive them.
    ;;   If the results of an invocation of step do not satisfy final?,
    ;;     then producer can receive them.
    ;;   If the elements of arguments do not satisfy final?, then step can
    ;;     receive them.
    ;;   If the results of an invocation of step do not satisfy final?,
    ;;     then step can receive them.

    (define (unfold-heap may-precede? final? producer step)
      (build final?
             (constant (empty-heap))
             producer
             step
             (heap-adjoiner may-precede?)))

    ;; ===== process-heap =================================================
    ;; (alpha, alpha -> Boolean), (-> beta ...),
    ;; may-precede?               base          
    ;;           (alpha, beta ... -> beta ...) -> (bush(alpha) -> beta ...)
    ;;           combiner                          amaso

    ;; The process-heap procedure constructs a procedure that iteratively
    ;; applies combiner to an element of amaso and the results of the
    ;; previous iteration (or to the results of invoking base, if there was
    ;; no previous iteration).  The iteration takes up the elements of
    ;; amaso in the order specified by may-precede?. The constructed
    ;; procedure returns the results of the last application of combiner.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any elements of amaso.
    ;;   combiner can receive an element of amaso that bears may-precede?
    ;;     to every element of amaso and the results of an invocation of
    ;;     base.
    ;;   combiner can receive any element of amaso and the results of any
    ;;     invocation of combiner.
    ;;   amaso satisfies the heap invariants with respect to may-precede?.

    (define (process-heap may-precede? base combiner)
      (let ((extract-from-heap (heap-extractor may-precede?)))
        (let ((processor
               (rec (processor amaso . intermediates)
                 (if (empty-heap? amaso)
                     (delist intermediates)
                     (receive (chosen others) (extract-from-heap amaso)
                       (receive new-results
                                (apply combiner chosen intermediates)
                         (apply processor others new-results)))))))
          (receive base-values (base)
            (sect apply processor <> base-values)))))))

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
