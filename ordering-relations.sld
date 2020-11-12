;;; Ordering relations

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 21, 1998
;;; last revised January 6, 2017

(define-library (afp ordering-relations)
  (export ordered? prior-by arrange-by extreme extreme-and-others
          extreme-in-set extreme-in-list extreme-and-others-in-list
          compound-ordering pair-lex fixed-list-lex list-lex inserter)
  (import (afp primitives)
          (only (afp constant-procedures) values? create)
          (only (afp couplers) pipe)
          (only (afp adapters)
                >initial >next >exch converse compare-by)
          (only (afp predicate-operations) ^not ^et ^vel ^if)
          (only (afp lists)
                first rest empty-list? non-empty-list? prepend deprepend
                fold-list extend-to-variable-arity)
          (only (afp bags) bag put-into-bag take-from-bag fold-bag)
          (only (afp sets) take-from-set fold-set))
  (begin

    ;; ===== ordered? =====================================================
    ;; (alpha, alpha -> Boolean), list(alpha) -> Boolean
    ;; may-precede?               ls

    ;; The ordered? procedure determines whether ls is ordered with respect
    ;; to may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any elements of ls.

    (define (ordered? may-precede? ls)
      ((^vel empty-list?
             (rec (first-ordered? sublist)
               (receive (initial others) (deprepend sublist)
                 (or (empty-list? others)
                     (and (may-precede? initial (first others))
                          (first-ordered? others))))))
       ls))

    ;; ===== prior-by =====================================================
    ;; (alpha, alpha -> Boolean) -> (alpha, alpha -> alpha)
    ;; may-precede?                  left   right

    ;; The prior-by procedure constructs a procedure that returns whichever
    ;; of left and right bears may-precede? to the other.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive left and right.

    (define prior-by (sect ^if <> >initial >next))

    ;; ===== arrange-by ===================================================
    ;; (alpha, alpha -> Boolean) -> (alpha, alpha -> alpha, alpha)
    ;; may-precede?                  left   right

    ;; The arrange-by procedure constructs a procedure that returns left
    ;; and right, placing first whichever of them bears may-precede? to the
    ;; other.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive left and right.

    (define arrange-by (sect ^if <> values >exch))

    ;; ===== extreme ======================================================
    ;; (alpha, alpha -> Boolean) -> (bag(alpha) -> alpha)
    ;; may-precede?                  aro

    ;; The extreme procedure constructs a procedure that returns a value in
    ;; aro that bears may-precede? to every value in aro.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values in aro.
    ;;   aro is not empty.

    (define (extreme may-precede?)
      (let ((prior (prior-by may-precede?)))
        (lambda (aro)
          (receive (starter others) (take-from-bag aro)
            ((fold-bag (create starter) prior) others)))))

    ;; ===== extreme-and-others ===========================================
    ;; (alpha, alpha -> Boolean) -> (bag(alpha) -> alpha, bag(alpha))
    ;; may-precede?                  aro

    ;; The extreme-and-others procedure constructs a procedure that returns
    ;; a value in aro that bears may-precede? to every value in aro and a
    ;; bag containing all of the other values in aro.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values in aro.
    ;;   aro is not empty.

    (define (extreme-and-others may-precede?)
      (let ((arrange (arrange-by may-precede?)))
        (lambda (aro)
          (receive (starter others) (take-from-bag aro)
            ((fold-bag (create starter (bag))
                       (lambda (new so-far unchosen)
                         (receive (leader trailer) (arrange new so-far)
                           (values leader
                                   (put-into-bag trailer unchosen)))))
             others)))))

    ;; ===== extreme-in-set ===============================================
    ;; (alpha, alpha -> Boolean) -> (set(alpha) -> alpha)
    ;; may-precede?                  aro

    ;; The extreme-in-set procedure constructs a procedure that returns a
    ;; value in aro that bears may-precede? to every value in aro.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values in aro.
    ;;   aro is not empty.

    (define (extreme-in-set may-precede?)
      (let ((prior (prior-by may-precede?)))
        (lambda (aro)
          (receive (starter others) (take-from-set aro)
            ((fold-set (create starter) prior) others)))))

    ;; ===== extreme-in-list ==============================================
    ;; (alpha, alpha -> Boolean) -> (list(alpha) -> alpha)
    ;; may-precede?                  ls

    ;; The extreme-in-list procedure constructs a procedure that returns an
    ;; element of ls that bears may-precede? to every element of ls.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any elements of ls.
    ;;   ls is not empty.

    (define (extreme-in-list may-precede?)
      (let ((prior (prior-by may-precede?)))
        (lambda (ls)
          (receive (starter others) (deprepend ls)
            ((fold-list (create starter) prior) others)))))

    ;; ===== extreme-and-others-in-list ===================================
    ;; (alpha, alpha -> Boolean) -> (list(alpha) -> alpha, list(alpha))
    ;; may-precede?                  ls

    ;; The extreme-and-others-in-list procedure constructs a procedure that
    ;; returns an element of ls that bears may-precede? to every element of
    ;; ls and a list containing all of the other elements of ls, in the
    ;; same relative order.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any elements of ls.
    ;;   ls is not empty.

    (define (extreme-and-others-in-list may-precede?)
      (rec (extracter ls)
        (receive (initial others) (deprepend ls)
          (if (empty-list? others)
              (values initial (list))
              (receive (so-far unchosen) (extracter others)
                (if (may-precede? initial so-far)
                    (values initial others)
                    (values so-far (prepend initial unchosen))))))))

    ;; ===== compound-ordering ============================================
    ;; (alpha, alpha -> Boolean) ... -> (alpha, alpha -> Boolean)
    ;; orderings                         left   right

    ;; The compound-ordering procedure constructs an ordering relation that
    ;; applies the elements of orderings, one by one, to left and right
    ;; until one of them determines which takes precedence.

    ;; Preconditions:
    ;;   Every element of orderings is an ordering relation.
    ;;   Every element of orderings can receive left and right.

    (define compound-ordering
      (extend-to-variable-arity values?
                                (lambda (primary tie-breaker)
                                  (^vel (^not (converse primary))
                                        (^et primary tie-breaker)))))

    ;; ===== pair-lex =====================================================
    ;; (alpha, alpha -> Boolean), (beta, beta -> Boolean) ->
    ;; car-may-precede?           cdr-may-precede?
    ;;                    (pair(alpha, beta), pair(alpha, beta) -> Boolean)
    ;;                     left               right

    ;; The pair-lex procedure constructs a lexicographic ordering relation
    ;; for pairs, applying car-may-precede? to their cars as a primary
    ;; ordering and cdr-may-precede? to their cdrs as a tie-breaker.

    ;; Preconditions:
    ;;   car-may-precede? is an ordering relation.
    ;;   car-may-precede? can receive the car of left and the car of right.
    ;;   cdr-may-precede? is an ordering relation.
    ;;   cdr-may-precede? can receive the cdr of left and the cdr of right.

    (define (pair-lex car-may-precede? cdr-may-precede?)
      (compound-ordering (compare-by car car-may-precede?)
                         (compare-by cdr cdr-may-precede?)))

    ;; ===== fixed-list-lex ===============================================
    ;; (alpha, alpha -> Boolean) ... ->
    ;; orderings
    ;;                                (list(alpha), list(alpha) -> Boolean)
    ;;                                 left         right

    ;; The fixed-list-lex procedure constructs a lexicographic ordering of
    ;; lists, applying the first of the ordering relations to the initial
    ;; elements of the lists, then (if necessary) the second ordering
    ;; relation to their next elements, and so on.

    ;; Preconditions:
    ;;   Every element of orderings is an ordering relation.
    ;;   The length of orderings is less than or equal to the length of
    ;;     left.
    ;;   The length of orderings is less than or equal to the length of
    ;;     right.
    ;;   Every element of orderings can receive the element at the
    ;;     corresponding position in left and the element at the
    ;;     corresponding position in right.

    (define fixed-list-lex
      (extend-to-variable-arity values?
                                (lambda (initial-may-precede? for-rest)
                                  (compound-ordering
                                    (compare-by first initial-may-precede?)
                                    (compare-by rest for-rest)))))

    ;; ===== list-lex =====================================================
    ;; (alpha, alpha -> Boolean) -> (list(alpha), list(alpha) -> Boolean)
    ;; may-precede?                  left         right

    ;; The list-lex procedure constructs a lexicographic ordering for
    ;; lists, using may-precede? to compare correspondent elements.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any element of left and any element of
    ;;     right.

    (define (list-lex may-precede?)
      (rec (ok? left right)
        (or (empty-list? left)
            (and (non-empty-list? right)
                 (or (not (may-precede? (first right) (first left)))
                     (and (may-precede? (first left) (first right))
                          (ok? (rest left) (rest right))))))))

    ;; ===== inserter =====================================================
    ;; (alpha, alpha -> Boolean) -> (alpha, list(alpha) -> list(alpha))
    ;; may-precede?                  new    ls

    ;; The inserter procedure constructs a procedure that constructs a
    ;; list, ordered by may-precede?, containing new and the elements of
    ;; ls.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive new and any element of ls.
    ;;   ls is ordered (by may-precede?).

    (define (inserter may-precede?)
      (lambda (new ls)
        ((rec (ins sublist)
           (if (empty-list? sublist)
               (list new)
               (if (may-precede? new (first sublist))
                   (prepend new sublist)
                   (prepend (first sublist) (ins (rest sublist))))))
         ls)))))

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
