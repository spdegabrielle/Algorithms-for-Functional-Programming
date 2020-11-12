;;; Bushes

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 12, 1999
;;; last revised January 9, 2017

(define-library (afp bushes)
  (export bush bush-root bush-children debush empty-bush? non-empty-bush?
          bush? bush=? bush-of bush-of= fold-bush bush->list
          for-all-in-bush? unfold-bush)
  (import (afp primitives)
          (only (afp arithmetic) mod)
          (only (afp constant-procedures) create)
          (only (afp couplers) pipe)
          (only (afp lists) first rest empty-list? non-empty-list?
                collect-map for-all? zip))
  (begin

    ;; ===== bush =========================================================
    ;; -> bush(alpha)

    ;; With no arguments, the bush procedure returns an empty bush.

    ;; alpha, bush(alpha) ... -> bush(alpha)
    ;; root   children

    ;; With one or more arguments, the bush procedure returns a bush with
    ;; root as its root and children as its children.

    (define bush list)

    ;; ===== bush-root ====================================================
    ;; bush(alpha) -> alpha
    ;; arbusto

    ;; The bush-root procedure returns the root of arbusto.

    ;; Precondition:
    ;;   arbusto is not empty.

    (define bush-root first)

    ;; ===== bush-children ================================================
    ;; bush(alpha) -> bush(alpha) ...
    ;; arbusto

    ;; The bush-children procedure returns the children of arbusto.

    ;; Precondition:
    ;;   arbusto is not empty.

    (define bush-children rest)

    ;; ===== debush =======================================================
    ;; bush(alpha) -> | alpha, bush(alpha) ...
    ;; arbusto

    ;; The debush procedure returns all of the components of arbusto.

    (define debush delist)

    ;; ===== empty-bush? ==================================================
    ;; bush(any) -> Boolean
    ;; arbusto

    ;; The empty-bush? predicate determines whether arbusto is empty.

    (define empty-bush? empty-list?)

    ;; ===== non-empty-bush? ==============================================
    ;; bush(any) -> Boolean
    ;; arbusto

    ;; The non-empty-bush? predicate determines whether its argument is a
    ;; non-empty bush.

    (define non-empty-bush? non-empty-list?)

    ;; ===== bush? ========================================================
    ;; any       -> Boolean
    ;; something

    ;; The bush? predicate determines whether something is a bush.

    (define (bush? something)
      (and (list? something)
           (or (empty-list? something)
               (and (non-empty-list? something)
                    (for-all? bush? (rest something))))))

    ;; ===== bush=? =======================================================
    ;; bush(alpha), bush(beta) -> Boolean
    ;; left         right

    ;; The bush=? predicate determines whether left and right are the same
    ;; bush, that is, whether they have the same structure and contain the
    ;; same values at corresponding positions.

    (define (bush=? left right)
      (or (and (empty-bush? left) (empty-bush? right))
          (and (non-empty-bush? left)
               (non-empty-bush? right)
               (equal? (bush-root left) (bush-root right))
               (let ((lefts (bush-children left))
                     (rights (bush-children right)))
                 (and (= (length lefts) (length rights))
                      (for-all? (pipe delist bush=?)
                                (zip lefts rights)))))))

    ;; ===== bush-of ======================================================
    ;; (any -> Boolean)       -> (any       -> Boolean)
    ;; right-type-of-element?     something

    ;; The bush-of procedure constructs a predicate that determines whether
    ;; something is a bush containing only values that satisfy
    ;; right-type-of-element?.

    ;; Precondition:
    ;;   right-type-of-element? can receive any value.

    (define (bush-of right-type-of-element?)
      (rec (ok? something)
        (or (empty-list? something)
            (and (non-empty-list? something)
                 (right-type-of-element? (first something))
                 (for-all? ok? (rest something))))))

    ;; ===== bush-of= =====================================================
    ;; (alpha, beta -> Boolean) -> (bush(alpha), bush(beta) -> Boolean)
    ;; element=?                    left         right

    ;; The bush-of= procedure constructs a predicate that determines
    ;; whether left and right are the same bush, that is, whether they have
    ;; the same structure and contain values that satisfy element=? in
    ;; corresponding positions.

    ;; Precondition:
    ;;   element=? can receive any element of left as its first argument
    ;;   and any element of right as its second.

    (define (bush-of= element=?)
      (rec (equivalent? left right)
        (or (and (empty-bush? left)
                 (empty-bush? right))
            (and (non-empty-bush? left)
                 (non-empty-bush? right)
                 (element=? (bush-root left) (bush-root right))
                 (let ((lefts (bush-children left))
                       (rights (bush-children right)))
                   (and (= (length lefts) (length rights))
                        (for-all? (pipe delist equivalent?)
                                  (zip lefts rights))))))))

    ;; ===== fold-bush ===================================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            combiner
    ;;                                           (bush(beta) -> alpha ...)
    ;;                                            arbusto

    ;; The fold-bush procedure constructs a procedure that returns the
    ;; results of invoking base if arbusto is empty.  If arbusto is
    ;; non-empty, the constructed procedure applies itself recursively to
    ;; each child of arbusto and returns the results of applying combiner
    ;; to the root of arbusto and the results of the recursive invocations.

    ;; Preconditions:
    ;;   combiner can receive the root of any leaf of arbusto and the
    ;;     results of an invocation of base.
    ;;   combiner can receive the root of any internal node of arbusto and
    ;;     the results of any number of invocations of combiner.

    (define (fold-bush base combiner)
      (rec (folder arbusto)
        (if (empty-bush? arbusto)
            (base)
            (apply combiner (bush-root arbusto)
                            (collect-map folder
                                         (bush-children arbusto))))))

    ;; ===== bush->list ===================================================
    ;; bush(alpha) -> list(alpha)
    ;; arbusto

    ;; The bush->list procedure constructs a list of the values in arbusto.

    (define bush->list
      (fold-bush (create (list))
                 (lambda (root . recursive-results)
                   (cons root (apply append recursive-results)))))

    ;; ===== for-all-in-bush? =============================================
    ;; (alpha -> Boolean), bush(alpha) -> Boolean
    ;; condition-met?      arbusto

    ;; The for-all-in-bush? procedure determines whether each of the values
    ;; in arbusto satisfies condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any value in arbusto.

    (define (for-all-in-bush? condition-met? arbusto)
      ((rec (ok? subbush)
         (or (empty-bush? subbush)
             (and (condition-met? (bush-root subbush))
                  (for-all? ok? (bush-children subbush)))))
       arbusto))

    ;; ===== unfold-bush ==================================================

    ;; (alpha -> Boolean), (alpha -> beta), (alpha -> alpha ...) ->
    ;; final?              producer         step
    ;;                                                (alpha -> bush(beta))
    ;;                                                 guide

    ;; The unfold-list procedure constructs a procedure that first
    ;; determines whether guide satisfies final?.  If so, the constructed
    ;; procedure returns the empty bush.  Otherwise, it returns a non-empty
    ;; bush in which the root element is the result of applying producer to
    ;; guide, and the children are the results of first applying step to
    ;; guide and then applying the constructed procedure recursively to
    ;; each of the results.

    ;; Preconditions:
    ;;   final? can receive guide.
    ;;   final? can receive the result of any invocation of step.
    ;;   If guide does not satisfy final?, then producer can receive it.
    ;;   If any of the the results of an invocation of step does not
    ;;     satisfy final?, then producer can receive it.
    ;;   If guide does not satisfy final?, then step can receive it.
    ;;   If any of the results of an invocation of step does not satisfy
    ;;     final?, then step can receive it.

    (define (unfold-bush final? producer step)
      (rec (unfolder guide)
        (if (final? guide)
            (bush)
            (receive step-results (step guide)
              (apply bush (producer guide)
                          (map unfolder step-results))))))))

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
