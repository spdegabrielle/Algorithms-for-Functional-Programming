;;; Trees

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created March 22, 1999
;;; last revised January 5, 2017

(define-library (afp trees)
  (export make-empty-tree de-empty-tree empty-tree? empty-tree=?
          make-non-empty-tree non-empty-tree-root non-empty-tree-left
          non-empty-tree-right de-non-empty-tree non-empty-tree?
          non-empty-tree=? tree? tree=? singleton-tree tree-of tree-of=
          tree-size fold-tree tree-height detree for-all-in-tree?
          unfold-tree rightmost leftmost extract-rightmost tree->list)
  (import (afp primitives)
          (only (afp arithmetic) add1)
          (only (afp constant-procedures) values? create black-hole
                constant)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) >all-but-initial)
          (only (afp predicate-operations) ^et ^vel)
          (only (afp lists) prepend fold-list catenate run collect-map
                every))
  (begin
    
    ;; ===== trees ========================================================

    ;; Either a tree is empty, or it comprises a value (the root) and two
    ;; subtrees (left and right).  Here both kinds of trees are implemented
    ;; as tuples.

    (define-record-type empty-tree
      (make-empty-tree)
      empty-tree?)

    (define de-empty-tree black-hole)  

    (define empty-tree=? (constant #t))

    (define-record-type non-empty-tree
      (make-non-empty-tree root left right)
      proto-non-empty-tree?
      (root non-empty-tree-root)
      (left non-empty-tree-left)
      (right non-empty-tree-right))

    (define de-non-empty-tree
      (dispatch non-empty-tree-root
                non-empty-tree-left
                non-empty-tree-right))

    (define (non-empty-tree? something)
      (and (proto-non-empty-tree? something)
           (tree? (non-empty-tree-left something))
           (tree? (non-empty-tree-right something))))

    (define (non-empty-tree=? tr-0 tr-1)
      (and (equal? (non-empty-tree-root tr-0) (non-empty-tree-root tr-1))
           (tree=? (non-empty-tree-left tr-0) (non-empty-tree-left tr-1))
           (tree=? (non-empty-tree-right tr-0)
                   (non-empty-tree-right tr-1))))

    ;; ===== tree? ========================================================
    ;; any       -> Boolean
    ;; something

    ;; The tree? predicate determines whether something is a tree -- either
    ;; an empty one or a non-empty one.

    (define tree? (^vel empty-tree? non-empty-tree?))

    ;; ===== tree=? =======================================================
    ;; tree(alpha), tree(alpha) -> Boolean;
    ;; left         right

    ;; The tree=? predicate tests whether left and right are the same tree,
    ;; that is, whether they have the same structure and contain the same
    ;; values at corresponding positions.

    (define tree=? (^vel (every empty-tree?)
                         (^et (every non-empty-tree?)
                              non-empty-tree=?)))

    ;; ===== singleton-tree ===============================================
    ;; alpha -> tree(alpha)
    ;; element

    ;; The singleton-tree procedure constructs a tree with element as its
    ;; root and only element.

    (define singleton-tree
      (sect make-non-empty-tree <> (make-empty-tree) (make-empty-tree)))

    ;; ===== tree-of ======================================================
    ;; (any -> Boolean)       -> (any       -> Boolean)
    ;; right-type-of-element?     something

    ;; The tree-of procedure constructs a predicate that determines whether
    ;; something is a tree containing only values that satisfy
    ;; right-type-of-element?.

    ;; Precondition:
    ;;   right-type-of-element? can receive any value.

    (define (tree-of right-type-of-element?)
      (rec (ok? something)
        (or (empty-tree? something)
            (and (non-empty-tree? something)
                 (right-type-of-element? (non-empty-tree-root something))
                 (ok? (non-empty-tree-left something))
                 (ok? (non-empty-tree-right something))))))

    ;; ===== tree-of= =====================================================
    ;; (alpha, beta -> Boolean) -> (tree(alpha), tree(beta) -> Boolean)
    ;; element=?                    left         right

    ;; The tree-of-procedure constructs a predicate that determines whether
    ;; left and right have the same structure and contain elements that
    ;; satisfy element=? in corresponding positions.

    ;; Precondition:
    ;;   element=? can receive any elements of left and right respectively.

    (define (tree-of= element=?)
      (rec (equivalent? left right)
        (or (and (empty-tree? left)
                 (empty-tree? right))
            (and (non-empty-tree? left)
                 (non-empty-tree? right)
                 (element=? (non-empty-tree-root left)
                            (non-empty-tree-root right))
                 (equivalent? (non-empty-tree-left left)
                              (non-empty-tree-left right))
                 (equivalent? (non-empty-tree-right left)
                              (non-empty-tree-right right))))))

    ;; ===== tree-size ====================================================
    ;; tree(any) -> natural-number
    ;; tr

    ;; The tree-size procedure computes the number of elements in tr.

    (define (tree-size tr)
      (if (empty-tree? tr)
          0
          (add1 (+ (tree-size (non-empty-tree-left tr))
                   (tree-size (non-empty-tree-right tr))))))

    ;; ===== fold-tree ====================================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            combiner
    ;;                                            (tree(beta) -> alpha ...)
    ;;                                             tr

    ;; The fold-tree procedure constructs a procedure that returns the
    ;; results of invoking base if tr is empty.  If tr is non-empty, the
    ;; constructed procedure applies itself recursively to the left and
    ;; right subtrees of tr and returns the results of applying combiner to
    ;; the root of tr and the results of the recursive invocations.

    ;; Preconditions:
    ;;   combiner can receive the root of any leaf of tr and the results of
    ;;     two invocations of base.
    ;;   combiner can receive the root of any internal node of tr and the
    ;;     results of two invocations of combiner.

    (define (fold-tree base combiner)
      (rec (folder tr)
        (if (empty-tree? tr)
            (base)
            (apply combiner
                   (non-empty-tree-root tr)
                   (collect-map folder
                                (list (non-empty-tree-left tr)
                                      (non-empty-tree-right tr)))))))

    ;; ===== tree-height ==================================================
    ;; tree(any) -> natural-number
    ;; tr

    ;; The tree-height procedure computes the height of tr.

    (define tree-height
      (fold-tree (create 0) (run >all-but-initial max add1)))

    ;; ===== detree =======================================================
    ;; tree(alpha) -> alpha ...
    ;; tr

    ;; The detree procedure returns all of the values contained in tr.

    (define detree (fold-tree (create) values))

    ;; ===== for-all-in-tree? =============================================
    ;; (alpha -> Boolean), tree(alpha) -> Boolean
    ;; condition-met?      tr

    ;; The for-all-in-tree? procedure determines whether all of the values
    ;; contained in tr satisfy condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any element of tr.

    (define (for-all-in-tree? condition-met? tr)
      ((rec (ok? subtree)
         (or (empty-tree? subtree)
             (and (condition-met? (non-empty-tree-root subtree))
                  (ok? (non-empty-tree-left subtree))
                  (ok? (non-empty-tree-right subtree)))))
       tr))

    ;; ===== unfold-tree ==================================================
    ;; (alpha ... -> Boolean), (alpha ... -> beta),
    ;; final?                  producer
    ;;     (alpha ... -> alpha ...), (alpha ... -> alpha ...) -> 
    ;;     left-step                 right-step
    ;;                                            (alpha ... -> tree(beta))
    ;;                                             arguments

    ;; The unfold-tree procedure constructs a procedure that first
    ;; determines whether the elements of arguments satisfy final?.  If so,
    ;; the constructed procedure returns the empty tree.  Otherwise, it
    ;; returns a non-empty tree in which the root element is the result of
    ;; applying producer to the elements of arguments, the left subtree is
    ;; the result of first applying left-step to the elements of arguments
    ;; and then applying the constructed procedure recursively to the
    ;; results, and the right subtree is the result of first applying
    ;; right-step to the elements of arguments and then applying the
    ;; constructed procedure recursively to the results.

    ;; Preconditions:
    ;;   final? can receive the elements of arguments.
    ;;   final? can receive the results of any invocation of left-step.
    ;;   final? can receive the results of any invocation of right-step.
    ;;   If the elements of arguments do not satisfy final?, then producer
    ;;     can receive them.
    ;;   If the results of an invocation of left-step do not satisfy
    ;;     final?, then producer can receive them.
    ;;   If the results of an invocation of right-step do not satisfy
    ;;     final?, then producer can receive them.
    ;;   If the elements of arguments do not satisfy final?, then
    ;;     left-step can receive them.
    ;;   If the results of an invocation of left-step do not satisfy
    ;;     final?, then left-step can receive them.
    ;;   If the results of an invocation of right-step do not satisfy
    ;;     final?, then left-step can receive them.
    ;;   If the elements of arguments do not satisfy final?, then
    ;;     right-step can receive them.
    ;;   If the results of an invocation of left-step do not satisfy
    ;;     final?,then right-step can receive them.
    ;;   If the results of an invocation of right-step do not satisfy
    ;;     final?, then right-step can receive them.

    (define (unfold-tree final? producer left-step right-step)
      (rec (unfolder . arguments)
        (if (apply final? arguments)
            (make-empty-tree)
            (make-non-empty-tree (apply producer arguments)
                                 (apply (pipe left-step unfolder)
                                        arguments)
                                 (apply (pipe right-step unfolder)
                                        arguments)))))

    ;; ===== rightmost ====================================================
    ;; tree(alpha) -> alpha
    ;; tr

    ;; The rightmost procedure returns the rightmost value in tr.

    ;; Precondition:
    ;;   tr is not empty.

    (define (rightmost tr)
      (let ((right (non-empty-tree-right tr)))
        (if (empty-tree? right)
            (non-empty-tree-root tr)
            (rightmost right))))

    ;; ===== leftmost =====================================================
    ;; tree(alpha) -> alpha
    ;; tr

    ;; The leftmost procedure returns the leftmost value in tr.

    ;; Precondition:
    ;;   tr is not empty.

    (define (leftmost tr)
      (let ((left (non-empty-tree-left tr)))
        (if (empty-tree? left)
            (non-empty-tree-root tr)
            (leftmost left))))

    ;; ===== extract-rightmost ============================================
    ;; tree(alpha) -> alpha, tree(alpha)
    ;; tr

    ;; The extract-rightmost procedure returns the rightmost value in tr
    ;; and a tree, similar to tr, containing all of the other values in tr.

    ;; Precondition:
    ;;   tr is not empty.

    (define (extract-rightmost tr)
      (receive (root left right) (de-non-empty-tree tr)
        (if (empty-tree? right)
            (values root left)
            (receive (extracted new-right) (extract-rightmost right)
              (values extracted
                      (make-non-empty-tree root left new-right))))))

    ;; ===== tree->list ===================================================
    ;; tree(alpha) -> list(alpha)
    ;; tr

    ;; The tree->list procedure constructs a list of the values in tr, in
    ;; order from leftmost to rightmost.

    (define tree->list
      (fold-tree list
                 (lambda (root from-left from-right)
                   (catenate from-left (prepend root from-right)))))))

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
