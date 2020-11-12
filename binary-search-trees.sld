;;; Binary search trees

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 9, 1999
;;; last revised January 13, 2017

(define-library (afp binary-search-trees)
  (export put-into-binary-search-tree search-binary-search-tree
          binary-search-tree-invariant?
          extract-from-binary-search-tree join-trees)
  (import (afp primitives)
          (only (afp boxes) box)
          (only (afp trees)
                empty-tree? make-empty-tree make-non-empty-tree
                de-non-empty-tree singleton-tree rightmost leftmost
                extract-rightmost)
          (afp bags))
  (begin

    ;; ===== put-into-binary-search-tree ==================================
    ;; (alpha, alpha -> Boolean) -> (alpha, tree(alpha) -> tree(alpha))
    ;; may-precede?                  new    bst

    ;; The put-into-binary-search-tree procedure constructs a procedure
    ;; that, in turn, constructs a tree that satisfies the
    ;; binary-search-tree invariant with respect to may-precede? and is
    ;; similar to bst except that it contains new along with all of the
    ;; values in bst.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive new and any value in bst.
    ;;   bst satisfies the binary-search-tree invariant with respect to
    ;;     may-precede?.

    (define (put-into-binary-search-tree may-precede?)
      (lambda (new bst)
        ((rec (putter subtree)
           (if (empty-tree? subtree)
               (singleton-tree new)
               (receive (root left right) (de-non-empty-tree subtree)
                 (if (may-precede? new root)
                     (make-non-empty-tree root (putter left) right) 
                     (make-non-empty-tree root left (putter right))))))
         bst)))

    ;; ===== search-binary-search-tree ====================================
    ;; (alpha, alpha -> Boolean) ->
    ;; may-precede?
    ;;                         (alpha, tree(alpha) -> box(alpha) | Boolean)
    ;;                          sought bst

    ;; The search-binary-search-tree procedure constructs a procedure that
    ;; searches within bst for a value equivalent to sought, using as its
    ;; criterion of sameness the equivalence relation induced by
    ;; may-precede?.  If the search is successful, the constructed
    ;; procedure returns a box containing the matching value from bst; if
    ;; not, it returns #f, unboxed.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive sought and any value in bst, in either
    ;;     order.
    ;;   bst satisfies the binary-search-tree invariant with respect to
    ;;     may-precede?.

    (define (search-binary-search-tree may-precede?)
      (lambda (sought bst)
        ((rec (searcher subtree)
           (if (empty-tree? subtree)
               #f
               (receive (root left right) (de-non-empty-tree subtree)
                 (if (not (may-precede? root sought))
                     (searcher left)
                     (if (not (may-precede? sought root))
                         (searcher right)
                         (box root))))))
         bst)))

    ;; ===== binary-search-tree-invariant? ================================
    ;; (alpha, alpha -> Boolean), tree(alpha) -> Boolean
    ;; may-precede?               tr

    ;; The binary-search-tree-invariant? predicate determines whether tr
    ;; satisfies the binary-search-tree invariant with respect to
    ;; may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values in tr.

    (define (binary-search-tree-invariant? may-precede? tr)
      ((rec (ok? subtree)
         (or (empty-tree? subtree)
             (receive (root left right) (de-non-empty-tree subtree)
               (and (ok? left)
                    (ok? right)
                    (or (empty-tree? left)
                        (may-precede? (rightmost left) root))
                    (or (empty-tree? right)
                        (may-precede? root (leftmost right)))))))
       tr))

    ;; ===== extract-from-binary-search-tree ==============================
    ;; (alpha, alpha -> Boolean) ->
    ;; may-precede?
    ;;            (alpha, tree(alpha) -> box(alpha) | Boolean, tree(alpha))
    ;;             sought bst

    ;; The extract-from-binary-search-tree procedure constructs a procedure
    ;; that searches within bst for a value equivalent to sought, using as
    ;; its criterion of sameness the equivalence relation induced by
    ;; may-precede?.  If the search is successful, the constructed
    ;; procedure returns a box containing the matching value from bst and a
    ;; tree that satisfies the binary-search-tree invariant with respect to
    ;; may-precede? and contains all of the other values in bst; if not,
    ;; the constructed procedure returns #f, unboxed, and bst.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive sought and any value in bst, in either
    ;;     order.
    ;;   bst satisfies the binary-search-tree invariant with respect to
    ;;     may-precede?.

    (define (extract-from-binary-search-tree may-precede?)
      (lambda (sought bst)
        ((rec (extracter subtree)
           (if (empty-tree? subtree)
               (values #f (make-empty-tree))
               (receive (root left right) (de-non-empty-tree subtree)
                 (if (not (may-precede? root sought))
                     (receive (extracted others) (extracter left)
                       (values extracted
                               (make-non-empty-tree root others right)))
                     (if (not (may-precede? sought root))
                         (receive (extracted others) (extracter right)
                           (values extracted
                                   (make-non-empty-tree root left others)))
                         (values (box root) (join-trees left right)))))))
         bst)))

    ;; ===== join-trees ===================================================
    ;; tree(alpha), tree(alpha) -> tree(alpha)
    ;; fore         aft

    ;; The join-trees procedure constructs a tree containing all of the
    ;; values in fore and all of the values in aft.  If fore and aft
    ;; satisfy the binary-search-tree invariant with respect to some
    ;; ordering relation, and every value in fore bears that relation to
    ;; every value in aft, then the constructed tree also satisfies that
    ;; invariant.

    (define (join-trees fore aft)
      (if (empty-tree? fore)
          aft
          (if (empty-tree? aft)
              fore
              (receive (new-root new-fore) (extract-rightmost fore)
                (make-non-empty-tree new-root new-fore aft)))))))

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
