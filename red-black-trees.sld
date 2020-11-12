;;; Red-black trees

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created December 11, 1998
;;; last revised January 6, 2017

(define-library (afp red-black-trees)
  (export color? color=? red-black-tree? red-black-tree=? red?
          red-black-tree-element satisfies-red-black-invariant?
          red-black-search-tree? color-flip rotate-left rotate-right
          lift-red left-leaning? lean-left rebalance-reds force-color
          put-into-red-black-tree search-red-black-tree
          safe-red-black-tree? avoid-unsafe-right avoid-unsafe-left
          extract-leftmost-from-red-black-tree join-red-black-trees
          extract-from-red-black-tree)
  (import (afp primitives)
          (only (afp arithmetic) add1)
          (only (afp constant-procedures) values? constant)
          (only (afp couplers) pipe dispatch cross)
          (only (afp adapters)
                >initial identity converse ~initial ~next compare-by)
          (only (afp predicate-operations) ^not ^et ^vel ^if)
          (only (afp pairs) decons pair-of pair-of=)
          (only (afp boxes) box box?)
          (only (afp lists) ^or run)
          (only (afp trees)
                make-empty-tree empty-tree? make-non-empty-tree
                non-empty-tree-root non-empty-tree-left
                non-empty-tree-right de-non-empty-tree non-empty-tree?
                singleton-tree tree-of tree-of=)
          (only (afp binary-search-trees)
                binary-search-tree-invariant?))
  (begin

    ;; ===== color? =======================================================
    ;; any       -> Boolean
    ;; something

    ;; The color? predicate determines whether something is a color -- that
    ;; is, one of the symbols denoting the specific colors red and black.

    (define color?
      (^et symbol?
           (^vel (sect symbol=? <> 'red)
                 (sect symbol=? <> 'black))))

    ;; ===== color=? ======================================================
    ;; color, color -> Boolean
    ;; left   right

    ;; The color=? predicate determines whether left and right are the same
    ;; color.

    (define color=? symbol=?)

    ;; ===== red-black-tree? ==============================================
    ;; any       -> Boolean
    ;; something

    ;; The red-black-tree? predicate determines whether something has the
    ;; structure of a red-black tree.

    (define red-black-tree? (tree-of (pair-of color? values?)))

    ;; ===== red-black-tree=? =============================================
    ;; red-black-tree(any), red-black-tree(any) -> Boolean
    ;; left                 right

    ;; The red-black-tree=? predicate determines whether left and right
    ;; have the same shape and contain the same values and colors at
    ;; corresponding positions.

    (define red-black-tree=? (tree-of= (pair-of= color=? equal?)))

    ;; ===== red? =========================================================
    ;; red-black-tree(any) -> Boolean
    ;; tr

    ;; The red? predicate determines whether tr is red.

    (define red?
      (^et non-empty-tree?
           (run non-empty-tree-root car (sect color=? <> 'red))))

    ;; ===== red-black-tree-element =======================================
    ;; red-black-tree(alpha) -> alpha
    ;; tr

    ;; The red-black-tree-element procedure returns the value stored in the
    ;; root of tr.

    ;; Precondition:
    ;;   tr is not empty.

    (define red-black-tree-element (pipe non-empty-tree-root cdr))

    ;; ===== satisfies-red-black-invariant? ===============================
    ;; red-black-tree(any) -> Boolean
    ;; tr

    ;; The satisfies-red-black-invariant? predicate determines whether tr,
    ;; which has the right structure to be a red-black tree, satisfies the
    ;; red-black invariant.

    ;; This determination involves the computation of a numerical attribute
    ;; of tr, sometimes called its "black height," but discussed in
    ;; _Algorithms for functional programming_ as a function m that takes a
    ;; red-black tree as its argument and returns a positive integer.  In a
    ;; tree that satisfies the red-black invariant, the value of the
    ;; function m is the number of black nodes along any branch, starting
    ;; from the root and ending with an empty tree.  One clause of the
    ;; red-black invariant requires this function to be well defined: Any
    ;; non-empty tree must have left and right subtrees to which m assigns
    ;; the same value.

    (define satisfies-red-black-invariant?
      (pipe (rec (m-or-failure tr)
              (if (empty-tree? tr)
                  0
                  (let ((tr-left (non-empty-tree-left tr))
                        (tr-right (non-empty-tree-right tr)))
                    (if (and (red? tr)
                             (or (red? tr-left) (red? tr-right)))
                        #f
                        (let ((left-m (m-or-failure tr-left)))
                          (if (not left-m)
                              #f
                              (let ((right-m (m-or-failure tr-right)))
                                (if (not right-m)
                                    #f
                                    (if (not (= left-m right-m))
                                        #f
                                        (if (red? tr)
                                            left-m
                                            (add1 left-m)))))))))))
            natural-number?))

    ;; ===== red-black-search-tree? =======================================
    ;; (alpha, alpha -> Boolean), any       -> Boolean
    ;; may-precede?               something

    ;; The red-black-search-tree? procedure determines whether something is
    ;; a red-black tree, satisfying both the red-black invariant and the
    ;; binary-search-tree invariant (with respect to may-precede?).

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   If something is a tree of pairs, then may-precede? can receive the
    ;;     cdrs of any of those pairs.

    (define (red-black-search-tree? may-precede? something)
      (and (red-black-tree? something)
           (satisfies-red-black-invariant? something)
           (binary-search-tree-invariant? (compare-by cdr may-precede?)
                                          something)))

    ;; ===== color-flip ===================================================
    ;; red-black-tree(alpha) -> red-black-tree(alpha)
    ;; tr

    ;; The color-flip procedure changes the color of tr and the colors of
    ;; both of its immediate subtrees.

    ;; Preconditions:
    ;;   tr is not empty.
    ;;   Neither subtree of tr is empty.
    ;;   The color of tr is different from the color of both of its
    ;;     subtrees.

    (define color-flip
      (let ((opposite (lambda (col)
                        (if (color=? col 'red) 'black 'red))))
        (let ((flip-root (run decons (~initial opposite) cons)))
          (let ((flip (run de-non-empty-tree
                           (~initial flip-root)
                           make-non-empty-tree)))
            (run de-non-empty-tree
                 (cross flip-root flip flip)
                 make-non-empty-tree)))))

    ;; ===== rotate-left ==================================================
    ;; red-black-tree(alpha) -> red-black-tree(alpha)
    ;; tr

    ;; The rotate-left procedure constructs a red-black tree containing the
    ;; same elements as tr, but with the element at the root of its right
    ;; subtree raised into the root position, the former root becoming its
    ;; left subtree.  Colors and subtrees are shifted around so as to
    ;; preserve the binary-search-tree invariant and to restore the
    ;; red-black invariant.

    ;; Preconditions:
    ;;   tr is not empty.
    ;;   The right subtree of tr is not empty.
    ;;   The right subtree of tr is red.

    (define (rotate-left tr)
      (let ((right (non-empty-tree-right tr)))
        (make-non-empty-tree
          (cons (if (red? tr) 'red 'black) (red-black-tree-element right))
          (make-non-empty-tree (cons 'red (red-black-tree-element tr))
                               (non-empty-tree-left tr)
                               (non-empty-tree-left right))
          (non-empty-tree-right right))))

    ;; ===== rotate-right =================================================
    ;; red-black-tree(alpha) -> red-black-tree(alpha)
    ;; tr

    ;; The rotate-right procedure constructs a red-black tree containing
    ;; the same elements as tr, but with the element at the root of its
    ;; left subtree raised into the root position, the former root becoming
    ;; its right subtree.  Colors and subtrees are shifted around so as to
    ;; preserve the binary-search-tree invariant and to restore the
    ;; red-black invariant.

    ;; Preconditions:
    ;;   tr is not empty.
    ;;   The left subtree of tr is not empty.
    ;;   The left subtree of tr is red.

    (define (rotate-right tr)
      (let ((left (non-empty-tree-left tr)))
        (make-non-empty-tree
          (cons (if (red? tr) 'red 'black) (red-black-tree-element left))
          (non-empty-tree-left left)
          (make-non-empty-tree (cons 'red (red-black-tree-element tr))
                               (non-empty-tree-right left)
                               (non-empty-tree-right tr)))))

    ;; ===== lift-red =====================================================
    ;; red-black-tree(alpha) -> red-black-tree(alpha)
    ;; tr

    ;; If both subtrees of tr are red, the lift-red procedure constructs a
    ;; color-flipped version of it; otherwise, it returns tr unchanged.

    ;; Preconditions:
    ;;   tr is black.
    ;;   tr is not empty.

    (define lift-red
      (^if (^et (pipe non-empty-tree-left red?)
                (pipe non-empty-tree-right red?))
           color-flip
           identity))

    ;; ===== left-leaning? ================================================
    ;; red-black-tree(any) -> Boolean
    ;; tr

    ;; The left-leaning? predicate determines whether tr satisfies the
    ;; left-leaning invariant, that is, whether every subtree that has one
    ;; red subtree and one black subtree has the red one on the left.

    (define (left-leaning? tr)
      (or (empty-tree? tr)
          (receive (ignored left right) (de-non-empty-tree tr)
            (and (or (red? left) (not (red? right)))
                 (left-leaning? left)
                 (left-leaning? right)))))

    ;; ===== lean-left ====================================================
    ;; red-black-tree(alpha) -> red-black-tree(alpha)
    ;; tr

    ;; If tr has a black left subtree and a red right subtree, the
    ;; lean-left procedure constructs a left-rotated version of it;
    ;; otherwise, it returns tr unchanged.

    ;; Precondition:
    ;;   tr is not empty.

    (define lean-left
      (^if (^et (pipe non-empty-tree-left (^not red?))
                (pipe non-empty-tree-right red?))
           rotate-left
           identity))

    ;; ===== rebalance-reds ===============================================
    ;; red-black-tree(alpha) -> red-black-tree(alpha)
    ;; tr

    ;; If tr has a red left subtree that also has a red left subtree, the
    ;; rebalance-reds procedure constructs a right-rotated version of it;
    ;; otherwise, it returns tr unchanged.

    ;; Precondition:
    ;;   tr is not empty.

    (define rebalance-reds
      (^if (pipe non-empty-tree-left
                 (^et red? (pipe non-empty-tree-left red?)))
           rotate-right
           identity))

    ;; ===== force-color ==================================================
    ;; color,    red-black-tree(alpha) -> red-black-tree(alpha)
    ;; new-color tr

    ;; The force-color procedure constructs a red-black tree similar to tr,
    ;; but with new-color as its color.

    ;; Precondition:
    ;;   tr is not empty.

    (define (force-color new-color tr)
      (receive (root left right) (de-non-empty-tree tr)
        (receive (ignored element) (decons root)
          (make-non-empty-tree (cons new-color element) left right))))

    ;; ===== put-into-red-black-tree ======================================
    ;; (alpha, alpha -> Boolean) ->
    ;; may-precede?
    ;;              (alpha, red-black-tree(alpha) -> red-black-tree(alpha))
    ;;               new    tr

    ;; The put-into-red-black-tree procedure constructs a procedure that,
    ;; in turn, constructs a black, left-leaning red-black tree that
    ;; satisfies the red-black invariant, is ordered with respect to
    ;; may-precede?, and contains all of the values in tr as well as new.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive new and any value in tr.
    ;;   tr satisfies the red-black invariant.
    ;;   tr satisfies the left-leaning invariant.
    ;;   tr satisfies the binary-search-tree invariant with respect to
    ;;     may-precede?.

    (define (put-into-red-black-tree may-precede?)
      (lambda (new tr)
        ((pipe (rec (putter subtree)
                 (if (empty-tree? subtree)
                     (singleton-tree (cons 'red new))
                     (receive (root left right)
                              (de-non-empty-tree (lift-red subtree))
                       (rebalance-reds
                         (lean-left
                           (if (may-precede? new (cdr root))
                               (make-non-empty-tree
                                 root (putter left) right) 
                               (make-non-empty-tree
                                 root left (putter right))))))))
              (sect force-color 'black <>))
         tr)))

    ;; ===== search-red-black-tree ========================================
    ;; (alpha, alpha -> Boolean) ->
    ;; may-precede?
    ;;               (alpha, red-black-tree(alpha) -> box(alpha) | Boolean)
    ;;                sought tr

    ;; The search-red-black-tree procedure constructs a procedure that
    ;; searches within tr for a value equivalent to sought, using as its
    ;; criterion of sameness the equivalence relation induced by
    ;; may-precede?.  If the search is successful, the constructed
    ;; procedure returns a box containing the matching value from tr; if
    ;; not, it returns #f, unboxed.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive sought and any value in tr, in either
    ;;     order.
    ;;   tr satisfies the binary-search-tree invariant with respect to
    ;;     may-precede?.

    (define (search-red-black-tree may-precede?)
      (lambda (sought tr)
        ((rec (searcher subtree)
           (if (empty-tree? subtree)
               #f
               (receive (root left right) (de-non-empty-tree subtree)
                 (if (not (may-precede? (cdr root) sought))
                     (searcher left)
                     (if (not (may-precede? sought (cdr root)))
                         (searcher right)
                         (box (cdr root)))))))
         tr)))

    ;; ===== safe-red-black-tree? =========================================
    ;; red-black-tree(any) -> Boolean
    ;; tr

    ;; The safe-red-black-tree procedure determines whether tr is "safe,"
    ;; that is, whether it can be guaranteed that an attempt to extract
    ;; from it will either fail (because tr contains no matching element)
    ;; or result in the deletion of a red leaf.

    ;; Precondition:
    ;;   tr satisfies the left-leaning invariant.

    (define safe-red-black-tree?
      (^or empty-tree? red? (pipe non-empty-tree-left red?)))

    ;; ===== avoid-unsafe-right ===========================================
    ;; red-black-tree(alpha) -> red-black-tree(alpha)
    ;; tr

    ;; The avoid-unsafe-right procedure returns tr, if its right subtree is
    ;; safe; if not, it constructs a red-black tree that contains the same
    ;; elements as tr, satisfies the red-black invariant, and has a safe
    ;; right subtree that satisfies the left-leaning invariant.

    ;; Preconditions:
    ;;   tr is not empty.
    ;;   tr is safe.
    ;;   tr satisfies the red-black invariant.
    ;;   tr satisfies the left-leaning invariant.

    (define avoid-unsafe-right
      (^if (pipe non-empty-tree-right safe-red-black-tree?)
           identity
           (^if (pipe non-empty-tree-left red?)
                rotate-right
                (pipe color-flip
                      (^if (run non-empty-tree-left
                                non-empty-tree-left
                                red?)
                           (run rotate-right
                                color-flip
                                de-non-empty-tree
                                (cross identity identity lean-left)
                                make-non-empty-tree)
                           identity)))))

    ;; ===== avoid-unsafe-left ============================================
    ;; red-black-tree(alpha) -> red-black-tree(alpha)
    ;; tr

    ;; The avoid-unsafe-left procedure returns tr, if its left subtree is
    ;; safe; if not, it constructs a red-black tree that contains the same
    ;; elements as tr, satisfies to the red-black and left-leaning
    ;; invariants, and has a safe left subtree.

    ;; Preconditions:
    ;;   tr is not empty.
    ;;   tr is safe.
    ;;   tr satisfies the red-black invariant.
    ;;   tr satisfies the left-leaning invariant.

    (define avoid-unsafe-left
      (^if (pipe non-empty-tree-left safe-red-black-tree?)
           identity
           (pipe color-flip
                 (^if (run non-empty-tree-right non-empty-tree-left red?)
                      (run de-non-empty-tree
                           (cross identity identity rotate-right)
                           make-non-empty-tree
                           rotate-left
                           color-flip
                           de-non-empty-tree
                           (cross identity identity lean-left)
                           make-non-empty-tree)
                      identity))))

    ;; ===== extract-leftmost-from-red-black-tree =========================
    ;; red-black-tree(alpha) -> alpha, red-black-tree(alpha)
    ;; tr

    ;; The extract-leftmost-from-red-black-tree extracts the leftmost
    ;; element of tr and returns it, along with a black, left-leaning
    ;; red-black tree containing the rest of the elements of tr and
    ;; satisfying the red-black invariant.

    ;; Preconditions:
    ;;   tr is not empty.
    ;;   tr is safe.
    ;;   tr satisfies the red-black invariant.
    ;;   tr satisfies the left-leaning invariant.

    (define (extract-leftmost-from-red-black-tree tr)
      (if (empty-tree? (non-empty-tree-left tr))
          (values (red-black-tree-element tr) (make-empty-tree))
          (receive (root left right)
                   (de-non-empty-tree (avoid-unsafe-left tr))
            (receive (chosen others)
                     (extract-leftmost-from-red-black-tree left)
              (values chosen (lean-left
                               (make-non-empty-tree root others right)))))))

    ;; ===== join-red-black-trees =========================================
    ;; color,     red-black-tree(alpha), red-black-tree(alpha) ->
    ;; root-color left                   right
    ;;                                                red-black-tree(alpha)

    ;; The join-red-black-trees procedure constructs a left-leaning
    ;; red-black tree that contains all of the elements of left and right,
    ;; satisfies the red-black invariant, and has root-color as its color
    ;; (unless left or right is empty, in which case the result is black
    ;; regardless of the value of root-color).

    ;; Preconditions:
    ;;   If left is empty, then right is empty.
    ;;   left satisfies the red-black invariant.
    ;;   left satisfies the left-learning invariant.
    ;;   right is safe.
    ;;   right satisfies the red-black invariant.
    ;;   right satisfies the left-learning invariant.
    ;;   m(left) = m(right).

    (define (join-red-black-trees root-color left right)
      (if (empty-tree? left)
          (make-empty-tree)
          (if (empty-tree? right)
              (force-color 'black left)
              (receive (leftmost new-right)
                       (extract-leftmost-from-red-black-tree right)
                (make-non-empty-tree (cons root-color leftmost)
                                     left
                                     new-right)))))

    ;; ===== extract-from-red-black-tree ==================================
    ;; (alpha, alpha -> Boolean) ->
    ;; may-precede?
    ;;     (alpha, red-black-tree(alpha) ->
    ;;      sought tr
    ;;                         box(alpha) | Boolean, red-black-tree(alpha))

    ;; The extract-from-red-black-tree procedure constructs a procedure
    ;; that searches within tr for a value equivalent to sought, using as
    ;; its criterion of sameness the equivalence relation induced by
    ;; may-precede?.  If the search is successful, the constructed
    ;; procedure returns a box containing the matching value from tr as its
    ;; first result; if not, it returns #f, unboxed.  In either case, the
    ;; second result is a black, left-leaning red-black tree that satisfies
    ;; the binary-search-tree invariant with respect to may-precede? and
    ;; contains all of the other values in tr.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive sought and any value in tr, in either
    ;;     order.
    ;;   tr satisfies the red-black invariant.
    ;;   tr satisfies the left-leaning invariant.
    ;;   tr satisfies the binary-search-tree invariant with respect to
    ;;     may-precede?.

    (define (extract-from-red-black-tree may-precede?)
      (lambda (sought tr)
        ((run
          (^if safe-red-black-tree? identity (sect force-color 'red <>))
          (rec (extracter subtree)
            (if (empty-tree? subtree)
                (values #f subtree)
                (if (may-precede? (red-black-tree-element subtree) sought)
                    (receive (root left right)
                        (de-non-empty-tree
                          (avoid-unsafe-right subtree))
                      (if (may-precede? sought (cdr root))
                          (values (box (cdr root))
                                  (join-red-black-trees
                                    (car root) left right))
                          (receive (result new-right) (extracter right)
                            (values result
                                    (lean-left (make-non-empty-tree
                                                 root left new-right))))))
                    (receive (root left right)
                        (de-non-empty-tree
                          (avoid-unsafe-left subtree))
                      (receive (result new-left) (extracter left)
                        (values result
                                (lean-left (make-non-empty-tree
                                             root new-left right))))))))
          (~next (^if empty-tree? identity (sect force-color 'black <>))))
         tr)))))

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
