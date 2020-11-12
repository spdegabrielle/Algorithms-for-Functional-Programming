;;; Red-black tables

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created December 9, 2011
;;; last revised January 9, 2017

(define-library (afp red-black-tables)
  (export rbtable-of fast-rbtable-adjoiner rbtable-builder rbtable-searcher
          rbtable-keys rbtable-entries rbtable-deleter rbtable-adjoiner
          rbtable-size rbtable=? rbtable-of= rbtable-updater)
  (import (afp primitives)
          (only (afp couplers) pipe)
          (only (afp adapters) converse ~initial ~each compare-by)
          (only (afp predicate-operations) ^not ^et ^vel)
          (only (afp pairs) pair=? pair-of pair-of=)
          (only (afp boxes) box? debox)
          (only (afp lists)
                first empty-list? list-of= fold-list ^and run
                all-different)
          (only (afp trees)
                empty-tree? make-empty-tree tree-of fold-tree tree-size
                tree->list)
          (only (afp bags) put-into-bag bag bag-union)
          (only (afp sets) fast-put-into-set set fast-union)
          (only (afp red-black-trees)
                color? red? red-black-search-tree? left-leaning?
                put-into-red-black-tree search-red-black-tree
                extract-from-red-black-tree))
  (begin

    ;; ===== rbtable-of ===================================================
    ;; (any -> Boolean), (any -> Boolean), (alpha, alpha -> Boolean) ->
    ;; key?              entry?            key<=?
    ;;                                               (any       -> Boolean)
    ;;                                                something

    ;; The rbtable-of procedure constructs a predicate that determines
    ;; whether something is a red-black table ordered by key<=?, using key?
    ;; to determine whether some value is a key, entry? to determine
    ;; whether some value is an entry, and the equivalence relation induced
    ;; by key=? as the criterion of sameness for keys.

    ;; Preconditions:
    ;;   key? can receive any value.
    ;;   entry? can receive any value.
    ;;   key<=? is an ordering relation.
    ;;   key<=? can receive any values that satisfy key?.

    (define (rbtable-of key? entry? key<=?)
      (^and (tree-of (pair-of color? (pair-of key? entry?)))
            (^not red?)
            (sect red-black-search-tree? (compare-by car key<=?) <>)
            (^vel empty-tree? left-leaning?)
            (run tree->list
                 (let ((cadr (pipe cdr car)))
                   (sect map cadr <>))
                 delist
                 (all-different (^et key<=? (converse key<=?))))))

    ;; ===== fast-rbtable-adjoiner ========================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key=?
    ;;          (alpha, beta, rbtable(alpha, beta) -> rbtable(alpha, beta))
    ;;           key    entry tab

    ;; The fast-rbtable-adjoiner procedure constructs a procedure that, in
    ;; turn, constructs a table similar to tab, except that key is
    ;; associated with entry, using key<=? as the ordering relation for
    ;; keys.

    ;; Preconditions:
    ;;   key<=? is an ordering relation.
    ;;   key<=? can receive key and any key in tab.
    ;;   key is not a key in tab.
    ;;   tab is ordered by key<=?.

    (define (fast-rbtable-adjoiner key<=?)
      (let ((putter (put-into-red-black-tree (pipe (~each car) key<=?))))
        (lambda (key entry tab)
          (putter (cons key entry) tab))))

    ;; ===== rbtable-builder ==============================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key<=?
    ;;                      (pair(alpha, beta) ... -> rbtable(alpha, beta))
    ;;                       associations

    ;; The rbtable procedure constructs a procedure that, in turn,
    ;; constructs a table, using key<=? as its ordering relation, in which
    ;; the car of each element of associations is associated (as a key)
    ;; with the cdr of that element of associations (as an entry).

    ;; Preconditions:
    ;;   key<=? is an ordering relation.
    ;;   key<=? can receive the cars of any elements of associations.
    ;;   No two elements of associations have the same car.

    (define (rbtable-builder key<=?)
      (pipe list
            (fold-list make-empty-tree
                       (put-into-red-black-tree (pipe (~each car) key<=?)))))

    ;; ===== rbtable-searcher =============================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key=?
    ;;            (alpha, rbtable(alpha, beta), beta ... -> beta | Boolean)
    ;;             key    tab                   extras

    ;; The rbtable-searcher procedure constructs a procedure that searches
    ;; in tab for an association with key as its key, using key<=? as the
    ;; ordering relation for keys.  If the constructed procedure finds such
    ;; an association, it returns the entry with which key is associated;
    ;; otherwise, it returns the initial element of extras, or #f if extras
    ;; is empty.

    ;; Precondition:
    ;;   key<=? is an ordering relation.
    ;;   key<=? can receive key and any key in tab.
    ;;   tab is ordered by key<=?.

    (define (rbtable-searcher key<=?)
      (let ((key-entry<=? (pipe (~each car) key<=?)))
        (let ((searcher (pipe (~initial (sect cons <> null))
                              (search-red-black-tree key-entry<=?))))
          (lambda (key tab . extras)
            (let ((search-result (searcher key tab)))
              (if (box? search-result)
                  (cdr (debox search-result))
                  (if (empty-list? extras) #f (first extras))))))))

    ;; ===== rbtable-keys =================================================
    ;; rbtable(alpha, any) -> set(alpha)
    ;; tab

    ;; The rbtable-keys procedure constructs a set comprising the keys for
    ;; which tab contains associations.

    (define rbtable-keys
      (fold-tree set
                 (lambda (root left-keys right-keys)
                   (fast-put-into-set (car (cdr root))
                                      (fast-union left-keys right-keys)))))

    ;; ===== rbtable-entries ==============================================
    ;; rbtable(any, alpha) -> bag(alpha)
    ;; tab

    ;; The rbtable-entries procedure constructs a bag comprising the
    ;; entries with which tab associates keys.

    (define rbtable-entries
      (fold-tree bag
                 (lambda (root left-entries right-entries)
                   (put-into-bag (cdr (cdr root))
                                 (bag-union left-entries right-entries)))))

    ;; ===== rbtable-deleter ==============================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key<=?
    ;;                (alpha, rbtable(alpha, beta) -> rbtable(alpha, beta))
    ;;                 key    tab

    ;; The rbtable-deleter procedure constructs a procedure that, in turn,
    ;; constructs a table similar to tab, with key<=? as its ordering
    ;; relation, but containing no association for key.  (If no such
    ;; association exists to begin with, the constructed procedure returns
    ;; tab.)

    ;; Precondition:
    ;;   key<=? is an ordering relation.
    ;;   key<=? can receive key and any key in tab.
    ;;   tab is ordered by key<=?.

    (define (rbtable-deleter key<=?)
      (let ((extracter (pipe (~initial (sect cons <> null))
                             (extract-from-red-black-tree
                               (pipe (~each car) key<=?)))))
        (lambda (key tab)
          (receive (result tab-without-key) (extracter key tab)
            (if (box? result) tab-without-key tab)))))

    ;; ===== rbtable-adjoiner =============================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key=?
    ;;          (alpha, beta, rbtable(alpha, beta) -> rbtable(alpha, beta))
    ;;           key    entry tab

    ;; The rbtable-adjoiner procedure constructs a procedure that, in turn,
    ;; constructs a table similar to tab, except that key is associated
    ;; with entry, using key<=? as the ordering relation for keys.  (If tab
    ;; already associates key with some value, the new association
    ;; displaces the old one.)

    ;; Preconditions:
    ;;   key<=? is an ordering relation.
    ;;   key<=? can receive key and any key in tab.
    ;;   tab is ordered by key<=?.

    (define (rbtable-adjoiner key<=?)
      (let ((adjoiner (fast-rbtable-adjoiner key<=?))
            (deleter (rbtable-deleter key<=?)))
        (lambda (key entry tab)
          (adjoiner key entry (deleter key tab)))))

    ;; ===== rbtable-size =================================================
    ;; rbtable(any, any) -> natural-number
    ;; tab

    ;; The rbtable-size procedure computes the number of associations in
    ;; tab.

    (define rbtable-size tree-size)

    ;; ===== rbtable=? ====================================================
    ;; rbtable(any, any), rbtable(any, any) -> Boolean
    ;; left               right

    ;; The rbtable=? procedure determines whether left and right are the
    ;; same table -- that is, whether they contain the same keys,
    ;; associated with the same entries.

    (define rbtable=? (compare-by (pipe tree->list (sect map cdr <>))
                                  (list-of= pair=?)))

    ;; ===== rbtable-of= ==================================================
    ;; (alpha, beta -> Boolean), (gamma, delta -> Boolean) ->
    ;; same-key?                 same-entry?
    ;;             (rbtable(alpha, gamma), rbtable(beta, delta) -> Boolean)
    ;;              left                   right

    ;; The rbtable-of= procedure constructs a predicate that determines
    ;; whether left and right are the same table, using same-key? as the
    ;; criterion of sameness for keys and same-entry? as the criterion of
    ;; sameness for entries.

    ;; Preconditions:
    ;;   same-key? can receive any key in left and any key in right.
    ;;   same-entry? can receive any entry in left and any entry in right.

    (define (rbtable-of= same-key? same-entry?)
      (compare-by (pipe tree->list (sect map cdr <>))
                  (list-of= (pair-of= same-key? same-entry?))))

    ;; ===== rbtable-updater ==============================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key<=?
    ;;     (alpha, (beta -> beta), rbtable(alpha, beta), beta ... ->
    ;;      key    updater         tab                   extras
    ;;                                                rbtable(alpha, beta))

    ;; The rbtable-updater procedure constructs a procedure that looks up
    ;; key in tab (using key<=? as the ordering relation on keys), applies
    ;; updater to the entry with which tab associates key, associates key
    ;; with the result returned by updater, and and constructs a table
    ;; similar to tab, except that the new association replaces the
    ;; previous assocation for key.  If tab does not associate key with any
    ;; value, the constructed procedure instead applies updater to the
    ;; initial element of extras, or to #f if extras is empty.

    ;; Preconditions:
    ;;   key<=? is an ordering relation.
    ;;   key<=? can receive any keys in tab.
    ;;   updater can receive any entry in tab.
    ;;   tab is ordered by key<=?.
    ;;   If tab contains no association for key and extras is empty, then
    ;;     updater can receive #f.
    ;;   If tab contains no association for key and extras is not empty,
    ;;     then updater can receive the first element of extras.

    (define (rbtable-updater key<=?)
      (let ((adjoiner (fast-rbtable-adjoiner key<=?))
            (extracter (pipe (~initial (sect cons <> null))
                             (extract-from-red-black-tree
                               (pipe (~each car) key<=?)))))
        (lambda (key updater tab . extras)
          (receive (result tab-without-key) (extracter key tab)
            (adjoiner key
                      (updater (if (box? result)
                                   (cdr (debox result))
                                   (if (empty-list? extras)
                                       #f
                                       (first extras))))
                      tab-without-key)))))))

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
