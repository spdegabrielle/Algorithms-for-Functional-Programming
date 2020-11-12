;;; Tables

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 6, 1999
;;; last revised January 6, 2017

(define-library (afp tables)
  (export table? table-of fast-put-into-table put-into-table
          table-adjoiner table lookup table-searcher table-keys
          table-entries delete-from-table table-deleter table-size table=?
          table-of= table-update table-updater)
  (import (afp primitives)
          (only (afp procedure-sections) equal-to)
          (only (afp couplers) pipe)
          (only (afp adapters) ~initial >next)
          (only (afp predicate-operations) ^et)
          (only (afp pairs) pair=? pair-of pair-of=)
          (only (afp lists) first empty-list? run all-different)
          (only (afp bags)
                bag bag-of debag put-into-bag take-from-bag empty-bag?
                bag-of= extract-from-bag bag-cardinality fold-bag map-bag)
          (only (afp sets) set fast-put-into-set))
  (begin
    
    ;; ===== table? =======================================================
    ;; any       -> Boolean
    ;; something
    ;; The table? procedure determines whether something is a table.

    (define table?
      (^et (bag-of pair?)
           (run (sect map-bag car <>) debag (all-different equal?))))

    ;; ===== table-of =====================================================
    ;; (any -> Boolean), (any -> Boolean), (alpha, alpha -> Boolean) ->
    ;; key?              entry?            key=?
    ;;                                               (any       -> Boolean)
    ;;                                                something

    ;; The table-of procedure constructs a predicate that determines
    ;; whether something is a table, using key? to determine whether some
    ;; value is a key, entry? to determine whether some value is an entry,
    ;; and key=? to determine whether two values are the same key.

    ;; Preconditions:
    ;;   key? can receive any value.
    ;;   entry? can receive any value.
    ;;   key=? is an equivalence relation.
    ;;   key=? can receive any values that satisfy key?.

    (define (table-of key? entry? key=?)
      (^et (bag-of (pair-of key? entry?))
           (run (sect map-bag car <>) debag (all-different key=?))))

    ;; ===== fast-put-into-table ==========================================
    ;; alpha, beta, table(alpha, beta) -> table(alpha, beta)
    ;; key    entry tab

    ;; The fast-put-into-table procedure constructs a table similar to tab,
    ;; except that key is associated with entry.

    ;; Precondition:
    ;;   key is not associated with any entry in tab.

    (define (fast-put-into-table key entry tab)
      (put-into-bag (cons key entry) tab))

    ;; ===== put-into-table ===============================================
    ;; alpha, beta, table(alpha, beta) -> table(alpha, beta)
    ;; key    entry tab

    ;; The put-into-table procedure constructs a table similar to tab,
    ;; except that key is associated with entry.  (If tab already
    ;; associates key with some value, the new association displaces the
    ;; old one.)

    (define (put-into-table key entry tab)
      (receive (discarded tab-without-key)
               (extract-from-bag (pipe car (equal-to key)) tab)
        (put-into-bag (cons key entry) tab-without-key)))

    ;; ===== table-adjoiner ===============================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key=?
    ;;              (alpha, beta, table(alpha, beta) -> table(alpha, beta))
    ;;               key    entry tab

    ;; The table-adjoiner procedure constructs a procedure that, in turn,
    ;; constructs a table similar to tab, except that key is associated
    ;; with entry, using key=? as its criterion of sameness of keys.  (If
    ;; tab already associates key with some value, the new association
    ;; displaces the old one.)

    ;; Preconditions:
    ;;   key=? is an equivalence relation.
    ;;   key=? can receive key and any key in tab.

    (define (table-adjoiner key=?)
      (lambda (key entry tab)
        (receive (discarded tab-without-key)
                 (extract-from-bag (pipe car (sect key=? <> key)) tab)
          (put-into-bag (cons key entry) tab-without-key))))

    ;; ===== table ========================================================
    ;; pair(alpha, beta) ... -> table(alpha, beta)
    ;; associations

    ;; The table procedure constructs a table in which the car of each
    ;; element of associations is associated (as a key) with the cdr of
    ;; that element of associations (as an entry).

    ;; Precondition:
    ;;   No two elements of associations have the same car.

    (define table bag)

    ;; ===== lookup ======================================================
    ;; alpha, table(alpha, beta), beta ... -> beta | Boolean
    ;; key    tab                 extras

    ;; The lookup procedure searches in tab for an association with key as
    ;; its key.  If it finds one, it returns the entry with which key is
    ;; associated; otherwise, it returns the initial element of extras, or
    ;; #f if extras is empty.

    (define (lookup key tab . extras)
      (let ((default (if (empty-list? extras) #f (first extras))))
        ((rec (searcher aro)
           (if (empty-bag? aro)
               default
               (receive (chosen others) (take-from-bag aro)
                 (if (equal? key (car chosen))
                     (cdr chosen)
                     (searcher others)))))
         tab)))

    ;; ===== table-searcher ===============================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key=?
    ;;              (alpha, table(alpha, beta), beta ... -> beta | Boolean)
    ;;               key    tab                 extras

    ;; The table-searcher procedure constructs a procedure that searches in
    ;; tab for an association with key as its key, using key=? as its
    ;; criterion of sameness of keys.  If the constructed procedure finds
    ;; such an association, it returns the entry with which key is
    ;; associated; otherwise, it returns the initial element of extras, or
    ;; #f if extras is empty.

    ;; Precondition:
    ;;   key=? is an equivalence relation.
    ;;   key=? can receive key and any key in tab.

    (define (table-searcher key=?)
      (lambda (key tab . extras)
        (let ((default (if (empty-list? extras) #f (first extras))))
          ((rec (searcher aro)
             (if (empty-bag? aro)
                 default
                 (receive (chosen others) (take-from-bag aro)
                   (if (key=? key (car chosen))
                       (cdr chosen)
                       (searcher others)))))
           tab))))

    ;; ===== table-keys ===================================================
    ;; table(alpha, any) -> set(alpha)
    ;; tab

    ;; The table-keys procedure returns a set comprising the keys for which
    ;; tab contains associations.

    (define table-keys
      (fold-bag set (pipe (~initial car) fast-put-into-set)))

    ;; ===== table-entries ================================================
    ;; table(any, alpha) -> bag(alpha)
    ;; tab

    ;; The table-entries procedure returns a bag comprising the entries
    ;; with which tab associates keys.

    (define table-entries (sect map-bag cdr <>))

    ;; ===== delete-from-table ============================================
    ;; alpha, table(alpha, beta) -> table(alpha, beta)
    ;; key    tab

    ;; The delete-from-table procedure constructs a table similar to tab,
    ;; but containing no association for key.  (If tab contains no
    ;; association for key to begin with, delete-from-table returns tab.)

    (define (delete-from-table key tab)
      ((pipe extract-from-bag >next) (pipe car (equal-to key)) tab))

    ;; ===== table-deleter ================================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key=?
    ;;                    (alpha, table(alpha, beta) -> table(alpha, beta))
    ;;                     key    tab

    ;; The table-deleter procedure constructs a procedure that, in turn,
    ;; constructs a table similar to tab, but containing no association for
    ;; any value that, with key, satisfies key=?.  (If no such association
    ;; exists, the constructed procedure returns tab.)

    ;; Precondition:
    ;;   key=? is an equivalence relation.
    ;;   key=? can receive key and any key in tab.

    (define (table-deleter key=?)
      (lambda (key tab)
        ((pipe extract-from-bag >next) (pipe car (sect key=? <> key)) tab)))

    ;; ===== table-size ===================================================
    ;; table(any, any) -> natural-number
    ;; tab

    ;; The table-size procedure computes the number of associations in tab.

    (define table-size bag-cardinality)

    ;; ===== table=? ======================================================
    ;; table(any, any), table(any, any) -> Boolean
    ;; left             right

    ;; The table=? procedure determines whether left and right are the same
    ;; table -- that is, whether they contain the same keys, associated with
    ;; the same entries.

    (define table=? (bag-of= pair=?))

    ;; ===== table-of= ====================================================
    ;; (alpha, alpha -> Boolean), (beta, beta -> Boolean) ->
    ;; same-key?                  same-entry?
    ;;                  (table(alpha, beta), table(alpha, beta) -> Boolean)
    ;;                   left                right

    ;; The table-of= procedure constructs a predicate that determines
    ;; whether left and right are the same table, using same-key? as the
    ;; criterion of sameness for keys and same-entry? as the criterion of
    ;; sameness for entries.

    ;; Preconditions:
    ;;   same-key? can receive any key in left and any key in right.
    ;;   same-entry? can receive any entry in left and any entry in right.

    (define table-of= (pipe pair-of= bag-of=))

    ;; ===== table-update =================================================
    ;; alpha, (beta -> beta), table(alpha, beta), beta ... ->
    ;; key    updater         tab                 extras
    ;;                                                   table(alpha, beta)

    ;; The table-update procedure looks up key in tab, applies updater to
    ;; the entry with which tab associates key, associates key with the
    ;; result returned by updater, and constructs a table similar to tab,
    ;; except that the new association displaces the previous association
    ;; for key.  If tab does not associate key with any value, updater is
    ;; applied instead to the initial element of extras, or to #f if extras
    ;; is empty.

    ;; Preconditions:
    ;;   updater can receive any entry in tab.
    ;;   If tab contains no association for key and extras is empty, then
    ;;     updater can receive #f.
    ;;   If tab contains no association for key and extras is not empty, then
    ;;     updater can receive the initial element of extras.

    (define (table-update key updater tab . extras)
      (put-into-table key (updater (apply lookup key tab extras)) tab))

    ;; ===== table-updater ================================================
    ;; (alpha, alpha -> Boolean) ->
    ;; key=?
    ;;     (alpha, (beta -> beta), table(alpha, beta), beta ... ->
    ;;      key    updater         tab                 extras
    ;;                                                  table(alpha, beta))

    ;; The table-updater procedure constructs a procedure that looks up key
    ;; in tab (using key=? as its criterion of sameness), applies updater
    ;; to the entry with which tab associates key, associates key with the
    ;; result returned by updater, and and constructs a table similar to
    ;; tab, except that the new association replaces the previous
    ;; association for key.  If tab does not associate key with any value,
    ;; updater is applied instead to the initial element of extras, or to
    ;; #f if extras is empty.

    ;; Preconditions:
    ;;   key=? is an equivalence relation.
    ;;   key=? can receive any keys in tab.
    ;;   updater can receive any entry in tab.
    ;;   If tab contains no association for key and extras is empty, then
    ;;     updater can receive #f.
    ;;   If tab contains no association for key and extras is not empty,
    ;;     then updater can receive the first element of extras.

    (define (table-updater key=?)
      (let ((adjoiner (table-adjoiner key=?))
            (searcher (table-searcher key=?)))
        (lambda (key updater tab . extras)
          (adjoiner key (updater (apply searcher key tab extras)) tab))))))

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
