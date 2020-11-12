;;; Bags

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created March 21, 1999
;;; last revised January 9, 2017

(define-library (afp bags)
  (export bag? bag-of bag debag put-into-bag take-from-bag empty-bag?
          bag=? bag-of= extract-from-bag bag-cardinality for-all-in-bag?
          exists-in-bag? fold-bag bag-union map-bag filter-bag remp-bag
          partition-bag unfold-bag list->bag partition-bag-with-count
          cons-with-each cons-with-each-source prepend-to-each
          prepend-each-to-each put-into-each-bag remove-bag-from-bag)
  (import (afp primitives)
          (only (afp arithmetic) add1)
          (only (afp constant-procedures) constant create black-hole)
          (only (afp procedure-sections) curry equal-to)
          (only (afp couplers) pipe)
          (only (afp adapters) >next ~initial)
          (only (afp recursion-managers) recur build)
          (only (afp predicate-operations) ^not conditionally-combine)
          (only (afp natural-numbers) tally)
          (only (afp boxes) box box?)
          (only (afp lists)
                empty-list? prepend deprepend list-of fold-list run
                extract)
          (only (afp sources) finite-source))
  (begin

    ;; ===== bag? =========================================================
    ;; any       -> Boolean
    ;; something

    ;; The bag? predicate determines whether something is a bag.

    (define bag? list?)

    ;; ===== bag-of =======================================================
    ;; (any -> Boolean)     -> (any       -> Boolean)
    ;; right-type-of-value?     something

    ;; The bag-of procedure constructs a predicate that determines whether
    ;; something is a bag in which every value satisfies
    ;; right-type-of-value?.

    ;; Precondition:
    ;;   right-type-of-value? can receive any value.

    (define bag-of list-of)

    ;; ===== bag ==========================================================
    ;; alpha ... -> bag(alpha)
    ;; arguments

    ;; The bag procedure constructs a bag containing the elements of
    ;; arguments.

    (define bag list)

    ;; ===== debag ========================================================
    ;; bag(alpha) -> alpha ...
    ;; aro

    ;; The debag procedure returns the values that aro contains (in any
    ;; order).

    (define debag delist)

    ;; ===== put-into-bag =================================================
    ;; alpha, bag(alpha) -> bag(alpha)
    ;; new    aro

    ;; The put-into-bag procedure constructs a bag containing new, in
    ;; addition to all of the values in aro.

    (define put-into-bag prepend)

    ;; ===== take-from-bag ================================================
    ;; bag(alpha) -> alpha, bag(alpha)
    ;; aro

    ;; The take-from-bag procedure returns one of the values in aro and a
    ;; bag containing all of the other values in aro.

    ;; Precondition:
    ;;   aro is not empty.

    (define take-from-bag deprepend)

    ;; ===== empty-bag? ===================================================
    ;; bag(any) -> Boolean
    ;; aro

    ;; The empty-bag? predicate determines whether aro is empty.

    (define empty-bag? empty-list?)

    ;; ===== bag=? ========================================================
    ;; bag(any), bag(any) -> Boolean
    ;; left      right

    ;; The bag=? predicate determines whether left and right are the same
    ;; bag, that is, whether the same values are in both (regardless of
    ;; order, but counting duplicates as distinct within each bag).

    (define (bag=? left right)
      (if (empty-list? left)
          (empty-list? right)
          (receive (first-of-left rest-of-left) (deprepend left)
            (receive (sought rest-of-right)
                     (extract (equal-to first-of-left) right)
              (and (box? sought)
                   (bag=? rest-of-left rest-of-right))))))

    ;; ===== bag-of= ======================================================
    ;; (alpha, beta -> Boolean) -> (bag(alpha), bag(beta) -> Boolean)
    ;; element=?                    left        right

    ;; The bag-of= procedure constructs a procedure that determines
    ;; whether, for each value in left, there is a corresponding value in
    ;; right such that those values satisfy element=?.

    ;; Preconditions:
    ;;   element=? can receive any value in left as its first argument and
    ;;     any value in right as its second.
    ;;   For any values left-0, left-1, right-0, and right-1, if left-0 and
    ;;     right-0 satisfy element=?, and left-1 and right-0 satisfy
    ;;     element=?, and left-0 and right-1 satisfy element=?, then left-1
    ;;     and right-1 satisfy element=?.

    (define (bag-of= element=?)
      (rec (equivalent? left right)
        (if (empty-list? left)
            (empty-list? right)
            (receive (first-of-left rest-of-left) (deprepend left)
              (receive (sought rest-of-right)
                       (extract (sect element=? first-of-left <>) right)
                (and (box? sought)
                     (equivalent? rest-of-left rest-of-right)))))))

    ;; ===== extract-from-bag =============================================
    ;; (alpha -> Boolean), bag(alpha) -> (box(alpha) | Boolean), bag(alpha)
    ;; condition-met?      aro

    ;; The extract-from-bag procedure searches aro for a value that
    ;; satisfies condition-met?.  If it finds one, it returns a box
    ;; containing that value and a bag containing the other values in aro;
    ;; otherwise, it returns #f and aro.

    ;; Precondition:
    ;;   condition-met? can receive any element of aro.

    (define (extract-from-bag condition-met? aro)
      ((rec (extracter areto)
         (if (empty-bag? areto)
             (values #f areto)
             (receive (chosen others) (take-from-bag areto)
               (if (condition-met? chosen)
                   (values (box chosen) others)
                   (receive (sought relicts) (extracter others)
                     (values sought (put-into-bag chosen relicts)))))))
       aro))

    ;; ===== bag-cardinality ==============================================
    ;; bag(any) -> natural-number
    ;; aro

    ;; The bag-cardinality procedure computes the number of values aro
    ;; contains.

    (define bag-cardinality (tally empty-bag? (pipe take-from-bag >next)))

    ;; ===== for-all-in-bag? ==============================================
    ;; (alpha -> Boolean), bag(alpha) -> Boolean
    ;; condition-met?      aro

    ;; The for-all-in-bag? procedure determines whether all of the values
    ;; in aro satisfy condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any value in aro.

    (define (for-all-in-bag? condition-met? aro)
      ((rec (ok? areto)
         (or (empty-bag? areto)
             (receive (chosen others) (take-from-bag areto)
               (and (condition-met? chosen) (ok? others)))))
       aro))

    ;; ===== exists-in-bag? ===============================================
    ;; (alpha -> Boolean), bag(alpha) -> Boolean
    ;; condition-met?      aro

    ;; The exists-in-bag? procedure determines whether at least one of the
    ;; values in aro satisfies condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any value in aro.

    (define exists-in-bag? (run (~initial ^not) for-all-in-bag? not))

    ;; ===== fold-bag =====================================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            combiner
    ;;                                             (bag(beta) -> alpha ...)
    ;;                                              aro

    ;; The fold-bag procedure constructs a procedure that returns the
    ;; results of invoking base if aro is empty.  If aro is not empty, the
    ;; constructed procedure takes a value out of aro, applies itself
    ;; recursively to a bag containing the other values in aro, and returns
    ;; the results of applying combiner to the taken value and the results
    ;; of the recursive invocation.

    ;; Preconditions:
    ;;   combiner can receive any value in aro and the results of an
    ;;     invocation of base.
    ;;   combiner can receive any value in aro and the results of an
    ;;     invocation of combiner.
    ;;   combiner satisfies the order-independence condition:  For any
    ;;     values foo and bar and any list starters of values,
    ;;
    ;;       (receive results (apply combiner foo starters)
    ;;         (apply combiner bar results))
    ;;
    ;;     and
    ;;
    ;;       (receive results (apply combiner bar starters)
    ;;         (apply combiner foo results))
    ;;
    ;;     yield equivalent results.

    (define (fold-bag base combiner)
      (recur empty-bag? (pipe black-hole base) take-from-bag combiner))

    ;; ===== bag-union ====================================================
    ;; bag(alpha), bag(alpha) -> bag(alpha)
    ;; left        right

    ;; The bag-union procedure constructs a bag containing all of the
    ;; values that left and right contain.

    (define (bag-union left right)
      ((fold-bag (create right) put-into-bag) left))

    ;; ===== map-bag ======================================================
    ;; (alpha -> beta), bag(alpha) -> bag(beta)
    ;; procedure        aro

    ;; The map-bag procedure constructs a bag containing each of the
    ;; results of applying procedure to a value in aro.

    ;; Precondition:
    ;;   procedure can receive any value in aro.

    (define (map-bag procedure aro)
      ((fold-bag (create (bag)) (pipe (~initial procedure) put-into-bag))
       aro))

    ;; ===== filter-bag ===================================================
    ;; (alpha -> Boolean), bag(alpha) -> bag(alpha)
    ;; keep?               aro

    ;; The filter-bag procedure constructs a bag containing the values in
    ;; aro that satisfy keep?.

    ;; Precondition:
    ;;   keep? can receive any value in aro.

    (define (filter-bag keep? aro)
      ((fold-bag bag (conditionally-combine keep? put-into-bag)) aro))

    ;; ===== remp-bag =====================================================
    ;; (alpha -> Boolean), bag(alpha) -> bag(alpha)
    ;; exclude?            aro

    ;; The remp-bag procedure constructs a bag containing the values in aro
    ;; that do not satisfy exclude?.

    ;; Precondition:
    ;;   exclude? can receive any value in aro.

    (define remp-bag (pipe (~initial ^not) filter-bag))

    ;; ===== partition-bag ================================================
    ;; (alpha -> Boolean), bag(alpha) -> bag(alpha), bag(alpha)
    ;; condition-met?      aro

    ;; The partition-bag procedure constructs two bags, one containing the
    ;; values in aro that satisfy condition-met?, the other those that do
    ;; not.

    ;; Precondition:
    ;;   condition-met? can receive any value in aro.

    (define (partition-bag condition-met? aro)
      ((fold-bag (create (bag) (bag))
                 (lambda (candidate ins outs)
                   (if (condition-met? candidate)
                       (values (put-into-bag candidate ins) outs)
                       (values ins (put-into-bag candidate outs)))))
       aro))

    ;; ===== unfold-bag ===================================================
    ;; (alpha ... -> Boolean), (alpha ... -> beta),
    ;; final?                  producer
    ;;                 (alpha ... -> alpha ...) -> (alpha ... -> bag(beta))
    ;;                 step                         arguments

    ;; The unfold-bag procedure constructs a procedure that first
    ;; determines whether the elements of arguments satisfy final?.  If so,
    ;; the constructed procedure returns the empty bag.  Otherwise, it
    ;; returns a bag containing the result of applying producer to the
    ;; elements of arguments, in addition to the values in the bag obtained
    ;; by first applying step to the elements of arguments and then
    ;; applying the constructed procedure recursively to the results.

    ;; Preconditions:
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

    (define (unfold-bag final? producer step)
      (build final? (constant (bag)) producer step put-into-bag))

    ;; ===== list->bag ====================================================
    ;; list(alpha) -> bag(alpha)

    ;; The list->bag procedure constructs a bag containing the elements of
    ;; ls.

    (define list->bag (fold-list bag put-into-bag))

    ;; ===== partition-bag-with-count =====================================
    ;; (alpha -> Boolean), bag(alpha) ->
    ;; condition-met?      aro
    ;;                               natural-number, bag(alpha), bag(alpha)

    ;; The partition-bag-with-count procedure returns the number of values
    ;; in aro that satisfy condition-met?, a bag containing those values,
    ;; and a bag containing the values in aro that do not satisfy
    ;; condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any values in aro.

    (define (partition-bag-with-count condition-met? aro)
      ((fold-bag (create 0 (bag) (bag))
                 (lambda (item count ins outs)
                   (if (condition-met? item)
                       (values (add1 count) (put-into-bag item ins) outs)
                       (values count ins (put-into-bag item outs)))))
       aro))

    ;; ===== cons-with-each ===============================================
    ;; alpha,    bag(beta)   -> bag(pair(alpha, beta))
    ;; car-value bag-of-cdrs

    ;; The cons-with-each procedure constructs a bag containing every pair
    ;; that has car-value as its car and a value from bag-of-cdrs as its
    ;; cdr.

    (define (cons-with-each car-value bag-of-cdrs)
      (map-bag (sect cons car-value <>) bag-of-cdrs))

    ;; ===== cons-with-each-source ========================================
    ;; alpha,    bag(beta)   -> source(pair(alpha, beta))
    ;; car-value bag-of-cdrs

    ;; The cons-with-each-source procedure constructs a finite source
    ;; containing every pair that has car-value as its car and a value from
    ;; bag-of-cdrs as its cdr.

    (define (cons-with-each-source car-value bag-of-cdrs)
      ((rec (mapper subbag)
         (if (empty-bag? subbag)
             (finite-source)
             (source (receive (cdr-value others) (take-from-bag subbag)
                       (values (cons car-value cdr-value)
                               (mapper others))))))
       bag-of-cdrs))

    ;; ===== prepend-to-each ==============================================
    ;; alpha,      bag(list(alpha)) -> bag(list(alpha))
    ;; new-initial aro

    ;; The prepend-to-each procedure constructs a bag of lists containing
    ;; every list formed by prepending new-initial to a list in aro.

    (define (prepend-to-each new-initial aro)
      (map-bag (sect prepend new-initial <>) aro))

    ;; ===== prepend-each-to-each =========================================
    ;; bag(alpha), bag(list(alpha)) -> bag(list(alpha))
    ;; initials    aro

    ;; The prepend-each-to-each procedure constructs a bag containing every
    ;; list formed by prepending a value in initials to a list in aro.

    (define (prepend-each-to-each initials aro)
      ((fold-bag bag (pipe (~initial (sect prepend-to-each <> aro))
                           bag-union))
       initials))

    ;; ===== put-into-each-bag ============================================
    ;; alpha, bag(bag(alpha)) -> bag(bag(alpha))
    ;; new    araro

    ;; The put-into-each-bag procedure constructs a bag of bags, similar to
    ;; araro except that new has been added to each of the bags in it.

    (define (put-into-each-bag new araro)
      (map-bag (sect put-into-bag new <>) araro))

    ;; ===== remove-bag-from-bag ==========================================
    ;; bag(alpha), bag(bag(alpha)) -> bag(bag(alpha))
    ;; aro         araro

    ;; The remove-bag-from-bag procedure constructs a bag of bags, similar
    ;; to araro except that aro (if present in araro) has been removed.

    (define remove-bag-from-bag
      (run (~initial (curry bag=?)) extract-from-bag >next))))

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
