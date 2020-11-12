;;; Sets

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created March 6, 1999
;;; last revised January 9, 2017

(define-library (afp sets)
  (export set-classification set? set-of set-adjoiner put-into-set
          fast-put-into-set set-maker set deset take-from-set empty-set?
          set=? set-of= fold-set set-mapper set-unfolder map-set
          unfold-set fast-map-set filter-set remp-set partition-set
          cardinality extract-from-set remove-from-set for-all-in-set?
          exists-in-set? member? set-membership subset? set-subsethood
          set-unioner union fast-union intersection set-intersectioner
          disjoint? set-disjointness set-difference set-differencer
          set->bag)
  (import (afp primitives)
          (only (afp arithmetic) add1)
          (only (afp constant-procedures) constant create black-hole)
          (only (afp procedure-sections) curry)
          (only (afp couplers) pipe)
          (only (afp adapters) >next ~initial)
          (only (afp recursion-managers) recur build)
          (only (afp predicate-operations)
                ^not ^et ^if conditionally-combine)
          (only (afp boxes) box)
          (only (afp lists) extend-to-variable-arity run every
                all-different)
          (only (afp bags)
                bag bag? debag empty-bag? put-into-bag take-from-bag bag=?
                bag-of= exists-in-bag?))
  (begin

    ;; ===== set-classification ===========================================
    ;; (alpha, alpha -> Boolean) -> (any       -> Boolean)
    ;; equivalent?                   something

    ;; The set-classification procedure constructs a predicate that
    ;; determines whether something is a set, using equivalent? as its
    ;; criterion of sameness among the members of the putative set.

    ;; Preconditions:
    ;;   equivalent? is symmetric.
    ;;   equivalent? can receive any values.

    (define (set-classification equivalent?)
      (^et bag? (pipe debag (all-different equivalent?))))

    ;; ===== set? =========================================================
    ;; any       -> Boolean
    ;; something

    ;; The set? procedure determines whether something is a set, using
    ;; equal? as the criterion of sameness among the members of the
    ;; putative set.

    (define set? (set-classification equal?))

    ;; ===== set-of =======================================================
    ;; (any -> Boolean),     (alpha, alpha -> Boolean) ->
    ;; right-type-of-member? equivalent?
    ;;                                               (any       -> Boolean)
    ;;                                                something

    ;; The set-of procedure constructs a predicate that determines whether
    ;; something is a set of which all the members satisfy
    ;; right-type-of-member?, using equivalent? as the criterion of
    ;; sameness among the members of the putative set.

    ;; Preconditions:
    ;;   right-type-of-member? can receive any value.
    ;;   equivalent? can receive any values that satisfy
    ;;     right-type-of-member?.
    ;;   equivalent? is symmetric.

    (define (set-of right-type-of-member? equivalent?)
      (^et bag? (pipe debag (^et (every right-type-of-member?)
                                 (all-different equivalent?)))))

    ;; ===== set-adjoiner =================================================
    ;; (alpha, alpha -> Boolean) -> (alpha, set(alpha) -> set(alpha))
    ;; equivalent?                   new    aro

    ;; The set-adjoiner procedure constructs a procedure that, in turn,
    ;; constructs a set containing the members of aro and, in addition,
    ;; new, provided that no member of aro and new satisfy equivalent?.

    ;; Precondition:
    ;;   equivalent? is an equivalence relation.
    ;;   equivalent? can receive new and any member of aro.

    (define (set-adjoiner equivalent?)
      (^if (pipe (~initial (curry equivalent?)) exists-in-bag?)
           >next
           put-into-bag))

    ;; ===== put-into-set =================================================
    ;; alpha, set(alpha) -> set(alpha)
    ;; new    aro

    ;; The put-into-set procedure constructs a set containing the members
    ;; of aro and, in addition, new, provided that new is not a member of
    ;; aro.

    (define put-into-set (set-adjoiner equal?))

    ;; ===== fast-put-into-set ============================================
    ;; alpha, set(alpha) -> set(alpha)
    ;; new    aro

    ;; The fast-put-into-set procedure constructs a set containing new, in
    ;; addition to all the members of aro.

    ;; Precondition:
    ;;   new is not a member of aro.

    (define fast-put-into-set put-into-bag)

    ;; ===== set-maker ====================================================
    ;; (alpha, alpha -> Boolean) -> (alpha ... -> set(alpha))
    ;; equivalent?                   arguments

    ;; The set-maker procedure constructs a procedure that, in turn,
    ;; constructs a set with the elements of arguments as its members,
    ;; excluding duplicates (as determined by equivalent?).

    ;; Preconditions:
    ;;   equivalent? is an equivalence relation.
    ;;   equivalent? can receive any elements of arguments.

    (define (set-maker equivalent?)
      (extend-to-variable-arity (bag) (set-adjoiner equivalent?)))

    ;; ===== set ==========================================================
    ;; alpha ... -> set(alpha)
    ;; arguments 

    ;; The set procedure constructs a set with the elements of arguments as
    ;; its members, excluding duplicates.

    (define set (set-maker equal?))

    ;; ===== deset ==========================================================
    ;; set(alpha) -> alpha ...
    ;; aro

    ;; The deset procedure returns the members of aro.

    (define deset debag)

    ;; ===== take-from-set ================================================
    ;; set(alpha) -> alpha, set(alpha)
    ;; aro

    ;; The take-from-set procedure returns a member of aro and a set with
    ;; the other members of aro as its members.

    ;; Precondition:
    ;;   aro is not empty.

    (define take-from-set take-from-bag)

    ;; ===== empty-set? ===================================================
    ;; set(any) -> Boolean
    ;; aro

    ;; The empty-set? predicate determines whether aro is empty.

    (define empty-set? empty-bag?)

    ;; ===== set=? ========================================================
    ;; set(any), set(any) -> Boolean
    ;; left      right

    ;; The set=? predicate determines whether left and right have the same
    ;; members.

    (define set=? bag=?)

    ;; ===== set-of= ======================================================
    ;; (alpha, beta -> Boolean) -> (set(alpha), set(beta) -> Boolean)
    ;; member=?                     left        right

    ;; The set-of= procedure constructs a predicate that determines whether
    ;; left and right have the same members, using member=? as its
    ;; criterion of sameness.

    ;; Preconditions:
    ;;   member=? can receive any member of left as its first argument and
    ;;     any member of right as its second.
    ;;   For any values left-0, left-1, right-0, and right-1, if left-0 and
    ;;     right-0 satisfy member=?, and left-1 and right-0 satisfy member=?,
    ;;     and left-0 and right-1 satisfy member=?, then left-1 and right-1
    ;;     satisfy member=?.

    (define set-of= bag-of=)

    ;; ===== fold-set =====================================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            combiner
    ;;                                             (set(beta) -> alpha ...)
    ;;                                              aro

    ;; The fold-set procedure constructs a procedure that returns the
    ;; results of invoking base if aro is empty.  If aro is not empty, the
    ;; constructed procedure takes a member out of aro, applies itself
    ;; recursively to the set of the other members of aro, and returns the
    ;; results of applying combiner to the taken value and the results of
    ;; the recursive invocation.

    ;; Preconditions:
    ;;   combiner can receive any member of aro and the results of an
    ;;     invocation of base.
    ;;   combiner can receive any member of aro and the results of any
    ;;     invocation of combiner.

    (define (fold-set base combiner)
      (recur empty-set? (pipe black-hole base) take-from-set combiner))

    ;; ===== set-mapper ===================================================
    ;; (alpha, alpha -> Boolean) ->
    ;; equivalent?
    ;;                           ((beta -> alpha), set(beta) -> set(alpha))
    ;;                            procedure        aro

    ;; The set-mapper procedure constructs a procedure that, in turn,
    ;; constructs a set, the members of which are the results of applying
    ;; procedure to members of aro.  The criterion of sameness for members
    ;; of the constructed set is equivalent?.

    ;; Preconditions:
    ;;   equivalent? can receive any results of invocations of procedure.
    ;;   equivalent? is an equivalence relation.
    ;;   procedure can receive any member of aro.

    (define (set-mapper equivalent?)
      (lambda (procedure aro)
        ((fold-set (set-maker equivalent?)
                   (pipe (~initial procedure) (set-adjoiner equivalent?)))
         aro)))

    ;; ===== set-unfolder =================================================
    ;; (alpha, alpha -> Boolean) ->
    ;; equivalent?
    ;;     ((beta ... -> Boolean), (beta ... -> alpha),
    ;;      final?                 producer
    ;;                 (beta ... -> beta ...) -> (beta ...  -> set(alpha)))
    ;;                 step                       arguments

    ;; The set-unfolder procedure constructs a procedure, in turn,
    ;; constructs a procedure that first determines whether the elements of
    ;; arguments satisfy final?.  If so, the inner constructed procedure
    ;; returns the empty set.  Otherwise, it returns a set containing the
    ;; result of applying producer to the elements of arguments, in
    ;; addition to the members of the set obtained by first applying step
    ;; to the elements of arguments and then applying the constructed
    ;; procedure recursively to the results, excluding duplicates.  The
    ;; criterion of sameness for members of the constructed set is
    ;; equivalent?.

    ;; Preconditions:
    ;;   equivalent? can receive the results of any invocation of producer.
    ;;   equivalent? is an equivalence relation.
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

    (define (set-unfolder equivalent?)
      (lambda (final? producer step)
        (build final?
               (constant ((set-maker equivalent?)))
               producer
               step
               (set-adjoiner equivalent?))))

    ;; ===== map-set ======================================================
    ;; (alpha -> beta), set(alpha) -> set(beta)
    ;; procedure        aro

    ;; The map-set procedure constructs a set containing each of the
    ;; results of applying procedure to a value in aro, excluding
    ;; duplicates.

    ;; Precondition:
    ;;   procedure can receive any member of aro.

    (define map-set (set-mapper equal?))

    ;; ===== unfold-set ===================================================
    ;; (alpha ... -> Boolean), (alpha ... -> beta),
    ;; final?                  producer
    ;;                 (alpha ... -> alpha ...) -> (alpha ... -> set(beta))
    ;;                 step                         arguments

    ;; The unfold-set procedure constructs a procedure that first
    ;; determines whether the elements of arguments satisfy final?.  If so,
    ;; the constructed procedure returns the empty set.  Otherwise, it
    ;; returns a set containing the result of applying producer to the
    ;; elements of arguments, in addition to the members of the set
    ;; obtained by first applying step to the elements of arguments and
    ;; then applying the constructed procedure recursively to the results,
    ;; excluding duplicates.

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

    (define unfold-set (set-unfolder equal?))

    ;; ===== fast-map-set =================================================
    ;; (alpha -> beta), set(alpha) -> set(beta)
    ;; procedure        aro

    ;; The fast-map-set procedure constructs a set containing each of the
    ;; results of applying procedure to a value in aro.

    ;; Precondition:
    ;;   procedure can receive any member of aro.
    ;;   The results of applying procedure to different members of aro are
    ;;     different.

    (define (fast-map-set procedure aro)
      ((fold-set set (pipe (~initial procedure) fast-put-into-set)) aro))

    ;; ===== filter-set ===================================================
    ;; (alpha -> Boolean), set(alpha) -> set(alpha)
    ;; keep?               aro

    ;; The filter-set procedure constructs a set containing the members of
    ;; aro that satisfy keep?.

    ;; Precondition:
    ;;   keep? can receive any member of aro.

    (define (filter-set keep? aro)
      ((fold-set set (conditionally-combine keep? fast-put-into-set))
       aro))

    ;; ===== remp-set =====================================================
    ;; (alpha -> Boolean), set(alpha) -> set(alpha)
    ;; exclude?            aro

    ;; The remp-set procedure constructs a set containing the values in aro
    ;; that do not satisfy exclude?.

    ;; Precondition:
    ;;   exclude? can receive any member of aro.

    (define remp-set (pipe (~initial ^not) filter-set))

    ;; ===== partition-set ================================================
    ;; (alpha -> Boolean), set(alpha) -> set(alpha), set(alpha)
    ;; condition-met?      aro

    ;; The partition-set procedure constructs two sets, one containing the
    ;; members of aro that satisfy condition-met?, the other those that do
    ;; not.

    ;; Precondition:
    ;;   condition-met? can receive any member of aro.

    (define (partition-set condition-met? aro)
      ((fold-set (create (set) (set))
                 (lambda (candidate ins outs)
                   (if (condition-met? candidate)
                       (values (fast-put-into-set candidate ins) outs)
                       (values ins (fast-put-into-set candidate outs)))))
       aro))

    ;; ===== cardinality ==================================================
    ;; set(any) -> natural-number
    ;; aro

    ;; The cardinality procedure computes the number of members of aro.

    (define cardinality (fold-set (create 0) (pipe >next add1)))

    ;; ===== extract-from-set =============================================
    ;; (alpha -> Boolean), set(alpha) -> (box(alpha) | Boolean), set(alpha)
    ;; condition-met?      aro

    ;; The extract-from-bag procedure searches aro for a value that
    ;; satisfies condition-met?.  If it finds one, it returns a box
    ;; containing that value and a set containing the other members of aro;
    ;; otherwise, it returns #f and aro.

    ;; Precondition:
    ;;   condition-met? can receive any element of aro.

    (define (extract-from-set condition-met? aro)
      ((rec (extracter areto)
         (if (empty-set? areto)
             (values #f areto)
             (receive (chosen others) (take-from-set areto)
               (if (condition-met? chosen)
                   (values (box chosen) others)
                   (receive (sought relicts) (extracter others)
                     (values sought (fast-put-into-set chosen relicts)))))))
       aro))

    ;; ===== remove-from-set ==============================================
    ;; alpha, set(alpha) -> set(alpha)
    ;; delend aro

    ;; The remove-from-set procedure constructs a set containing all of the
    ;; members of aro except for delend.

    ;; Precondition:
    ;;   delend is a member of aro.

    (define (remove-from-set delend aro)
      ((rec (remover areto)
         (receive (chosen others) (take-from-set areto)
           (if (equal? delend chosen)
               others
               (fast-put-into-set chosen (remover others)))))
       aro))

    ;; ===== for-all-in-set? ===============================================
    ;; (alpha -> Boolean), set(alpha) -> Boolean
    ;; condition-met?      aro

    ;; The for-all-in-set? procedure determines whether all of the members
    ;; of aro satisfy condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any member of aro.

    (define (for-all-in-set? condition-met? aro)
      ((rec (ok? areto)
         (or (empty-set? areto)
             (receive (chosen others) (take-from-set areto)
               (and (condition-met? chosen) (ok? others)))))
       aro))

    ;; ===== exists-in-set? ===============================================
    ;; (alpha -> Boolean), set(alpha) -> Boolean
    ;; condition-met?      aro

    ;; The exists-in-set? procedure determines whether at least one of the
    ;; members of aro satisfies condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any member of aro.

    (define exists-in-set? (run (~initial ^not) for-all-in-set? not))

    ;; ===== member? ======================================================
    ;; alpha, set(alpha) -> Boolean
    ;; item   aro

    ;; The member? predicate determines whether item is a member of aro.

    (define member? (pipe (~initial (curry equal?)) exists-in-set?))

    ;; ===== set-membership ===============================================
    ;; (alpha, alpha -> Boolean) -> (alpha, set(alpha) -> Boolean)
    ;; equivalent?                   item   aro

    ;; The set-membership procedure constructs a predicate that determines
    ;; whether item is a member of aro, using equivalent? as its criterion
    ;; of sameness.

    ;; Preconditions:
    ;;   equivalent? can receive any members of aro. 
    ;;   equivalent? is an equivalence relation.

    (define (set-membership equivalent?)
      (pipe (~initial (curry equivalent?)) exists-in-set?))

    ;; ===== subset? ======================================================
    ;; set(alpha), set(alpha) -> Boolean
    ;; left        right

    ;; The subset? predicate determines whether left is a subset of right.

    (define (subset? left right)
      (for-all-in-set? (sect member? <> right) left))

    ;; ===== set-subsethood ===============================================
    ;; (alpha, alpha -> Boolean) -> (set(alpha), set(alpha) -> Boolean)
    ;; equivalent?                   left        right

    ;; The set-subsethood procedure constructs a predicate that determines
    ;; whether left is a subset of right, using equivalent? as its
    ;; criterion of sameness.

    ;; Preconditions:
    ;;   equivalent? can receive any member of left and any member of
    ;;     right.
    ;;   equivalent? is an equivalence relation.

    (define (set-subsethood equivalent?)
      (let ((mem? (set-membership equivalent?)))
        (lambda (left right)
          (for-all-in-set? (sect mem? <> right) left))))

    ;; ===== set-unioner ==================================================
    ;; (alpha, alpha -> Boolean) -> (set(alpha), set(alpha) -> set(alpha))
    ;; equivalent?                   left        right

    ;; The set-unioner procedure constructs a procedure that constructs a
    ;; set containing every member of left and every member of right, using
    ;; equivalent? as its criterion of sameness.

    ;; Precondition:
    ;;   equivalent? can receive any member of left or of right.
    ;;   equivalent? is an equivalence relation.

    (define (set-unioner equivalent?)
      (lambda (left right)
        ((fold-set (create right) (set-adjoiner equivalent?)) left)))

    ;; ===== union ========================================================
    ;; set(alpha), set(alpha) -> set(alpha)
    ;; left        right

    ;; The union procedure constructs a set containing every member of left
    ;; and every member of right.

    (define (union left right)
      ((fold-set (create right) put-into-set) left))

    ;; ===== fast-union ===================================================
    ;; set(alpha), set(alpha) -> set(alpha)
    ;; left        right

    ;; The fast-union procedure constructs a set containing every member of
    ;; left and every member of right.

    ;; Precondition:
    ;;   No member of left is a member of right.

    (define (fast-union left right)
      ((fold-set (create right) fast-put-into-set) left))

    ;; ===== intersection =================================================
    ;; set(alpha), set(alpha) -> set(alpha)
    ;; left        right

    ;; The intersection procedure constructs a set containing the common
    ;; members of left and right.

    (define (intersection left right)
      (filter-set (sect member? <> right) left))

    ;; ===== set-intersectioner ===========================================
    ;; (alpha, alpha -> Boolean) -> (set(alpha), set(alpha) -> set(alpha))
    ;; equivalent?                   left        right

    ;; The set-intersectioner procedure constructs a procedure that, in
    ;; turn, constructs a set containing the common members of left and
    ;; right, using equivalent? as its criterion of sameness.

    ;; Precondition:
    ;;   equivalent? can receive any member of left and any member of
    ;;     right.
    ;;   equivalent? is an equivalence relation.

    (define (set-intersectioner equivalent?)
      (let ((mem? (set-membership equivalent?)))
        (lambda (left right)
          (filter-set (sect mem? <> right) left))))

    ;; ===== disjoint? ====================================================
    ;; set(alpha), set(alpha) -> Boolean
    ;; left        right

    ;; The disjoint? procedure determines whether left and right are
    ;; disjoint.

    (define disjoint? (pipe intersection empty-set?))

    ;; ===== set-disjointness =============================================
    ;; (alpha, alpha -> Boolean) -> (set(alpha), set(alpha) -> Boolean)
    ;; equivalent?                   left        right

    ;; The set-disjointness procedure constructs a predicate that
    ;; determines whether left and right are disjoint, using equivalent? as
    ;; its criterion of sameness.

    ;; Preconditions:
    ;;   equivalent? can receive any member of left and any member of right.
    ;;   equivalent? is an equivalence relation.

    (define (set-disjointness equivalent?)
      (pipe (set-intersectioner equivalent?) empty-set?))

    ;; ===== set-difference ===============================================
    ;; set(alpha), set(alpha) -> set(alpha)
    ;; left        right

    ;; The set-difference procedure constructs the set of members of left
    ;; that are not members of right.

    (define (set-difference left right)
      (remp-set (sect member? <> right) left))

    ;; ===== set-differencer ==============================================
    ;; (alpha, alpha -> Boolean) -> (set(alpha), set(alpha) -> set(alpha))
    ;; equivalent?                   left        right

    ;; The set-differencer procedure constructs a procedure that, in turn,
    ;; constructs the set of members of left that are not members of right,
    ;; using equivalent? as its criterion of sameness.

    ;; Preconditions:
    ;;   equivalent? can receive any member of left and any member of
    ;;     right.
    ;;   equivalent? is an equivalence relation.

    (define (set-differencer equivalent?)
      (let ((mem? (set-membership equivalent?)))
        (lambda (left right)
          (remp-set (sect mem? <> right) left))))

    ;; ===== set->bag =====================================================
    ;; set(alpha) -> bag(alpha)
    ;; aro

    ;; The set->bag procedure constructs a bag containing the members of
    ;; aro.

    (define set->bag (fold-set bag put-into-bag))))

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
