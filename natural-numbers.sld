;;; Natural numbers and recursion

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 25, 1999
;;; last revised December 30, 2016

(define-library (afp natural-numbers)
  (export fold-natural ply-natural lower-ply-natural summation tally
          conditionally-tally for-all-less-than exists-less-than)
  (import (afp primitives)
          (only (afp arithmetic) add1 sub1)
          (only (afp constant-procedures) constant create black-hole)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) >next identity ~initial)
          (only (afp recursion-managers) recur build check)
          (only (afp predicate-operations) ^not conditionally-combine))
  (begin

    ;; ===== fold-natural =================================================
    ;; (-> alpha ...), (alpha ... -> alpha ...) ->
    ;; base            step
    ;;                                        (natural-number -> alpha ...)
    ;;                                         nat

    ;; The fold-natural procedure constructs a procedure that returns the
    ;; results of invoking base if nat is 0.  If nat is non-zero, the
    ;; constructed procedure applies itself recursively to the predecessor
    ;; of nat and returns the results of applying step to the results of
    ;; the recursive invocation.

    ;; Preconditions:
    ;;   step can receive the results of an invocation of base.
    ;;   step can receive the results of any invocation of step.

    (define (fold-natural base step)
      (rec (natural-number-mapper nat)
        (if (zero? nat)
            (base)
            (receive recursive-results (natural-number-mapper (sub1 nat))
              (apply step recursive-results)))))

    ;; ===== ply-natural ==================================================
    ;; (-> alpha ...), (natural-number, alpha ... -> alpha ...) ->
    ;; base            step
    ;;                                        (natural-number -> alpha ...)
    ;;                                         nat

    ;; The ply-natural procedure constructs a procedure that returns the
    ;; results of invoking base if nat is zero.  If nat is non-zero, the
    ;; constructed procedure applies itself recursively to the predecessor
    ;; of nat and returns the results of applying step to nat and the
    ;; results of the recursive invocation.

    ;; Preconditions:
    ;;   step can receive 1 and the results of an invocation of base.
    ;;   step can receive any natural number greater than 1 and the results
    ;;     of any invocation of step.

    (define (ply-natural base step)
      (recur zero? (pipe black-hole base) (dispatch identity sub1) step))

    ;; ===== lower-ply-natural ============================================
    ;; (-> alpha ...), (natural-number, alpha ... -> alpha ...) ->
    ;; base            step
    ;;                                        (natural-number -> alpha ...)
    ;;                                         nat

    ;; The lower-ply-natural procedure constructs a procedure that returns
    ;; the results of invoking base if nat is zero.  If nat is non-zero,
    ;; the constructed procedure applies itself recursively to the
    ;; predecessor of nat and returns the results of applying step to the
    ;; predecessor of nat and the results of the recursive invocation.

    ;; Preconditions:
    ;;   step can receive 0 and the results of an invocation of base.
    ;;   step can receive any positive integer and the results of any
    ;;     invocation of step.

    (define (lower-ply-natural base step)
      (ply-natural base (pipe (~initial sub1) step)))

    ;; ===== summation ====================================================
    ;; (integer -> number), integer, integer -> number
    ;; function             lower    upper

    ;; The summation procedure computes the sum of the results of applying
    ;; function to all of the integers from lower up to and including
    ;; upper.

    ;; Preconditions:
    ;;   function can receive any integer in the range from lower up to
    ;;     and including upper.
    ;;   lower is less than or equal to upper.

    (define (summation function lower-bound upper-bound)
      ((ply-natural (create (function lower-bound))
                    (pipe (~initial (sect + <> lower-bound))
                          (pipe (~initial function) +)))
       (- upper-bound lower-bound)))

    ;; ===== tally ========================================================
    ;; (alpha ... -> Boolean), (alpha ... -> alpha ...) ->
    ;; final?                  step
    ;;                                        (alpha ... -> natural-number)
    ;;                                         arguments

    ;; The tally procedure constructs a procedure that returns 0 if the
    ;; elements of arguments satisfy final?; otherwise, the constructed
    ;; procedure applies step to those elements, applies itself recursively
    ;; to the results, and returns the successor of the result.

    ;; Preconditions:
    ;;   final? can receive the elements of arguments.
    ;;   final? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy final?, then step can
    ;;     receive them.
    ;;   If the results of an invocation of step do not satisfy final?,
    ;;     then step can receive them.

    (define (tally final? step)
      (rec (tallier .  arguments)
        (if (apply final? arguments)
            0
            (add1 (apply (pipe step tallier) arguments)))))

    ;; ===== conditionally-tally ==========================================
    ;; (alpha ... -> Boolean), (alpha ... -> Boolean),
    ;; final?                  condition-met?
    ;;            (alpha ... -> alpha ...) -> (alpha ... -> natural-number)
    ;;            step                         arguments

    ;; The conditionally-tally procedure constructs a procedure that
    ;; returns 0 if the elements of arguments satisfy final?; otherwise,
    ;; the constructed procedure checks whether those elements satisfy
    ;; condition-met?, then applies step to those elements, applies itself
    ;; recursively to the results, and returns either the successor of the
    ;; result (if the elements satisfied condition-met?) or the result
    ;; itself (if they did not).

    ;; Preconditions:
    ;;   final? can receive the elements of arguments.
    ;;   final? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy final?, then
    ;;     condition-met? can receive them.
    ;;   If the results of an invocation of step do not satisfy final?, then
    ;;     condition-met? can receive them.
    ;;   If the elements of arguments do not satisfy final?, then step can
    ;;     receive them.
    ;;   If the results of an invocation of step do not satisfy final?, then
    ;;     step can receive them.

    (define (conditionally-tally final? condition-met? step)
      (build final?
             (constant 0)
             condition-met?
             step
             (conditionally-combine identity (pipe >next add1))))

    ;; ===== for-all-less-than ============================================
    ;; (natural-number -> Boolean) -> (natural-number        -> Boolean)
    ;; condition-met?                  exclusive-upper-bound  

    ;; The for-all-less-than procedure constructs a predicate that
    ;; determines whether every natural number less than
    ;; exclusive-upper-bound satisfies condition-met?.

    ;; Precondition:
    ;;   predicate can receive any natural number less than
    ;;     exclusive-upper-bound.

    (define (for-all-less-than condition-met?)
      (lambda (exclusive-upper-bound)
        ((check (sect = <> exclusive-upper-bound) condition-met? add1) 0)))

    ;; ===== exists-less-than =============================================
    ;; (natural-number -> Boolean) -> (natural-number        -> Boolean)
    ;; condition-met?                  exclusive-upper-bound  

    ;; The exists-less-than procedure constructs a predicate that
    ;; determines whether at least one natural number less than
    ;; exclusive-upper-bound satisfies condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any natural number less than
    ;;     exclusive-upper-bound.

    (define exists-less-than (pipe ^not (pipe for-all-less-than ^not)))))

;;; copyright (C) 2011, 2016 John David Stone

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
