;;; Pairs

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 31, 1998
;;; last revised January 5, 2017

(define-library (afp pairs)
  (export decons pair=? pair-of pair-of=)
  (import (afp primitives)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) compare-by)
          (only (afp predicate-operations) ^et))
  (begin

    ;; ===== decons =======================================================
    ;; pair(alpha, beta) -> alpha, beta
    ;; pr

    ;; The decons procedure returns the components of pr.

    (define decons (dispatch car cdr))

    ;; ===== pair=? =======================================================
    ;; pair(any, any), pair(any, any) -> Boolean
    ;; left            right

    ;; The pair=? predicate determines whether left and right have the same
    ;; components, arranged in the same order.

    (define pair=? (^et (compare-by car equal?) (compare-by cdr equal?)))

    ;; ===== pair-of ======================================================
    ;; (any -> Boolean), (any -> Boolean) -> (any       -> Boolean)
    ;; car-tester?       cdr-tester?          something

    ;; The pair-of procedure constructs a predicate that determines whether
    ;; something is a pair in which the car satisfies car-tester? and the
    ;; cdr satisfies cdr-tester?.

    ;; Preconditions:
    ;;   car-tester? can receive any value.
    ;;   cdr-tester? can receive any value.

    (define (pair-of car-tester? cdr-tester?)
      (^et pair? (^et (pipe car car-tester?) (pipe cdr cdr-tester?))))

    ;; ===== pair-of= =====================================================
    ;; (alpha, alpha -> Boolean), (beta, beta -> Boolean) ->
    ;; same-car?                  same-cdr?
    ;;                    (pair(alpha, beta), pair(alpha, beta) -> Boolean)
    ;;                     left               right

    ;; The pair-of= procedure constructs a predicate that determines
    ;; whether left and right are the same by checking whether their cars
    ;; satisfy same-car? and their cdrs satisfy same-cdr?.

    ;; Preconditions:
    ;;   same-car? is an equivalence relation.
    ;;   same-car? can receive the car of left and the car of right.
    ;;   same-cdr? is an equivalence relation.
    ;;   same-cdr? can receive the cdr of left and the cdr of right.

    (define (pair-of= same-car? same-cdr?)
      (^et (compare-by car same-car?) (compare-by cdr same-cdr?)))))

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
