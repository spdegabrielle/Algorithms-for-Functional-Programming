;;; Adapters

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created March 1, 1999
;;; last revised December 30, 2016

(define-library (afp adapters)
  (export >initial >next >all-but-initial identity >exch converse ~initial
          ~next ~each compare-by same-in-parity?)
  (import (afp primitives)
          (only (afp couplers) pipe))
  (begin

    ;; ===== >initial =====================================================
    ;; alpha,    any ... -> alpha
    ;; initial ignored

    ;; The >initial procedure returns initial.

    (define (>initial initial . ignored)
      initial)

    ;; ===== >next ========================================================
    ;; any,    alpha, any ... -> alpha
    ;; initial next   ignored

    ;; The >next procedure returns next.

    (define (>next initial next . ignored)
      next)

    ;; ===== >all-but-initial =============================================
    ;; any,    alpha ... -> alpha ...
    ;; initial others

    ;; The >all-but-initial procedure returns the elements of others.

    (define (>all-but-initial initial . others)
      (delist others))

    ;; ===== identity =====================================================
    ;; alpha     -> alpha
    ;; something

    ;; The identity procedure returns something.

    (define (identity something)
      something)

    ;; ===== >exch ========================================================
    ;; alpha,  beta, gamma ... -> beta, alpha, gamma ...
    ;; initial next  others

    ;; The >exch procedure returns next, initial, and the elements of others,
    ;; in that order.

    (define (>exch initial next . others)
      (apply values next initial others))

    ;; ===== converse =====================================================
    ;; (alpha, beta -> gamma ...) -> (beta, alpha -> gamma ...)
    ;; procedure                      left  right

    ;; The converse procedure constructs a binary procedure that applies
    ;; procedure to right and left and returns the results.  (In other
    ;; words, it returns a procedure similar to procedure, but with the
    ;; order of the arguments reversed.)

    ;; Precondition:
    ;;   procedure can receive right and left.

    (define converse (sect pipe >exch <>))

    ;; ===== ~initial =====================================================
    ;; (alpha -> beta) -> (alpha,  gamma ... -> beta, gamma ...)
    ;; procedure           initial others

    ;; The ~initial procedure constructs a procedure that applies procedure
    ;; to initial and returns the result of that application and the
    ;; elements of others.

    ;; Precondition:
    ;;   procedure can receive initial.

    (define (~initial procedure)
      (lambda (initial . others)
        (apply values (procedure initial) others)))

    ;; ===== ~next ========================================================
    ;; (alpha -> beta) ->
    ;; procedure
    ;;                 (gamma,  alpha, delta ... -> gamma, beta, delta ...)
    ;;                  initial next   others

    ;; The ~next procedure constructs a procedure that applies procedure to
    ;; next and returns initial, the result of that application, and the
    ;; elements of others.

    ;; Precondition:
    ;;   procedure can receive next.

    (define (~next procedure)
      (lambda (initial next . others)
        (apply values initial (procedure next) others)))

    ;; ===== ~each ========================================================
    ;; (alpha -> beta) -> (alpha ... -> beta ...)
    ;; procedure           arguments

    ;; The ~each procedure constructs a procedure that applies procedure to
    ;; each element of arguments, returning the results.

    ;; Precondition:
    ;;   procedure can receive each element of arguments.

    (define (~each procedure)
      (lambda arguments
        (delist (map procedure arguments))))

    ;; ===== compare-by ===================================================
    ;; (alpha -> beta), (beta ... -> Boolean) -> (alpha ...  -> Boolean)
    ;; pre              comparer                  comparands

    ;; The compare-by procedure constructs a predicate that applies pre to
    ;; each element of comparands and applies comparer to the results,
    ;; returning the result.

    ;; Preconditions:
    ;;   pre can receive each element of comparands.
    ;;   comparer can receive the results of any invocation of pre.

    (define (compare-by pre comparer)
      (pipe (~each pre) comparer))

    ;; ===== same-in-parity? ==============================================
    ;; integer, integer -> Boolean
    ;; left     right

    ;; The same-in-parity? predicate determines whether the parity of left
    ;; and the parity of right are the same -- that is, whether both are
    ;; even or both are odd.

    (define same-in-parity? (compare-by even? boolean=?))))

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
