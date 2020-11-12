;;; Recursion managers

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 5, 1999
;;; last revised December 30, 2016

(define-library (afp recursion-managers)
  (export recur build check iterate)
  (import (afp primitives)
          (only (afp couplers) pipe))
  (begin

    ;; ===== recur ========================================================
    ;; (alpha -> Boolean), (alpha -> beta ...), (alpha -> gamma, alpha),
    ;; base?               terminal             simplify
    ;;                 (gamma, beta ... -> beta ...) -> (alpha -> beta ...)
    ;;                 integrate                         guide

    ;; The recur procedure constructs a singly recursive procedure that
    ;; applies base? to guide to determine whether the base case has been
    ;; reached, applies terminal to guide to obtain the results to be
    ;; returned in the base case, applies simplify to guide to separate the
    ;; values needed for the local computation from those needed for the
    ;; recursive invocation, invokes itself recursively, and applies
    ;; integrate to the locally needed values and the results of the
    ;; recursive invocation to obtain the results to be returned.

    ;; Preconditions:
    ;;   base? can receive guide.
    ;;   base? can receive the second result of any invocation of simplify.
    ;;   If guide satisfies base?, then terminal can receive it.
    ;;   If the second result of an invocation of simplify satisfies base?,
    ;;     then terminal can receive it.
    ;;   If guide does not satisfy base?, then simplify can receive it.
    ;;   If the second result of an invocation of simplify does not satisfy
    ;;     base?, then simplify can receive it.
    ;;   integrate can receive the first result of an invocation of
    ;;     simplify and the results of an invocation of terminal.
    ;;   integrate can receive the first result of an invocation of
    ;;     simplify and the results of an invocation of integrate.

    (define (recur base? terminal simplify integrate)
      (rec (recurrer guide)
        (if (base? guide)
            (terminal guide)
            (receive (current next) (simplify guide)
              (receive recursive-results (recurrer next)
                (apply integrate current recursive-results))))))

    ;; ===== build ==========================================================
    ;; (alpha ... -> Boolean), (alpha ... -> beta ...), (alpha ... -> gamma),
    ;; base?                   terminal                 derive
    ;;     (alpha ... -> alpha ...), (gamma, beta ... -> beta ...) ->
    ;;     simplify                  integrate
    ;;                                              (alpha ... -> beta ...)
    ;;                                               guides

    ;; The build procedure constructs a simply recursive procedure that
    ;; applies base? to the elements of guides to determine whether the
    ;; base case has been reached, applies terminal to the elements of
    ;; guides to obtain the results to be returned in the base case,
    ;; applies derive to the elements of guides to obtain the value needed
    ;; for the local computation, applies simplify to the elements of
    ;; guides to obtain the values needed for the recursive invocation,
    ;; invokes itself recursively, and applies integrate to the locally
    ;; needed value and the results of the recursive invocation to obtain
    ;; the results to be returned in the recursive case.

    ;; Preconditions:
    ;;   base? can receive the elements of guides.
    ;;   base? can receive the results of any invocation of simplify.
    ;;   If the elements of guides satisfy base?, then terminal can receive
    ;;     them.
    ;;   If the results of an invocation of simplify satisfy base?, then
    ;;     terminal can receive them.
    ;;   If the elements of guides do not satisfy base?, then derive can
    ;;     receive them.
    ;;   If the results of an invocation of simplify do not satisfy base?,
    ;;     then derive can receive them.
    ;;   If the elements of guides do not satisfy base?, then simplify can
    ;;     receive them.
    ;;   If the results of an invocation of simplify do not satisfy base?,
    ;;     then simplify can receive them.
    ;;   integrate can receive the result of any invocation of derive and
    ;;     the results of any invocation of terminal.
    ;;   integrate can receive the result of any invocation of derive and
    ;;     the results of any invocation of integrate.

    (define (build base? terminal derive simplify integrate)
      (rec (builder . guides)
        (if (apply base? guides)
            (apply terminal guides)
            (receive recursive-results
                     (apply (pipe simplify builder) guides)
              (apply integrate (apply derive guides) recursive-results)))))

    ;; ===== check ========================================================
    ;; (alpha ... -> Boolean), (alpha ... -> Boolean),
    ;; stop?                   continue?
    ;;                   (alpha ... -> alpha ...) -> (alpha ... -> Boolean)
    ;;                   step                         arguments

    ;; The check procedure constructs a predicate that is satisfied if
    ;; either the elements of arguments satisfy stop?, or they satisfy
    ;; continue? and the results of applying step to them satisfy the
    ;; constructed predicate.

    ;; Preconditions:
    ;;   stop? can receive the elements of arguments.
    ;;   stop? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy stop?, then continue?
    ;;     can receive them.
    ;;   If the results of an invocation of step do not satisfy stop?, then
    ;;     continue? can receive them.
    ;;   If the elements of arguments do not satisfy stop? but do satisfy
    ;;     continue?, then step can receive them.
    ;;   If the results of an invocation of step do not satisfy stop? but
    ;;     do satisfy continue?, then step can receive them.

    (define (check stop? continue? step)
      (rec (checker . arguments)
        (or (apply stop? arguments)
            (and (apply continue? arguments)
                 (apply (pipe step checker) arguments)))))

    ;; ===== iterate ======================================================
    ;; (alpha ... -> Boolean), (alpha ... -> alpha ...) ->
    ;; stop?                   step
    ;;                                             (alpha ... -> alpha ...)
    ;;                                             arguments

    ;; The iterate procedure constructs a singly recursive procedure that
    ;; applies stop? to the elements of arguments to determine whether the
    ;; base case has been reached (in which case it returns the elements of
    ;; arguments) and applies step to the elements of arguments to obtain
    ;; the values needed for the recursive invocation (returning the
    ;; results of that recursive invocation).

    ;; Preconditions:
    ;;   stop? can receive the elements of arguments.
    ;;   stop? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy stop?, then step can
    ;;     receive them.
    ;;   If the results of an invocation of step do not satisfy stop?, then
    ;;     step can receive them.

    (define (iterate stop? step)
      (rec (iterator . arguments)
        (if (apply stop? arguments)
            (apply values arguments)
            (apply (pipe step iterator) arguments))))))

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
