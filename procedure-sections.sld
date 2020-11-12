;;; Procedure sections

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created March 3, 1999
;;; last revised December 30, 2016

(define-library (afp procedure-sections)
  (export invoke curry equal-to)
  (import (afp primitives))
  (begin

    ;; ===== invoke =======================================================
    ;; (alpha ... -> beta ...), alpha ... -> beta ...
    ;; procedure                arguments

    ;; The invoke procedure applies procedure to the elements of arguments,
    ;; returning the results.

    ;; Precondition:
    ;;   procedure can receive the elements of arguments.

    (define invoke (sect <> <...>))

    ;; ===== curry ========================================================
    ;; (alpha, beta ... -> gamma ...) ->
    ;; procedure
    ;;                                  (alpha   -> (beta ...  -> gamma ...))
    ;;                                   initial     remaining

    ;; The curry procedure constructs a unary procedure that, in turn,
    ;; constructs a procedure that applies procedure to initial and the
    ;; elements of remaining and returns the results.

    ;; Precondition:
    ;;   procedure can receive initial and the elements of remaining.

    (define (curry procedure)
      (lambda (initial)
        (lambda remaining
          (apply procedure initial remaining))))

    ;; ===== equal-to =====================================================
    ;; any  -> (any  -> Boolean)
    ;; left     right

    ;; The equal-to procedure constructs a unary predicate that determines
    ;; whether left and right are the same value.

    (define equal-to (curry equal?))))

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
