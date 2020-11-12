;;; Predicate operations

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 13, 1998
;;; last revised January 9, 2017

(define-library (afp predicate-operations)
  (export ^not ^et ^vel ^if conditionally-combine)
  (import (afp primitives)
          (only (afp couplers) pipe)
          (only (afp adapters) ~initial))
  (begin

    ;; ===== ^not =========================================================
    ;; (alpha ... -> Boolean) -> (alpha ... -> Boolean)
    ;; condition-met?            arguments

    ;; The ^not procedure constructs a predicate that the elements of
    ;; arguments satisfy if, and only if, they do not satisfy
    ;; condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive the elements of arguments.

    (define (^not condition-met?)
      (pipe condition-met? not))

    ;; ===== ^et ==========================================================
    ;; (alpha ... -> Boolean), (alpha ... -> Boolean) ->
    ;; left-condition-met?     right-condition-met?
    ;;                                               (alpha ... -> Boolean)
    ;;                                                arguments

    ;; The ^et procedure constructs a predicate that the elements of
    ;; arguments satisfy if, and only if, they satisfy both
    ;; left-condition-met? and right-condition-met?.

    ;; Preconditions:
    ;;   left-condition-met? can receive the elements of arguments.
    ;;   right-condition-met? can receive the elements of arguments.

    (define (^et left-condition-met? right-condition-met?)
      (lambda arguments
        (and (apply left-condition-met? arguments)
             (apply right-condition-met? arguments))))

    ;; ===== ^vel =========================================================
    ;; (alpha ... -> Boolean), (alpha ... -> Boolean) ->
    ;; left-condition-met?     right-condition-met?
    ;;                                               (alpha ... -> Boolean)
    ;;                                                arguments

    ;; The ^vel procedure constructs a predicate that the elements of
    ;; arguments satisfy if, and only if, they satisfy either
    ;; left-condition-met? or right-condition-met? (or both).

    ;; Preconditions:
    ;;   left-condition-met? can receive the elements of arguments.
    ;;   right-condition-met? can receive the elements of arguments.

    (define (^vel left-condition-met? right-condition-met?)
      (lambda arguments
        (or (apply left-condition-met? arguments)
            (apply right-condition-met? arguments))))

    ;; ===== ^if ==========================================================
    ;; (alpha ... -> Boolean), (alpha ... -> beta ...),
    ;; condition-met?          consequent
    ;;                   (alpha ... -> beta ...) -> (alpha ... -> beta ...)
    ;;                   alternate                   arguments

    ;; The ^if procedure constructs a procedure that first applies
    ;; condition-met? to the elements of arguments.  If they satisfy it,
    ;; then the constructed procedure applies consequent to the elements of
    ;; arguments and returns the results; if not, then the constructed
    ;; procedure applies alternate to the elements of arguments and returns
    ;; the results.

    ;; Preconditions:
    ;;   condition-met? can receive the elements of arguments.
    ;;   If the elements of arguments satisfy condition-met?, then 
    ;;     consequent can receive them.
    ;;   If the elements of arguments do not satisfy condition-met?, then
    ;;     alternate can receive them.

    (define (^if condition-met? consequent alternate)
      (lambda arguments
        (if (apply condition-met? arguments)
            (apply consequent arguments)
            (apply alternate arguments))))

    ;; ===== conditionally-combine ========================================
    ;; (alpha   -> Boolean), (alpha,  beta ... -> beta ...) ->
    ;; combine?               combiner
    ;;                                       (alpha,  beta ... -> beta ...)
    ;;                                        initial others

    ;; The conditionally-combine procedure constructs a procedure that
    ;; first applies combine? to initial.  If initial satisfies combine?,
    ;; then the constructed procedure applies combiner to initial and the
    ;; elements of others and returns the results; if not, the constructed
    ;; procedure returns the elements of others.

    ;; Preconditions:
    ;;   combine? can receive initial.
    ;;   If initial satisfies combine?, then combiner can receive initial
    ;;     and the elements of others.

    (define (conditionally-combine combine? combiner)
      (lambda (initial . others)
        (if (combine? initial)
            (apply combiner initial others)
            (delist others))))))

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
