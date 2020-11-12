;;; Boxes

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created March 8, 1999
;;; last revised January 5, 2017

(define-library (afp boxes)
  (export box box? box=? debox box-of box-of=)
  (import (afp primitives)
          (only (afp adapters) compare-by)
          (only (afp couplers) pipe)
          (only (afp predicate-operations) ^et)
          (afp pairs))
  (begin
    
    ;; ===== box ==========================================================
    ;; alpha    -> box(alpha)
    ;; contents

    ;; The box procedure constructs a box containing contents.

    (define box (sect cons <> null))

    ;; ===== box? =========================================================
    ;; any       -> Boolean
    ;; something

    ;; The box? predicate determines whether something is a box.

    (define box? (^et pair? (pipe cdr null?)))

    ;; ===== box=? ========================================================
    ;; box(any), box(any) -> Boolean
    ;; left      right

    ;; The box=? procedure determines whether left and right contain the
    ;; same value.

    (define box=? (compare-by car equal?))

    ;; ===== debox ========================================================
    ;; box(alpha) -> alpha
    ;; bx

    ;; The debox procedure returns the contents of bx.

    (define debox car)

    ;; ===== box-of =======================================================
    ;; (any -> Boolean) -> (any       -> Boolean)
    ;; contents-tester?     something

    ;; The box-of procedure constructs a predicate that determines whether
    ;; something is a box containing a value that satisfies
    ;; contents-tester?.

    ;; Precondition:
    ;;   contents-tester? can receive any value.

    (define (box-of contents-tester?)
      (^et box? (pipe debox contents-tester?)))

    ;; ===== box-of= ======================================================
    ;; (alpha, alpha -> Boolean) -> (box(alpha), box(alpha) -> Boolean)
    ;; same-contents?                left        right

    ;; The box-of= procedure constructs a predicate that determines whether
    ;; left and right are the same, by checking whether their contents
    ;; satisfy same-contents?.

    ;; Preconditions:
    ;;   same-contents? is an equivalence relation.
    ;;   same-contents? can receive the contents of left and the contents of
    ;;     right.

    (define box-of= (sect compare-by debox <>))))

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
