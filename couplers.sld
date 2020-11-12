;;; Couplers

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 9, 1998
;;; last revised December 30, 2016

(define-library (afp couplers)
  (export compose pipe cross dispatch)
  (import (afp primitives)
          (only (afp procedure-sections) invoke))
  (begin
    
    ;; ===== compose ======================================================
    ;; (alpha ... -> beta ...), (gamma ... -> alpha ...) ->
    ;; outer                    inner
    ;;                                              (gamma ... -> beta ...)
    ;;                                               arguments

    ;; The compose procedure constructs a procedure that applies outer to
    ;; the results of applying inner to the elements of arguments,
    ;; returning the results.

    ;; Preconditions:
    ;;   outer can receive the results of any invocation of inner.
    ;;   inner can receive the elements of arguments.

    (define (compose outer inner)
      (lambda arguments
        (receive intermediates (apply inner arguments)
          (apply outer intermediates))))

    ;; ===== pipe ==========================================================
    ;; (alpha ... -> beta ...), (beta ... -> gamma ...) ->
    ;; earlier                  later
    ;;                                             (alpha ... -> gamma ...)
    ;;                                              arguments

    ;; The pipe procedure constructs a procedure that applies later to the
    ;; results of applying inner to the elements of arguments, returning
    ;; the results.

    ;; Preconditions:
    ;;   earlier can receive the elements of arguments.
    ;;   later can receive the results of any invocation of earlier.

    (define (pipe earlier later)
      (lambda arguments
        (receive intermediates (apply earlier arguments)
          (apply later intermediates))))

    ;; ===== cross ========================================================
    ;; (alpha -> beta) ... -> (alpha ...   -> beta ...)
    ;; procedures              arguments

    ;; The cross procedure constructs a procedure that applies each of the
    ;; elements of procedures to the corresponding element of arguments and
    ;; returns the results.

    ;; Preconditions:
    ;;   The number of elements of procedures is equal to the number of
    ;;     elements of arguments.
    ;;   Each of the elements of procedures can receive the corresponding
    ;;     element of arguments.

    (define (cross . procedures)
      (lambda arguments
        (delist (map invoke procedures arguments))))

    ;; ===== dispatch =====================================================
    ;; (alpha ... -> beta) ... -> (alpha ...   -> beta ...)
    ;; procedures                  arguments

    ;; The dispatch procedure constructs a procedure that applies each of
    ;; the elements of procedures to the elements of arguments, returning
    ;; the results.

    ;; Precondition:
    ;;   Each element of procedures can receive the elements of arguments.

    (define (dispatch . procedures)
      (lambda arguments
        (delist (map (sect apply <> arguments) procedures))))))

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
