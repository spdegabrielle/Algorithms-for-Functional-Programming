;;; Constant procedures

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 21, 1998
;;; last revised December 30, 2016

(define-library (afp constant-procedures)
  (export values? constant create black-hole)
  (import (afp primitives))
  (begin
    
    ;; ===== values? ======================================================
    ;; any ... -> Boolean
    ;; ignored

    ;; The values? procedure returns #t no matter what argments it
    ;; receives.

    (define (values? . ignored)
      #t)

    ;; ===== constant =====================================================
    ;; alpha ...    -> (any ... -> alpha ...)
    ;; fixed-values     ignored

    ;; The constant procedure constructs a procedure that returns the
    ;; elements of fixed-values no matter what arguments it receives.

    (define (constant . fixed-values)
      (lambda ignored
        (delist fixed-values)))

    ;; ===== create =======================================================
    ;; alpha ...    -> (-> alpha ...)
    ;; fixed-values

    ;; The create procedure constructs a nullary procedure that returns the
    ;; elements of fixed-values.

    (define (create . fixed-values)
      (lambda ()
        (delist fixed-values)))

    ;; ===== black-hole ===================================================
    ;; any ... ->
    ;; ignored

    ;; The black-hole procedure returns no values, no matter what arguments
    ;; it receives.

    (define black-hole (constant))))

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
