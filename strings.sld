;;; Strings

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created December 8, 2005
;;; last revised January 9, 2017

(define-library (afp strings)
  (export occurs-at?)
  (import (afp primitives)
          (only (afp arithmetic) add1)
          (only (afp couplers) pipe dispatch)
          (only (afp recursion-managers) check))
  (begin

    ;; ===== occurs-at? ===================================================
    ;; string, string, natural-number -> Boolean
    ;; pattern text    shift

    ;; The occurs-at? procedure determines whether pattern is a substring
    ;; of text that begins at position shift.

    ;; Precondition:
    ;;   The sum of shift and the length of pattern is less than or equal
    ;;     to the length of text.

    (define (occurs-at? pattern text shift)
      (let ((pattern-length (string-length pattern)))
        ((check (sect = <> pattern-length)
                (pipe (dispatch (sect string-ref pattern <>)
                                (pipe (sect + shift <>)
                                      (sect string-ref text <>)))
                      char=?)
                add1)
         0)))))

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
