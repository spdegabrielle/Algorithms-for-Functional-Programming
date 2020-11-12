;;; Simple, slow sublist search

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created June 21, 1999
;;; last revised January 9, 2017

(define-library (afp sublist-search simple-slow-sublist-search)
  (export sublist-search substring-search)
  (import (afp primitives)
          (only (afp arithmetic) add1)
          (only (afp lists) rest prepend matches-prefix?)
          (afp strings))
  (begin

    ;; ===== sublist-search ===============================================
    ;; list(alpha), list(alpha) -> list(natural-number)
    ;; pattern      text

    ;; The sublist-search procedure constructs a list of the positions in
    ;; text of sublists that match pattern, in ascending order.

    ;; Precondition:
    ;;   The length of pattern is less than or equal to the length of text.

    (define (sublist-search pattern text)
      (let ((last-position (- (length text) (length pattern))))
        ((rec (searcher start remaining-text)
           (let ((rest-of-matches (if (= start last-position)
                                      (list)
                                      (searcher (add1 start)
                                                (rest remaining-text)))))
             (if (matches-prefix? pattern remaining-text)
                 (prepend start rest-of-matches)
                 rest-of-matches)))
         0 text)))

    ;; ===== substring-search =============================================
    ;; string, string -> list(natural-number)
    ;; pattern text

    ;; The substring-search procedure constructs a list of the positions in
    ;; text at which substrings that match pattern begin, in ascending
    ;; order.

    ;; Precondition:
    ;;   The length of pattern is less than or equal to the length of text.

    (define (substring-search pattern text)
      (let ((last-position (- (string-length text)
                              (string-length pattern))))
        ((rec (searcher start)
           (let ((rest-of-matches (if (= start last-position)
                                      (list)
                                      (searcher (add1 start)))))
             (if (occurs-at? pattern text start)
                 (prepend start rest-of-matches)
                 rest-of-matches)))
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
