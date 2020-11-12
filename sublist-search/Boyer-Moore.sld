;;; Boyer-Moore substring search

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created June 22, 1999
;;; last revised January 9, 2017

(define-library (afp sublist-search Boyer-Moore)
  (export rightmost-occurrence-finder Boyer-Moore-stepper substring-search)
  (import (afp primitives)
          (only (afp arithmetic) add1 sub1)
          (only (afp lists) first rest prepend empty-list?)
          (only (afp tables) put-into-table table lookup))
  (begin

    ;; ===== rightmost-occurrence-finder ==================================
    ;; string -> (character  -> integer)
    ;; pattern    sought

    ;; The rightmost-occurrence-finder procedure constructs a procedure
    ;; that computes the greatest position at which sought occurs within
    ;; pattern, returning -1 if there is no such position.

    (define (rightmost-occurrence-finder pattern)
      (let ((len (string-length pattern)))
        ((rec (builder position tab)
           (if (= position len)
               (sect lookup <> tab -1)
               (builder (add1 position)
                        (put-into-table (string-ref pattern position)
                                        position
                                        tab))))
         0 (table))))

    ;; ===== Boyer-Moore-stepper ==========================================
    ;; string, string -> (natural-number -> integer)
    ;; pattern text       start

    ;; The Boyer-Moore-stepper procedure constructs a procedure that tries
    ;; to match pattern to the substring of text that begins at start,
    ;; returning the position in pattern at which a mismatch occurs, or -1
    ;; if the match is successful.

    ;; Precondition:
    ;;   The sum of start and the length of pattern is less than or equal
    ;;     to the length of text.

    (define (Boyer-Moore-stepper pattern text)
      (let ((len (string-length pattern)))
        (lambda (start)
          ((rec (matcher remaining)
             (if (zero? remaining)
                 -1
                 (let ((next (sub1 remaining)))
                   (if (char=? (string-ref pattern next)
                               (string-ref text (+ start next)))
                       (matcher next)
                       next))))
           len))))

    ;; ===== substring-search =============================================
    ;; string, string -> list(natural-number)
    ;; pattern text

    ;; The substring-search procedure constructs a list of the positions in
    ;; text at which substrings that match pattern begin, in ascending
    ;; order.

    ;; Precondition:
    ;;   The length of pattern is less than or equal to the length of text.

    (define (substring-search pattern text)
      (let ((find (rightmost-occurrence-finder pattern))
            (step (Boyer-Moore-stepper pattern text))
            (last-position (- (string-length text)
                              (string-length pattern))))
        ((rec (shifter position)
           (if (< last-position position)
               (list)
               (let ((step-result (step position)))
                 (if (negative? step-result)
                     (prepend position (shifter (add1 position)))
                     (let ((pattern-position
                            (find (string-ref text
                                              (+ position step-result)))))
                       (shifter (+ position
                                   (max 1 (- pattern-position
                                             step-result)))))))))
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
