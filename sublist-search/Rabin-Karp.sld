;;; The Rabin-Karp algorithm

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created December 16, 2005
;;; last revised January 9, 2017

(define-library (afp sublist-search Rabin-Karp)
  (export modular-numeric-evaluator slide-text-window
          Rabin-Karp-sublist-searcher string-evaluate substring-search)
  (import (afp primitives)
          (only (afp arithmetic) add1 sub1 mod)
          (only (afp constant-procedures) create)
          (only (afp procedure-sections) equal-to)
          (only (afp couplers) pipe cross)
          (only (afp adapters) identity)
          (only (afp natural-numbers) fold-natural lower-ply-natural)
          (only (afp lists)
                first rest empty-list? prepend unfold-list process-list run
                drop take matches-prefix?)
          (afp strings))
  (begin

    ;; ===== modular-numeric-evaluator ====================================
    ;; natural-number, (alpha -> natural-number), natural-number ->
    ;; base            digit-value                modulus
    ;;                                      (list(alpha) -> natural-number)
    ;;                                       ls

    ;; The modular-numeric-evaluator procedure constructs a procedure that
    ;; computes the numeric value of ls modulo modulus, taking base as the
    ;; base of numeration and applying digit-value to each element to
    ;; determine its numeric value as a digit in that system of numeration.

    ;; Preconditions:
    ;;   digit-value can receive any element of ls.
    ;;   Every value returned by digit-value is less than base.
    ;;   digit-value returns a different result for every argument it can
    ;;     receive.
    ;;   modulus is positive.

    (define (modular-numeric-evaluator base digit-value modulus)
      (process-list (create 0)
                    (run (cross digit-value (sect * <> base))
                         +
                         (sect mod <> modulus))))

    ;; ===== slide-text-window ============================================
    ;; natural-number, natural-number, natural-number ->
    ;; len             base            modulus
    ;;   (natural-number, natural-number, natural-number -> natural-number)
    ;;    current         old             new

    ;; The slide-text-window procedure constructs a procedure that computes
    ;; the numeric value (modulo modulus) of a list of length len
    ;; consisting of all but the initial element of a list with old as the
    ;; digit value of its initial element and current as its numeric value
    ;; (modulo modulus), followed by an element with new as its digit
    ;; value.  The elements of the list are assumed to be drawn from a
    ;; fixed, finite set of cardinality base.

    ;; Preconditions:
    ;;   len is positive.
    ;;   current is less than modulus.
    ;;   old is less than base.
    ;;   new is less than base.

    (define (slide-text-window len base modulus)
      (let ((initial-weight ((fold-natural (create 1)
                                           (pipe (sect * <> base)
                                                 (sect mod <> modulus)))
                             (sub1 len))))
        (lambda (current old new)
          (mod (+ (* (mod (- current (* old initial-weight)) modulus)
                     base)
                  new)
               modulus))))

    ;; ===== Rabin-Karp-sublist-searcher ==================================
    ;; natural-number, (alpha -> natural-number), natural-number ->
    ;; base            digit-value                modulus
    ;;                   (list(alpha), list(alpha) -> list(natural-number))
    ;;                    pattern      text

    ;; The sublist-search procedure constructs a procedure that, in turn,
    ;; constructs a list of the positions in text at which sublists that
    ;; match pattern begin, in ascending order.

    ;; Preconditions:
    ;;   digit-value can receive any element of pattern.
    ;;   digit-value can receive any element of text.
    ;;   Every value returned by digit-value is less than base.
    ;;   digit-value returns a different result for every argument it can
    ;;     receive.
    ;;   modulus is positive.
    ;;   The length of pattern is less than or equal to the length of text.

    (define (Rabin-Karp-sublist-searcher base digit-value modulus)
      (let ((evaluator
              (modular-numeric-evaluator base digit-value modulus)))
        (lambda (pattern text)
          (let ((pattern-length (length pattern))
                (text-length (length text)))
            (if (zero? pattern-length)
                ((unfold-list (equal-to (add1 text-length)) identity add1)
                 0)
                (let ((slider
                        (slide-text-window pattern-length base modulus)) 
                      (pattern-signature (evaluator pattern)))
                  ((rec (searcher position subtext after-drop signature)
                     (let ((rest-of-matches
                            (if (empty-list? after-drop)
                                (list)
                                (searcher
                                  (add1 position)
                                  (rest subtext)
                                  (rest after-drop)
                                  (slider 
                                    signature
                                    (digit-value (first subtext))
                                    (digit-value (first after-drop)))))))
                       (if (and (= signature pattern-signature)
                                (matches-prefix? pattern subtext))
                           (prepend position rest-of-matches)
                           rest-of-matches)))
                   0
                   text
                   (drop text pattern-length)
                   (evaluator (take text pattern-length)))))))))

    ;; ===== string-evaluate ==============================================
    ;; string, natural-number -> natural-number
    ;; str     len

    ;; The string-evaluate procedure computes the numeric signature of the
    ;; prefix of str of length len.

    ;; The static variable base is the exclusive upper bound of Unicode
    ;; scalar values (just greater than the greatest possible value
    ;; returned by char->integer).

    ;; The static variable modulus is the greatest prime less than or equal
    ;; to the quotient that results from dividing the largest available
    ;; fixnum value by base.

    ;; Preconditions:
    ;;   len is less than or equal to the length of str.

    (define (string-evaluate str len)
      (let ((base 1114112)
            (modulus 479))
        ((lower-ply-natural (create 0)
                            (run (cross (pipe (sect string-ref str <>)
                                              char->integer)
                                        (sect * <> base))
                                 +
                                 (sect mod <> modulus)))
         len)))

    ;; ===== substring-search =============================================
    ;; string, string -> list(natural-number)
    ;; pattern text

    ;; The substring-search procedure constructs a list of the positions in
    ;; text at which substrings that match pattern begin, in ascending
    ;; order.

    ;; The static variable base is the exclusive upper bound of Unicode
    ;; scalar values (just greater than the greatest possible value
    ;; returned by char->integer).

    ;; The static variable modulus is the greatest prime less than or equal
    ;; to the quotient that results from dividing the largest available
    ;; fixnum value by base.

    ;; Precondition:
    ;;   The length of pattern is less than or equal to the length of text.

    (define substring-search
      (let ((base 1114112)
            (modulus 479))
        (lambda (pattern text)
          (let ((pattern-length (string-length pattern))
                (text-length (string-length text)))
            (if (zero? pattern-length)
                ((unfold-list (equal-to (add1 text-length)) identity add1)
                 0)
                (let ((slider
                        (slide-text-window pattern-length base modulus)) 
                      (pattern-signature
                        (string-evaluate pattern pattern-length)))
                  ((rec (searcher position signature)
                     (let ((rest-of-matches
                             (if (<= text-length
                                     (+ position pattern-length))
                                 (list)
                                 (searcher
                                   (add1 position)
                                   (slider 
                                     signature
                                     (char->integer
                                       (string-ref text position))
                                     (char->integer
                                       (string-ref
                                         text
                                         (+ position pattern-length))))))))
                       (if (and (= signature pattern-signature)
                                (occurs-at? pattern text position))
                           (prepend position rest-of-matches)
                           rest-of-matches)))
                   0
                   (string-evaluate text pattern-length))))))))))

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
