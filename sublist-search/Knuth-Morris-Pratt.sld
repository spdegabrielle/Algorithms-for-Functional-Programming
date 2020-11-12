;;; The Knuth-Morris-Pratt algorithm

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created December 21, 2005
;;; last revised January 9, 2017

(define-library (afp sublist-search Knuth-Morris-Pratt)
  (export next-prefix-length prefix-function KMP-stepper sublist-search
          next-string-prefix-length string-prefix-function
          KMP-string-stepper substring-search)
  (import (afp primitives)
          (only (afp arithmetic) add1 sub1)
          (only (afp constant-procedures) create)
          (only (afp procedure-sections) equal-to)
          (only (afp couplers) pipe)
          (only (afp adapters) identity)
          (only (afp natural-numbers) ply-natural)
          (only (afp lists) first rest empty-list? prepend unfold-list))
  (begin

    ;; ===== next-prefix-length =========================================
    ;; list(any), natural-number, list(natural-number) -> natural-number
    ;; ls         index           prefix-lengths

    ;; The next-prefix-length procedure computes the length of the longest
    ;; prefix of ls that is also a proper suffix of the prefix of ls of
    ;; length index, consulting prefix-lengths to obtain similar values for
    ;; lesser values of index.

    ;; Preconditions:
    ;;   index is less than or equal to the length of ls.
    ;;   The length of prefix-lengths is index.
    ;;   For every natural number n less than index, the element at
    ;;     position index - n - 1 of prefix-lengths is the length of the
    ;;     longest prefix of ls that is also a proper suffix of the prefix
    ;;     of ls of length n.

    (define (next-prefix-length ls index prefix-lengths)
      ((rec (matcher matched)
         (if (equal? (list-ref ls matched) (list-ref ls index))
             (add1 matched)
             (if (zero? matched)
                 0
                 (matcher (list-ref prefix-lengths (- index matched))))))
       (first prefix-lengths)))

    ;; ===== prefix-function ==============================================
    ;; list(any) -> (natural-number -> natural-number)
    ;; pattern       index

    ;; The prefix-function procedure constructs a procedure that computes
    ;; the length of the longest prefix of pattern that is also a proper
    ;; suffix of the prefix of pattern of length index.

    ;; Preconditions:
    ;;   index is less than or equal to the length of pattern.

    (define (prefix-function pattern)
      (let ((len (length pattern)))
        (let ((prefix-function-list
               ((ply-natural
                  (create (list 0))
                  (lambda (index prefix-lengths)
                    (prepend (next-prefix-length pattern
                                                 index
                                                 prefix-lengths)
                             prefix-lengths)))
                (sub1 len))))
          (pipe (sect - len <>) (sect list-ref prefix-function-list <>)))))

    ;; ===== KMP-stepper ==================================================
    ;; list(alpha) ->
    ;; pattern
    ;;             (alpha,       natural-number -> Boolean, natural-number)
    ;;              text-element matched

    ;; The KMP-stepper procedure constructs a procedure that tests whether
    ;; text-element occurs at position matched in pattern and, if so,
    ;; whether position matched is the last position in pattern.  If both
    ;; conditions are met, the constructed procedure returns #t and the
    ;; length of the longest proper prefix of pattern that is also a suffix
    ;; of pattern; otherwise, the constructed procedure returns #f and, as
    ;; a second result, either the successor of matched (if text-element
    ;; was successfully matched to an element of pattern), or the length of
    ;; the longest proper prefix of pattern that is also a suffix of the
    ;; prefix of pattern ending just before position matched.

    ;; Preconditions:
    ;;   pattern is not empty.
    ;;   matched is less than the length of pattern.

    (define (KMP-stepper pattern)
      (let ((len (length pattern))
            (pf (prefix-function pattern)))
        (lambda (text-element matched)
          ((rec (matcher pattern-position)
             (if (equal? (list-ref pattern pattern-position) text-element)
                 (let ((one-more (add1 pattern-position)))
                   (if (= one-more len)
                       (values #t (pf one-more))
                       (values #f one-more)))
                 (if (zero? pattern-position)
                     (values #f 0)
                     (matcher (pf pattern-position)))))
           matched))))

    ;; ===== sublist-search ===============================================
    ;; list(alpha), list(alpha) -> list(natural-number)
    ;; pattern      text

    ;; The sublist-search procedure constructs a list of the positions in
    ;; text at which sublists that match pattern begin, in ascending order.

    ;; Precondition:
    ;;   The length of pattern is less than or equal to the length of text.

    (define (sublist-search pattern text)
      (let ((text-length (length text)))
        (if (zero? (length pattern))
            ((unfold-list (equal-to (add1 text-length)) identity add1) 0)
            (let ((stepper (KMP-stepper pattern)))
              ((rec (searcher subtext position matched)
                 (if (empty-list? subtext)
                     (list)
                     (receive (completed new-matched)
                              (stepper (first subtext) matched)
                       (let ((recursive-result (searcher (rest subtext)
                                                         (add1 position)
                                                         new-matched)))
                         (if completed
                             (prepend (- position matched)
                                      recursive-result)
                             recursive-result)))))
               text 0 0)))))

    ;; ===== next-string-prefix-length ====================================
    ;; string, natural-number, list(natural-number) -> natural-number
    ;; str     index           prefix-lengths

    ;; The next-prefix-length procedure computes the length of the longest
    ;; prefix of str that is also a proper suffix of the prefix of str of
    ;; length index, consulting prefix-lengths to obtain similar values for
    ;; lesser values of index.

    ;; Preconditions:
    ;;   index is less than or equal to the length of str.
    ;;   The length of prefix-lengths is index.
    ;;   For every natural number n less than index, the element at
    ;;     position index - n - 1 of prefix-lengths is the length of the
    ;;     longest prefix of str that is also a proper suffix of the prefix
    ;;     of str of length n.

    (define (next-string-prefix-length str index prefix-lengths)
      ((rec (matcher matched)
         (if (equal? (string-ref str matched) (string-ref str index))
             (add1 matched)
             (if (zero? matched)
                 0
                 (matcher (list-ref prefix-lengths (- index matched))))))
       (first prefix-lengths)))

    ;; ===== string-prefix-function =======================================
    ;; string  -> (natural-number -> natural-number)
    ;; pattern     index

    ;; The string-prefix-function procedure constructs a procedure that
    ;; determines the length of the longest prefix of pattern that is also
    ;; a proper suffix of the prefix of pattern of length index.

    ;; Preconditions:
    ;;   index is less than or equal to the length of pattern.

    (define (string-prefix-function pattern)
      (let ((len (string-length pattern)))
        (let ((prefix-function-list
               ((ply-natural
                  (create (list 0))
                  (lambda (index prefix-lengths)
                    (prepend (next-string-prefix-length pattern
                                                        index
                                                        prefix-lengths)
                             prefix-lengths)))
                (sub1 len))))
          (pipe (sect - len <>) (sect list-ref prefix-function-list <>)))))

    ;; ===== KMP-string-stepper ===========================================
    ;; string  -> (character,   natural-number -> Boolean, natural-number)
    ;; pattern     text-element matched

    ;; The KMP-stepper procedure constructs a procedure that tests whether
    ;; text-element occurs at position matched in pattern and, if so,
    ;; whether position matched is the last position in pattern.  If both
    ;; conditions are met, the constructed procedure returns #t and the
    ;; length of the longest proper prefix of pattern that is also a suffix
    ;; of pattern; otherwise, the constructed procedure returns #f and, as
    ;; a second result, either the successor of matched (if text-element
    ;; was successfully matched to an element of pattern), or the length of
    ;; the longest proper prefix of pattern that is also a suffix of the
    ;; prefix of pattern ending just before position matched.

    ;; Preconditions:
    ;;   pattern is not empty.
    ;;   matched is less than the length of pattern.

    (define (KMP-string-stepper pattern)
      (let ((len (string-length pattern))
            (spf (string-prefix-function pattern)))
        (lambda (text-element matched)
          ((rec (matcher pattern-position)
             (if (char=? (string-ref pattern pattern-position)
                         text-element)
                 (let ((one-more (add1 pattern-position)))
                   (if (= one-more len)
                       (values #t (spf one-more))
                       (values #f one-more)))
                 (if (zero? pattern-position)
                     (values #f 0)
                     (matcher (spf pattern-position)))))
           matched))))

    ;; ===== substring-search =============================================
    ;; string, string -> list(natural-number)
    ;; pattern text

    ;; The substring-search procedure constructs a list of the positions in
    ;; text at which substrings that match pattern begin, in ascending
    ;; order.

    ;; Precondition:
    ;;   The length of pattern is less than or equal to the length of text.

    (define (substring-search pattern text)
      (let ((text-length (string-length text)))
        (if (zero? (string-length pattern))
            ((unfold-list (equal-to (add1 text-length)) identity add1) 0)
            (let ((stepper (KMP-string-stepper pattern)))
              ((rec (searcher position matched)
                 (if (= position text-length)
                     (list)
                     (receive (completed new-matched)
                              (stepper (string-ref text position) matched)
                       (let ((recursive-result (searcher (add1 position)
                                                         new-matched)))
                         (if completed
                             (prepend (- position matched)
                                      recursive-result)
                             recursive-result)))))
               0 0)))))))

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
