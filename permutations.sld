;;; Permutations

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 24, 1999
;;; last revised January 6, 2017

(define-library (afp permutations)
  (export permutations ordered-permutations
          ordered-permutations-source permutation-rank permutation-unrank)
  (import (afp primitives)
          (only (afp arithmetic) add1 sub1 factorial)
          (only (afp procedure-sections) equal-to)
          (only (afp lists)
                first rest empty-list? prepend catenate position-in
                all-but-position)
          (only (afp sources)
                tap finite-source catenate-sources
                ordered-prepend-to-each-source)
          (only (afp bags)
                bag empty-bag? extract-from-bag fold-bag bag-union
                prepend-to-each))
  (begin

    ;; ===== permutations =================================================
    ;; bag(alpha) -> bag(list(alpha))
    ;; aro

    ;; The permutations procedure constructs a bag containing every list
    ;; that can be formed from the values in aro, using each value exactly
    ;; once.

    (define (permutations aro)
      (if (empty-bag? aro)
          (bag (list))
          ((fold-bag bag
                     (lambda (chosen recursive-result)
                       (receive (ignored others)
                                (extract-from-bag (equal-to chosen) aro)
                         (bag-union (prepend-to-each chosen
                                                     (permutations others))
                                    recursive-result))))
           aro)))

    ;; ===== ordered-permutations =========================================
    ;; list(alpha) -> list(list(alpha))
    ;; ls

    ;; The ordered-permutations procedure constructs a list containing all
    ;; the permutations of ls, in lexicographic order.

    ;; Precondition:
    ;;   ls is ordered.

    (define (ordered-permutations ls)
      (let ((len (length ls)))
        (if (zero? len)
            (list (list))
            ((rec (recurrer position)
               (if (= position len)
                   (list)
                   (catenate
                     (prepend-to-each (list-ref ls position)
                                      (ordered-permutations
                                        (all-but-position ls position)))
                     (recurrer (add1 position)))))
             0))))

    ;; ===== ordered-permutations-source ==================================
    ;; list(alpha) -> source(list(alpha))
    ;; ls

    ;; The ordered-permutations-source constructs a finite source
    ;; containing all the permutations of ls, in lexicographic order.

    (define (ordered-permutations-source ls)
      (let ((len (length ls)))
        (if (zero? len)
            (finite-source (list))
            ((rec (recurrer position)
               (if (= position len)
                   (finite-source)
                   (source
                     (let ((prepender
                             (ordered-prepend-to-each-source
                               (list-ref ls position)
                               (ordered-permutations-source
                                 (all-but-position ls position)))))
                       (tap (catenate-sources
                              prepender
                              (recurrer (add1 position))))))))
             0))))

    ;; ===== permutation-rank =============================================
    ;; list(alpha), list(alpha) -> natural-number
    ;; ls           perm

    ;; The permutation-rank procedure computes the rank of perm in the
    ;; lexicographic ordering of the permutations of ls.

    ;; Preconditions:
    ;;   ls is ordered.
    ;;   perm is a permutation of ls.

    (define (permutation-rank ls perm)
      (if (empty-list? ls)
          0
          (let ((position (position-in (first perm) ls)))
            (+ (* position (factorial (sub1 (length ls))))
               (permutation-rank (all-but-position ls position)
                                 (rest perm))))))

    ;; ===== permutation-unrank ===========================================
    ;; list(alpha), natural-number -> list(alpha)
    ;; ls           rank

    ;; The permutation-unrank procedure constructs the permutation that is
    ;; in position rank in the lexicographic ordering of the permutations
    ;; of ls.

    ;; Preconditions:
    ;;   ls is ordered.
    ;;   rank is less than the factorial of the length of ls.

    (define (permutation-unrank ls rank)
      (if (empty-list? ls)
          (list)
          (receive (position subrank)
                   (div-and-mod rank (factorial (sub1 (length ls))))
            (prepend (list-ref ls position)
                     (permutation-unrank (all-but-position ls position)
                                         subrank)))))))

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
