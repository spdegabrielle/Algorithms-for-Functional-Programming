;;; Products and selections

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 15, 1999
;;; last revised January 6, 2017

(define-library (afp products-and-selections)
  (export Cartesian-product Cartesian-product-source
          generalized-Cartesian-product Cartesian-power
          ordered-Cartesian-power-source Cartesian-product-ranker
          Cartesian-product-unranker generalized-Cartesian-product-ranker
          generalized-Cartesian-product-unranker suffixes
          ordered-non-empty-suffixes prefixes non-empty-prefixes sublists
          sublist-ranker sublist-unranker sections subsequences
          subsequence->list-of-Booleans list-of-Booleans->subsequence
          selections selection-rank selection-unrank subbags combinations
          combinations-source)
  (import (afp primitives)
          (only (afp arithmetic) add1 sub1 termial antitermial binomial)
          (only (afp constant-procedures) constant create)
          (only (afp procedure-sections) curry)
          (only (afp couplers) pipe cross dispatch)
          (only (afp adapters) >next identity ~initial ~next)
          (only (afp recursion-managers) build)
          (only (afp natural-numbers) fold-natural)
          (only (afp pairs) decons)
          (only (afp lists)
                first rest empty-list? non-empty-list? prepend sum
                fold-list extend-to-variable-arity run drop take
                position-in positional-weights separate-indices)
          (only (afp sources)
                tap finite-source catenate-sources map-finite-source
                ordered-prepend-each-to-each-source)
          (only (afp bags)
                bag put-into-bag take-from-bag empty-bag? fold-bag
                bag-union cons-with-each cons-with-each-source
                prepend-to-each prepend-each-to-each put-into-each-bag))
  (begin

    ;; ===== Cartesian-product ============================================
    ;; bag(alpha), bag(beta) -> bag(pair(alpha, beta))
    ;; left        right

    ;; The Cartesian-product procedure constructs a bag containing all the
    ;; pairs in which the car is a value in left and the cdr a value in
    ;; right.

    (define (Cartesian-product left right)
      ((fold-bag bag (pipe (~initial (sect cons-with-each <> right))
                           bag-union))
       left))

    ;; ===== Cartesian-product-source =====================================
    ;; bag(alpha), bag(beta) -> source(pair(alpha, beta))
    ;; left        right

    ;; The Cartesian-product-source procedure constructs a finite source
    ;; containing all the pairs in which the car is a value in left and the
    ;; cdr a value in right.

    (define (Cartesian-product-source left right)
      ((rec (folder subbag)
         (if (empty-bag? subbag)
             (finite-source)
             (source (receive (car-value others) (take-from-bag subbag)
                       (tap (catenate-sources
                              (cons-with-each-source car-value right)
                              (folder others)))))))
       left))

    ;; ===== generalized-Cartesian-product ================================
    ;; bag(alpha) ... -> bag(list(alpha))
    ;; factors

    ;; The generalized-Cartesian-product procedure constructs a bag of
    ;; lists such that the length of each list is equal to the number of
    ;; elements of factors and each element of each list is a value in the
    ;; corresponding element of factors.

    (define generalized-Cartesian-product
      (extend-to-variable-arity (bag (list)) prepend-each-to-each))

    ;; ===== Cartesian-power ==============================================
    ;; bag(alpha), natural-number -> bag(list(alpha))
    ;; aro         len

    ;; The Cartesian-power procedure constructs a bag containing every list
    ;; of length len in which the elements are all values in aro (possibly
    ;; including duplicates).

    (define (Cartesian-power aro len)
      ((fold-natural (create (bag (list)))
                     (sect prepend-each-to-each aro <>))
       len))

    ;; ===== ordered-Cartesian-power-source ===============================
    ;; list(alpha), natural-number -> source(list(alpha))
    ;; ls           len

    ;; The ordered-Cartesian-power-source constructs a finite source
    ;; containing every list of length len containing values taken from ls
    ;; (allowing duplicates).  The finite source produces these lists in
    ;; the lexicographic order induced by the order of ls.

    ;; Precondition:
    ;;   ls is ordered.

    (define (ordered-Cartesian-power-source ls len)
      ((rec (recurrer remaining)
         (if (zero? remaining)
             (finite-source (list))
             (ordered-prepend-each-to-each-source
               ls (recurrer (sub1 remaining)))))
       len))

    ;; ===== Cartesian-product-ranker =====================================
    ;; list(alpha), list(beta) -> (pair(alpha, beta) -> natural-number)
    ;; left         right          pr

    ;; The Cartesian-product-ranker procedure constructs a procedure that
    ;; computes the rank of pr in the ordered Cartesian product of left and
    ;; right.

    ;; Preconditions:
    ;;   left is ordered.
    ;;   right is ordered.
    ;;   The car of pr is an element of left.
    ;;   The cdr of pr is an element of right.

    (define (Cartesian-product-ranker left right)
      (let ((right-length (length right)))
        (run decons
             (cross (sect position-in <> left) (sect position-in <> right))
             (~initial (sect * <> right-length))
             +)))

    ;; ===== Cartesian-product-unranker ===================================
    ;; list(alpha), list(beta) -> (natural-number -> pair(alpha, beta))
    ;; left         right          rank

    ;; The Cartesian-product-unranker procedure constructs a procedure
    ;; that, in turn, constructs the pair in position rank in the ordered
    ;; Cartesian product of left and right.

    ;; Preconditions:
    ;;   left is ordered.
    ;;   right is ordered.
    ;;   rank is less than the product of the lengths of left and right.

    (define (Cartesian-product-unranker left right)
      (let ((right-length (length right)))
        (run (sect div-and-mod <> right-length)
             (cross (sect list-ref left <>) (sect list-ref right <>))
             cons)))

    ;; ===== generalized-Cartesian-product-ranker =========================
    ;; list(alpha) ... -> (list(alpha) -> natural-number)
    ;; lists               ls

    ;; The generalized-Cartesian-product-ranker procedure constructs a
    ;; procedure that computes the rank of ls in the ordered generalized
    ;; Cartesian product of the elements of lists.

    ;; Preconditions:
    ;;   Every element of lists is ordered.
    ;;   Every element of ls is an element of the corresponding element of
    ;;     lists.

    (define (generalized-Cartesian-product-ranker . lists)
      (let ((weights (positional-weights (map length (rest lists)))))
        (run (sect map position-in <> lists)
             (sect map * <> weights)
             sum)))

    ;; ===== generalized-Cartesian-product-unranker =======================
    ;; list(alpha) ... -> (natural-number -> list(alpha))
    ;; lists               rank

    ;; The generalized-Cartesian-product-unranker procedure constructs a
    ;; procedure that, in turn, constructs the list in position rank in the
    ;; generalized Cartesian product of the elements of lists.

    ;; Preconditions:
    ;;   Every element of lists is ordered.
    ;;   rank is less than the product of the lengths of the elements of
    ;;     lists.

    (define (generalized-Cartesian-product-unranker . lists)
      (let ((weights (positional-weights (map length (rest lists)))))
        (pipe (sect separate-indices <> weights)
              (sect map list-ref lists <>))))

    ;; ===== suffixes =====================================================
    ;; list(alpha) -> bag(list(alpha))
    ;; ls

    ;; The suffixes procedure constructs a bag containing all of the
    ;; suffixes of ls.

    (define suffixes (build empty-list? bag identity rest put-into-bag))

    ;; ===== ordered-non-empty-suffixes ===================================
    ;; list(alpha) -> list(list(alpha))
    ;; ls

    ;; The ordered-non-empty-suffixes procedure constructs a
    ;; lexicographically ordered list of all of the non-empty suffixes of
    ;; ls.

    ;; Precondition:
    ;;   ls is ordered.

    (define ordered-non-empty-suffixes
      (build empty-list? (constant (list)) identity rest prepend))

    ;; ===== prefixes =====================================================
    ;; list(alpha) -> bag(list(alpha))
    ;; ls

    ;; The prefixes procedure constructs a bag containing all of the
    ;; prefixes of ls.

    (define prefixes
      (fold-list (create (bag (list)))
                 (pipe prepend-to-each (sect put-into-bag (list) <>))))

    ;; ===== non-empty-prefixes ===========================================
    ;; list(alpha) -> bag(list(alpha))
    ;; ls

    ;; The prefixes procedure constructs a bag containing all of the
    ;; non-empty prefixes of ls.

    (define non-empty-prefixes
      (fold-list bag (pipe (~next (sect put-into-bag (list) <>))
                           prepend-to-each)))

    ;; ===== sublists =====================================================
    ;; list(alpha) -> bag(list(alpha))
    ;; ls

    ;; The sublists procedure constructs a bag containing all of the
    ;; non-empty sublists of ls.

    (define sublists
      (pipe suffixes
            (fold-bag bag (pipe (~initial non-empty-prefixes) bag-union))))

    ;; ===== sublist-ranker ===============================================
    ;; list(alpha) -> (list(alpha) -> natural-number)
    ;; ls              subls

    ;; The sublist-ranker procedure constructs a procedure that computes
    ;; the rank of subls in the lexicographically ordered list of non-empty
    ;; sublists of ls.

    ;; Preconditions:
    ;;   ls is ordered.
    ;;   subls is a sublist of ls.

    (define (sublist-ranker ls)
      (let ((len (length ls)))
        (let ((number-of-sublists (termial len)))
          (pipe (dispatch (run first
                               (sect position-in <> ls)
                               (sect - len <>)
                               termial
                               (sect - number-of-sublists <>))
                          (pipe length sub1))
                +))))

    ;; ===== sublist-unranker =============================================
    ;; list(alpha) -> (natural-number -> list(alpha))
    ;; ls              rank

    ;; The sublist-unranker procedure constructs a procedure that determines
    ;; which of the sublists of ls is in position rank in the lexicographic
    ;; ordering of those sublists.

    ;; Preconditions:
    ;;   ls is ordered.
    ;;   rank is less than the termial of the length of ls.

    (define (sublist-unranker ls)
      (let ((len (length ls)))
        (let ((number-of-sublists (termial len)))
          (lambda (rank)
            (receive (position-from-end delends)
                     (antitermial (- (sub1 number-of-sublists) rank))
              (take (drop ls (- (sub1 len) position-from-end))
                    (- (add1 position-from-end) delends)))))))

    ;; ===== sections =====================================================
    ;; list(alpha), natural-number -> bag(list(alpha))
    ;; ls           len

    ;; The sections procedure constructs a bag containing the sublists of
    ;; ls of length len.

    ;; Precondition:
    ;;   len is less than or equal to the length of ls.

    (define (sections ls len)
      ((rec (sectioner subls count)
         (if (zero? count)
             (bag subls)
             (put-into-bag (take subls len)
                           (sectioner (rest subls) (sub1 count)))))
       ls (- (length ls) len)))

    ;; ===== subsequences =================================================
    ;; list(alpha) -> bag(list(alpha))
    ;; ls

    ;; The subsequences procedure constructs a bag containing the
    ;; subsequences of ls, that is, the lists whose elements are drawn from
    ;; ls, appearing in the same relative order as in ls.

    (define subsequences (fold-list (create (bag (list)))
                                    (pipe (dispatch >next prepend-to-each)
                                          bag-union)))

    ;; ===== subsequence->list-of-Booleans ================================
    ;; list(alpha) -> (list(alpha) -> list(Boolean))
    ;; ls              subseq

    ;; The subsequence->list-of-Booleans constructs a procedure that, in
    ;; turn, constructs the list of Boolean values that corresponds to
    ;; subseq, indicating at each position whether the element at that
    ;; position in ls is included in subseq.

    ;; Precondition:
    ;;   subseq is a subsequence of ls.

    (define subsequence->list-of-Booleans
      (curry (rec (constructor ls subseq)
               (if (empty-list? ls)
                   (list)
                   (if (and (non-empty-list? subseq)
                            (equal? (first ls) (first subseq)))
                       (prepend #t (constructor (rest ls) (rest subseq)))
                       (prepend #f (constructor (rest ls) subseq)))))))

    ;; ===== list-of-Booleans->subsequence ================================
    ;; list(alpha) -> (list(Boolean) -> list(alpha))
    ;; ls              bools

    ;; The list-of-Booleans->subsequence constructs a procedure that, in
    ;; turn, constructs the subsequence of ls to which bools corresponds,
    ;; each element of bools indicating whether the element at the
    ;; corresponding position in ls is included in the subsequence.

    ;; Precondition:
    ;;   The length of ls is equal to the length of bools.

    (define list-of-Booleans->subsequence

      (curry (rec (constructor ls bools)
               (if (empty-list? ls)
                   (list)
                   (let ((recursive-result (constructor (rest ls)
                                                        (rest bools))))
                     (if (first bools)
                         (prepend (first ls) recursive-result)
                         recursive-result))))))

    ;; ===== selections ===================================================
    ;; list(alpha), natural-number -> bag(list(alpha))
    ;; ls           len

    ;; The selections procedure constructs a bag containing all of the
    ;; subsequences of ls of length len.

    ;; Preconditions:
    ;;   len is less than or equal to the length of ls.

    (define (selections ls len)
      (if (zero? len)
          (bag (list))
          (if (empty-list? ls)
              (bag)
              (bag-union (prepend-to-each (first ls)
                                          (selections (rest ls)
                                                      (sub1 len)))
                         (selections (rest ls) len)))))

    ;; ===== selection-rank ===============================================
    ;; list(alpha), natural-number, list(alpha) -> natural-number
    ;; ls           len             selection

    ;; The selection-rank procedure computes the rank of selection in the
    ;; lexicographic ordering all selections of length len from ls.

    ;; Preconditions:
    ;;   len is less than or equal to the length of ls.
    ;;   selection is a selection from ls, and its length is len.

    (define (selection-rank ls len selection)
      (if (zero? len)
          0
          (if (equal? (first selection) (first ls))
              (selection-rank (rest ls) (sub1 len) (rest selection))
              (+ (binomial (length (rest ls)) (sub1 len))
                 (selection-rank (rest ls) len selection)))))

    ;; ===== selection-unrank =============================================
    ;; list(alpha), natural-number, natural-number -> list(alpha)
    ;; ls           len             rank

    ;; The selection-unrank procedure constructs the selection that is in
    ;; position rank in the lexicographic ordering of all selections of ls
    ;; of length len.

    ;; Preconditions:
    ;;   len is less than or equal to the length of ls.
    ;;   rank is less than or equal to the binomial coefficient of the
    ;;     length of ls and len.

    (define (selection-unrank ls len rank)
      (if (zero? len)
          (list)
          (let ((count (binomial (length (rest ls)) (sub1 len))))
            (if (< rank count)
                (prepend (first ls)
                         (selection-unrank (rest ls) (sub1 len) rank))
                (selection-unrank (rest ls) len (- rank count))))))

    ;; ===== subbags ======================================================
    ;; bag(alpha) -> bag(bag(alpha))
    ;; aro

    ;; The subbags procedure constructs a bag containing all of the bags
    ;; that can be formed from values in aro.

    (define subbags
      (fold-bag (create (bag (bag)))
                (pipe (dispatch >next put-into-each-bag) bag-union)))

    ;; ===== combinations =================================================
    ;; bag(alpha), natural-number -> bag(bag(alpha))
    ;; aro         size

    ;; The combinations procedure constructs a bag containing all of the
    ;; bags of cardinality size that can be formed from values in aro.

    ;; Precondition:
    ;;   size is less than or equal to the cardinality of aro.

    (define (combinations aro size)
      (if (zero? size)
          (bag (bag))
          (if (empty-bag? aro)
              (bag)
              (receive (chosen others) (take-from-bag aro)
                (bag-union (put-into-each-bag chosen
                                              (combinations others
                                                            (sub1 size)))
                           (combinations others size))))))

    ;; ===== combinations-source ==========================================
    ;; bag(alpha), natural-number -> source(bag(alpha))
    ;; aro         size

    ;; The combinations-source procedure constructs a source containing all
    ;; of the bags of cardinality size that can be formed from values in
    ;; aro.

    ;; Precondition:
    ;;   size is less than or equal to the cardinality of aro.

    (define (combinations-source aro size)
      (if (zero? size)
          (finite-source (bag))
          (if (empty-bag? aro)
              (finite-source)
              (source (receive (chosen others) (take-from-bag aro)
                        (tap (catenate-sources
                               (map-finite-source
                                 (sect put-into-bag chosen <>)
                                 (combinations-source others (sub1 size)))
                               (combinations-source others size))))))))))

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
