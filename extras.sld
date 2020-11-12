;;; Extras

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College
;;; stone@cs.grinnell.edu

;;; created August 22, 2011
;;; last revised January 11, 2017

;;; Particularly in the early chapters of _Algorithms for functional
;;; programming_, some procedures are defined primarily as illustrations
;;; and examples, not because they will subsequently be useful.  I have
;;; nevertheless collected them in this "extras" library in case some of
;;; them turn out to be useful after all (and to simplify testing).

;;; In a few cases, a procedure defined in the text duplicates one of
;;; Scheme's primitives, even using the same name.  To avoid collisions, I
;;; have renamed such procedures here.  (For instance, the length procedure
;;; from the section on lists is here called alternative-length, so as not
;;; to interfere with the operation of Scheme's length procedure.)

(define-library (afp extras)
  (export

    ;; From "Essential notations":

    meg inches-in-a-mile arithmetic-mean-long-form ratio-of-sum-to-product
    fixed-sum-times-variable-sum

    ;; From "The tool box":

    sum-of-squares sum-of-cubes alternative-sum-of-cubes alternative-invoke
    factorial-of-each transfer-unit alternative-same-in-parity?
    alternative-power-of-two? alternative-ceiling-of-log-two
    conventional-ceiling-of-log-two brute-force-greatest-common-divisor
    disparity alternative-fold-natural alternative-summation
    ceiling-of-log-two-using-tally alternative-tally

    ;; From "Data structures":

    suit? suit=? character-or-Boolean? character-or-Boolean=?
    make-confident-survey-response make-diffident-survey-response
    survey-response? survey-response=? confident? diffident?
    survey-response->Boolean negate character-and-Boolean?
    make-character-and-Boolean select-character select-Boolean
    alternative-make-confident-survey-response
    alternative-make-diffident-survey-response alternative-survey-response?
    alternative-survey-response=? alternative-confident?
    alternative-diffident? alternative-survey-response->Boolean
    alternative-negate make-new-zero make-new-successor new-natural-number?
    new-zero? new-positive? new-add1 new-sub1 new-= new-fold-natural new-+
    new-even? new-div-and-mod alternative-first list-of-numbers?
    list-of-numbers=? alternative-length length-using-fold-list
    sum-using-fold-list catenate-using-fold-list alternative-fold-list
    postpend slow-reverse alternative-reverse pairs-with-given-sum
    square-lister-with-ply-natural square-lister-with-unfold-list
    neutral-square-lister alternative-append alternative-map
    alternative-list-ref all-even? all-zeroes source-ref alternator
    disinterleave alternative-natural-number-source Fibonacci empty-source?
    alternative-make-star alternative-star-name alternative-star-magnitude
    alternative-star-distance alternative-star-spectral-class
    alternative-destar alternative-star? alternative-star=? make-star
    star-name star-magnitude star-distance star-spectral-class destar star?
    star=? brighter? filter-by-spectral-class nearest-star tree-of-numbers?
    tree-of-numbers=? alternative-list->ordered-tree list->ordered-tree
    iota-bush alternative-put-into-bag alternative-take-from-bag spectrum

    ;; From "Sorting":

    naive-binary-search-tree-invariant? heap-list-folder
    heap-list-catenator star<=? order-statistic-by-sorting

    ;; From "Combinatorial constructions":

    alternative-sublists alternative-subsequences

    ;; From "Graphs":

    alternative-related-by alternative-graph-converse slow-neighbors
    slow-spanning-tree

    ;; From "Sublist search":

    nucleotide-value dna-numeric-value dna-modular-numeric-value)

  (import (afp primitives)
          (only (afp arithmetic)
                lesser factorial add1 sub1 double halve mod divisible-by?)
          (afp constant-procedures)
          (only (afp procedure-sections) curry equal-to)
          (only (afp couplers) pipe dispatch cross)
          (only (afp adapters)
                >initial >next >all-but-initial identity converse ~initial
                ~each compare-by)
          (afp recursion-managers)
          (only (afp predicate-operations)
                ^not ^et ^vel ^if conditionally-combine)
          (only (afp natural-numbers) ply-natural lower-ply-natural tally)
          (only (afp lists)
                first rest empty-list? non-empty-list? prepend deprepend
                catenate fold-list process-list unfold-list
                extend-to-positive-arity ^and run filter drop position-in
                every)
          (only (afp sources) tap natural-number-source source-drop
                map-source unfold-source)
          (only (afp trees)
                make-empty-tree empty-tree? make-non-empty-tree
                non-empty-tree? non-empty-tree-root non-empty-tree-left
                non-empty-tree-right de-non-empty-tree unfold-tree
                for-all-in-tree?)
          (only (afp bushes) bush bush-root debush unfold-bush)
          (only (afp bags) bag debag fold-bag bag-union map-bag)
          (only (afp sets) set-of= fold-set fast-map-set exists-in-set?)
          (only (afp tables) put-into-table lookup table)
          (only (afp ordering-relations) extreme-and-others-in-list
                fixed-list-lex)
          (only (afp heaps) empty-heap merge-heaps)
          (only (afp sorting mergesort) sort)
          (only (afp products-and-selections)
                Cartesian-power suffixes prefixes
                list-of-Booleans->subsequence)
          (only (afp graphs)
                vertices arcs arc-tail arc-head same-endpoints? reverse-arc
                arcless-graph arcs-leaving relation-graph related-by
                add-labeled-edge edge-label ends edges)
          (only (afp paths) path-finder))

  (begin
    
    ;; ===== meg ==========================================================
    ;; integer

    ;; In computing, the term "meg" is often a shorthand for 2^20.

    (define meg 1048576)

    ;; ===== inches-in-a-mile =============================================
    ;; integer

    ;; Inches and miles are archaic Anglo-American units for measuring
    ;; length.  An inch is equal to 2.54 centimeters.  There are, by
    ;; definition, twelve inches in a foot and 5280 feet in a mile, so the
    ;; computation in the following definition is exact.

    (define inches-in-a-mile (* 12 5280))

    ;; ===== arithmetic-mean-long-form ====================================
    ;; number, number -> number
    ;; a       b

    ;; The arithmetic-mean-long-form procedure computes the arithmetic mean
    ;; of a and b.

    (define arithmetic-mean-long-form
      (lambda (a b)
        (/ (+ a b) 2)))

    ;; ===== ratio-of-sum-to-product ======================================
    ;; number ... -> number
    ;; numbers

    ;; The ratio-of-sum-to-product procedure computes the ratio of the sum
    ;; of the elements of numbers to the product of those elements.

    (define (ratio-of-sum-to-product . numbers)
      (/ (apply + numbers) (apply * numbers)))

    ;; ===== fixed-sum-times-variable-sum =================================
    ;; number, number, number, number ... -> number
    ;; alpha   beta    gamma   others

    ;; The fixed-sum-times-variable-sum procedure computes the product of
    ;; two sums: the sum of alpha, beta and gamma, and the sum of the
    ;; elements of others.

    (define (fixed-sum-times-variable-sum alpha beta gamma . others)
      (* (+ alpha beta gamma) (apply + others)))

    ;; ===== sum-of-squares ===============================================
    ;; number ... -> number
    ;; numbers

    ;; The sum-of-squares procedure computes the sum of the squares of the
    ;; elements of numbers.

    (define (sum-of-squares . numbers)
      (apply + (map square numbers)))

    ;; ===== sum-of-cubes =================================================
    ;; number ... -> number
    ;; numbers

    ;; The sum-of-cubes procedure computes the sum of the cubes of the
    ;; elements of numbers.

    (define (sum-of-cubes . numbers)
      (apply + (map * numbers numbers numbers)))

    ;; ===== alternative-sum-of-cubes =====================================
    ;; number ... -> number
    ;; numbers

    ;; The alternative-sum-of-cubes procedure computes the sum of the cubes
    ;; of the elements of numbers.

    (define (alternative-sum-of-cubes . numbers)
      (apply + (map (lambda (number)
                      (* number number number))
                    numbers)))

    ;; ===== alternative-invoke ===========================================
    ;; (alpha ... -> beta ...), alpha ... -> beta ...
    ;; procedure                arguments

    ;; The alternative-invoke procedure applies procedure to the elements
    ;; of arguments, returning the results.

    ;; Precondition:
    ;;   procedure can receive the elements of arguments.

    (define (alternative-invoke procedure . arguments)
      (apply procedure arguments))

    ;; ===== factorial-of-each ============================================
    ;; natural-number ... -> natural-number ...
    ;; arguments

    ;; The factorial-of-each procedure returns the factorials of the
    ;; elements of arguments.

    (define (factorial-of-each . arguments)
      (delist (map factorial arguments)))

    ;; ===== transfer-unit ================================================
    ;; number, number -> number, number
    ;; left    right

    ;; The transfer-unit procedure computes two results: the result of
    ;; subtracting 1 from left and the result of adding 1 to right.

    (define transfer-unit (cross sub1 add1))

    ;; ===== alternative-same-in-parity? ==================================
    ;; integer, integer -> Boolean
    ;; left     right

    ;; The alternative-same-in-parity? predicate determines whether the
    ;; parity of left and the parity of right are the same -- that is,
    ;; whether both are even or both are odd.

    (define alternative-same-in-parity? (pipe (~each even?) boolean=?))

    ;; ===== alternative-power-of-two? ====================================
    ;; natural-number -> Boolean
    ;; num

    ;; The alternative-power-of-two? predicate determines num is an exact
    ;; power of two.

    ;; Precondition:
    ;;   num is positive.

    (define alternative-power-of-two? (check (sect = <> 1) even? halve))

    ;; ===== alternative-ceiling-of-log-two ===============================

    ;; The alternative-ceiling-of-log-two procedure computes the ceiling of
    ;; the base-two logarithm of bound, or in other words the number of
    ;; doubling operations that must performed in succession, starting from
    ;; 1, to obtain a number that equals or exceeds bound.

    ;; Precondition:
    ;;   bound is positive.

    (define (alternative-ceiling-of-log-two bound)
      ((pipe (create 1 0)
             (pipe (iterate (pipe >initial (sect >= <> bound))
                            (cross double add1))
                   >next))))

    ;; ===== conventional-ceiling-of-log-two ==============================

    ;; The conventional-ceiling-of-log-two procedure computes the ceiling
    ;; of the base-two logarithm of bound, or in other words the number of
    ;; doubling operations that must performed in succession, starting from
    ;; 1, to obtain a number that equals or exceeds bound.

    ;; Precondition:
    ;;   bound is positive.

    (define (conventional-ceiling-of-log-two bound)
      ((rec (doubler most-recent-double count)
         (if (>= most-recent-double bound)
             count
             (doubler (double most-recent-double) (add1 count))))
       1 0))

    ;; ===== brute-force-greatest-common-divisor =========================
    ;; natural-number, natural-number -> natural-number
    ;; left            right

    ;; The brute-force-greatest-common-divisor algorithm computes the
    ;; greatest positive integer that evenly divides both left and right.

    ;; Preconditions:
    ;;   left is positive.
    ;;   right is positive.

    (define (brute-force-greatest-common-divisor left right)
      (let ((divides-both? (lambda (candidate)
                             (and (divisible-by? left candidate)
                                  (divisible-by? right candidate)))))
        ((iterate divides-both? sub1) (lesser left right))))

    ;; ===== disparity ====================================================
    ;; number, number -> number
    ;; left    right

    ;; The disparity procedure determines the amount by which left differs
    ;; from right.

    (define disparity (^if < (converse -) -))

    ;; ===== alternative-fold-natural =====================================
    ;; (-> alpha ...), (alpha ... -> alpha ...) ->
    ;; base            step
    ;;                                        (natural-number -> alpha ...)
    ;;                                         nat

    ;; The alternative-fold-natural procedure constructs a procedure that
    ;; returns the results of invoking base if nat is zero.  If nat is
    ;; non-zero, the constructed procedure applies itself recursively to
    ;; the predecessor of nat and returns the results of applying step to
    ;; the results of the recursive invocation.

    ;; Preconditions:
    ;;   step can receive the results of an invocation of base.
    ;;   step can receive the results of any invocation of step.

    (define (alternative-fold-natural base step)
      (recur zero?
             (pipe black-hole base)
             (dispatch identity sub1)
             (pipe >all-but-initial step)))

    ;; ===== alternative-summation ========================================
    ;; (integer -> number), integer, integer -> number
    ;; function             lower    upper

    ;; The alternative-summation procedure computes the sum of the results
    ;; of applying function to all of the integers from lower up to and
    ;; including upper.

    ;; Precondition:
    ;;   function function can receive any integer in the range from lower
    ;;   up to and including upper.

    (define (alternative-summation function lower-bound upper-bound)
      ((rec (summer counter)
         (if (zero? counter)
             (function lower-bound)
             (+ (function (+ counter lower-bound))
                (summer (sub1 counter)))))
       (- upper-bound lower-bound)))

    ;; ===== ceiling-of-log-two-using-tally ===============================
    ;; natural-number -> natural-number
    ;; bound

    ;; The ceiling-of-log-two-using-tally procedure computes and returns
    ;; the ceiling of the base-two logarithm of bound, or in other words
    ;; the number of doubling operations that must performed in succession,
    ;; starting from 1, to obtain a number that equals or exceeds bound.

    ;; Precondition:
    ;;   bound is positive.

    (define (ceiling-of-log-two-using-tally bound)
      ((tally (sect <= bound <>) double) 1))

    ;; ===== alternative-tally ============================================
    ;; (alpha ... -> Boolean), (alpha ... -> alpha ...) ->
    ;; final?                  step
    ;;                                        (alpha ... -> natural-number)
    ;;                                         arguments

    ;; The alternative-tally procedure constructs a procedure that returns
    ;; 0 if the elements of arguments satisfy final?; otherwise, the
    ;; constructed procedure applies step to those elements, applies itself
    ;; recursively to the results, and returns the successor of the result.

    ;; Preconditions:
    ;;   final? can receive the elements of arguments.
    ;;   final? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy final?, then step can
    ;;     receive them.
    ;;   If the results of an invocation of step do not satisfy final?,
    ;;     then step can receive them.

    (define (alternative-tally final? step)
      (build final? (constant 0) values? step (pipe >next add1)))

    ;; ===== suit? ========================================================
    ;; any       -> Boolean
    ;; something

    ;; The suit? predicate determines whether something is one of the four
    ;; symbols that designate suits in a deck of playing cards.

    (define (suit? something)
      (and (symbol? something)
           (or (symbol=? something 'clubs)
               (symbol=? something 'diamonds)
               (symbol=? something 'hearts)
               (symbol=? something 'spades))))

    ;; ===== suit=? =======================================================
    ;; suit, suit -> Boolean

    ;; This predicate determines whether two values of the suit type are
    ;; equal.

    (define suit=? symbol=?)

    ;; ===== character-or-Boolean? ========================================
    ;; any       -> Boolean
    ;; something

    ;; The character-or-Boolean? predicate determines whether something
    ;; belongs to a sum type formed from the character and Boolean types.

    (define character-or-Boolean? (^vel char? boolean?))

    ;; ===== character-or-Boolean=? =======================================
    ;; character | Boolean, character | Boolean -> Boolean
    ;; left                 right

    ;; The character-or-Boolean? predicate tests whether left and right are
    ;; equal.

    (define (character-or-Boolean=? left right)
      (or (and (char? left)
               (char? right)
               (char=? left right))
          (and (boolean? left)
               (boolean? right)
               (boolean=? left right))))

    ;; ===== make-confident-survey-response ===============================
    ;; Boolean -> survey-response
    ;; b

    ;; The make-confident-survey-response procedure constructs a value of
    ;; the survey-response type, expressing a confident response of the
    ;; polarity indicated by b.

    (define (make-confident-survey-response b)
      (if b 1 0))

    ;; ===== make-diffident-survey-response ===============================
    ;; Boolean -> survey-response
    ;; b

    ;; The make-diffident-survey-response procedure constructs a value of
    ;; the survey-response type, expressing a diffident response of the
    ;; polarity indicated by b.

    (define (make-diffident-survey-response b)
      (if b 3 2))

    ;; ===== survey-response? =============================================
    ;; any       -> Boolean
    ;; something

    ;; The survey-response? predicate determines whether something belongs
    ;; to the survey-response type, implemented as the natural numbers 0,
    ;; 1, 2, and 3.

    (define survey-response? (^et natural-number? (sect < <> 4)))

    ;; ===== survey-response=? ============================================
    ;; survey-response, survey-response -> Boolean
    ;; left             right

    ;; The survey-response=? predicate determines whether left and right
    ;; are equal (as survey responses).

    (define survey-response=? =)

    ;; ===== confident? ===================================================
    ;; survey-response -> Boolean
    ;; sr

    ;; The confident? predicate determines whether sr is one of the
    ;; "confident" survey-response values (0 or 1).

    (define confident? (sect < <> 2))

    ;; ===== diffident? ===================================================
    ;; survey-response -> Boolean
    ;; sr

    ;; The diffident? predicate determines whether sr is one of the
    ;; "diffident" survey-response values (2 or 3).

    (define diffident? (sect <= 2 <>))

    ;; ===== survey-response->Boolean =====================================
    ;; survey-response -> Boolean
    ;; sr

    ;; The survey-response->Boolean procedure projects the polarity of the
    ;; answer from sr.

    (define survey-response->Boolean odd?)

    ;; ===== negate =======================================================
    ;; survey-response -> survey-response
    ;; negand

    ;; The negate procedure constructs a survey response at the same
    ;; confidence level as negand, but of the opposite polarity.

    (define negate
      (let ((reverse-polarity (pipe survey-response->Boolean not)))
        (^if confident?
             (pipe reverse-polarity make-confident-survey-response)
             (pipe reverse-polarity make-diffident-survey-response))))

    ;; ===== character-and-Boolean? =======================================
    ;; any       -> Boolean
    ;; something

    ;; The character-and-Boolean? predicate determines whether something
    ;; belongs to a product type formed from the character and Boolean
    ;; types.

    (define character-and-Boolean? (^et pair? (^et (pipe car char?)
                                                   (pipe cdr boolean?))))

    ;; ===== make-character-and-Boolean ===================================
    ;; character, Boolean -> (character * Boolean)
    ;; ch         bl

    ;; The make-character-and-Boolean procedure constructs and returns a
    ;; data structure that comprises ch and bl.

    (define make-character-and-Boolean cons)

    ;; ===== select-character =============================================
    ;; (character * Boolean) -> character
    ;; chb

    ;; The select-character procedure extracts and returns the character
    ;; component of chb.

    (define select-character car)

    ;; ===== select-Boolean ===============================================
    ;; (character * Boolean) -> Boolean
    ;; chb

    ;; The select-Boolean procedure extracts and returns the Boolean
    ;; component of chb.

    (define select-Boolean cdr)

    ;; ===== alternative-make-confident-survey-response ===================
    ;; Boolean -> alternative-survey-response
    ;; b

    ;; The alternative-make-confident-survey-response procedure constructs
    ;; a value of the (alternative) survey-response type, expressing a
    ;; confident response of the polarity indicated by b.

    (define alternative-make-confident-survey-response
      (sect cons 'confident <>))

    ;; ===== alternative-make-diffident-survey-response ===================
    ;; Boolean -> alternative-survey-response
    ;; b

    ;; The alternative-make-diffident-survey-response procedure constructs
    ;; a value of the (alternative) survey-response type, expressing a
    ;; diffident response of the polarity indicated by b.

    (define alternative-make-diffident-survey-response
      (sect cons 'diffident <>))

    ;; ===== alternative-survey-response? =================================
    ;; any       -> Boolean
    ;; something

    ;; The alternative-survey-response? predicate determines whether
    ;; something belongs to the (alternative) survey-response type,
    ;; implemented using pairs to discriminate the source types in a
    ;; discriminated union.

    (define alternative-survey-response?
      (^et pair? (^et (pipe car (^et symbol?
                                     (^vel (sect symbol=? <> 'confident)
                                           (sect symbol=? <> 'diffident))))
                      (pipe cdr boolean?))))

    ;; ===== alternative-survey-response=? ================================
    ;; alternative-survey-response, alternative-survey-response -> Boolean
    ;; left                         right

    ;; The alternative-survey-response=? predicate determines whether left
    ;; and right are equal, as (alternative) survey responses.

    (define alternative-survey-response=? (^et (compare-by car symbol=?)
                                               (compare-by cdr boolean=?)))

    ;; ===== alternative-confident? =======================================
    ;; alternative-survey-response -> Boolean
    ;; asr

    ;; The alternative-confident? predicate determines whether asr is one
    ;; of the "confident" survey-response values.

    (define alternative-confident?
      (pipe car (sect symbol=? <> 'confident)))

    ;; ===== alternative-diffident? =======================================
    ;; alternative-survey-response -> Boolean
    ;; asr

    ;; The alternative-diffident? predicate determines whether asr is one
    ;; of the "diffident" survey-response values.

    (define alternative-diffident?
      (pipe car (sect symbol=? <> 'diffident)))

    ;; ===== alternative-survey-response->Boolean =========================
    ;; alternative-survey-response -> Boolean
    ;; asr

    ;; The alternative-survey-response->Boolean procedure projects the
    ;; polarity of the answer from asr.

    (define alternative-survey-response->Boolean cdr)

    ;; ===== alternative-negate ===========================================
    ;; alternative-survey-response -> alternative-survey-response
    ;; negand

    ;; The alternative-negate procedure constructs an (alternative) survey
    ;; response at the same confidence level as negand, but of the opposite
    ;; polarity.

    (define alternative-negate
      (let ((reverse-polarity
             (pipe alternative-survey-response->Boolean not)))
        (^if alternative-confident?
             (pipe reverse-polarity
                   alternative-make-confident-survey-response)
             (pipe reverse-polarity
                   alternative-make-diffident-survey-response))))

    ;; ===== make-new-zero ================================================
    ;; -> alternative-natural-number

    ;; The make-new-zero procedure generates a base value for an
    ;; alternative representation of natural numbers.

    (define make-new-zero (create (cons 'z 'z)))

    ;; ===== make-new-successor ===========================================
    ;; alternative-natural-number -> alternative-natural-number
    ;; num

    ;; The make-new-successor procedure constructs the successor of num.

    (define make-new-successor (sect cons 'nn <>))

    ;; ===== new-natural-number? ==========================================
    ;; any       -> Boolean
    ;; something

    ;; The new-natural-number? predicate determines whether something is an
    ;; (alternative) natural number.

    (define (new-natural-number? something)
      (and (pair? something)
           (symbol? (car something))
           (or (and (symbol=? (car something) 'z)
                    (symbol=? (cdr something) 'z))
               (and (symbol=? (car something) 'nn)
                    (new-natural-number? (cdr something))))))

    ;; ===== new-zero? ====================================================
    ;; alternative-natural-number -> Boolean
    ;; num

    ;; The new-zero? predicate determines whether num is 0 (as an
    ;; alternative natural number).

    (define new-zero? (pipe car (sect symbol=? <> 'z)))

    ;; ===== new-positive? ================================================
    ;; alternative-natural-number -> Boolean 
    ;; num

    ;; The new-positive? predicate determines whether num is positive.

    (define new-positive? (pipe car (sect symbol=? <> 'nn)))

    ;; ===== new-add1 =====================================================
    ;; alternative-natural-number -> alternative-natural-number
    ;; num

    ;; The new-add1 procedure computes the result of adding 1 to num.

    (define new-add1 make-new-successor)

    ;; ===== new-sub1 =====================================================
    ;; alternative-natural-number -> alternative-natural-number
    ;; num

    ;; The new-sub1 procedure computes the result of subtracting 1 from
    ;; num.

    ;; Precondition:
    ;;   num is positive.

    (define new-sub1 cdr)

    ;; ===== new-= ========================================================
    ;; alternative-natural-number, alternative-natural-number -> Boolean
    ;; left                        right

    ;; The new-= predicate determines whether left and right are equal, as
    ;; (alternative) natural numbers.

    (define new-= (check (^et (pipe >initial new-zero?)
                              (pipe >next new-zero?))
                         (^et (pipe >initial new-positive?)
                              (pipe >next new-positive?))
                         (~each new-sub1)))

    ;; ===== new-fold-natural =============================================
    ;; (-> alpha ...), (alpha ... -> alpha ...) ->
    ;; base            step
    ;;                            (alternative-natural-number -> alpha ...)
    ;;                             nat

    ;; The fold-natural procedure constructs a procedure that returns the
    ;; results of invoking base if nat is zero.  If nat is non-zero, the
    ;; constructed procedure applies itself recursively to the result of
    ;; subtracting 1 from nat and returns the results of applying step to
    ;; the results of the recursive invocation.

    ;; Preconditions:
    ;;   step can receive the results of an invocation of base.
    ;;   step can receive the results of any invocation of step.

    (define (new-fold-natural base step)
      (rec (natural-mapper nat)
        (if (new-zero? nat)
            (base)
            (receive recursive-results (natural-mapper (new-sub1 nat)) 
              (apply step recursive-results)))))

    ;; ===== new-+ ========================================================
    ;; alternative-natural-number, alternative-natural-number ->
    ;; augend                      addend
    ;;                                           alternative-natural-number

    ;; The new-+ procedure adds addend to augend and returns the sum.

    (define (new-+ augend addend)
      ((new-fold-natural (create augend) new-add1) addend))

    ;; ===== new-even? ====================================================
    ;; alternative-natural-number -> Boolean
    ;; num

    ;; The new-even? predicate determines whether num is even (that is,
    ;; whether 2 is a divisor of it).

    (define new-even? (new-fold-natural (create #t) not))

    ;; ===== new-div-and-mod ==============================================
    ;; alternative-natural-number, alternative-natural-number ->
    ;; dividend                    divisor
    ;;               alternative-natural-number, alternative-natural-number

    ;; The new-div-and-mod procedure performs a number-theoretic division of
    ;; divisor by dividend and returns the quotient and remainder.

    ;; Precondition:
    ;;   divisor is positive.

    (define (new-div-and-mod dividend divisor)
      ((new-fold-natural (create (make-new-zero) (make-new-zero))
                         (^if (pipe >next (pipe new-add1
                                                (sect new-= <> divisor)))
                              (cross new-add1 (constant (make-new-zero)))
                              (cross identity new-add1)))
       dividend))

    ;; ===== alternative-first ============================================
    ;; list(alpha) -> alpha
    ;; ls

    ;; The alternative-first procedure selects the initial element of a
    ;; list.

    ;; Precondition:
    ;;   ls is not empty.

    (define alternative-first (pipe delist >initial))

    ;; ===== list-of-numbers? =============================================
    ;; any       -> Boolean
    ;; something

    ;; The list-of-numbers? predicate determines whether something is a
    ;; list of which every element is a number.

    (define list-of-numbers?
      (check null? (^et pair? (pipe car number?)) cdr))

    ;; ===== list-of-numbers=? ============================================
    ;; list(number), list(number) -> Boolean
    ;; left          right

    ;; The list-of-numbers=? predicate determines whether left and right
    ;; are the same list of numbers.

    (define (list-of-numbers=? left right)
      (or (and (empty-list? left)
               (empty-list? right))
          (and (non-empty-list? left)
               (non-empty-list? right)
               (= (first left) (first right))
               (list-of-numbers=? (rest left) (rest right)))))

    ;; ===== alternative-length ===========================================
    ;; list(any) -> natural-number
    ;; ls

    ;; The alternative-length procedure computes the number of elements in
    ;; ls, counting duplicates as distinct.

    (define (alternative-length ls)
      (if (empty-list? ls)
          0
          (add1 (alternative-length (rest ls)))))

    ;; ===== length-using-fold-list =======================================
    ;; list(any) -> natural-number
    ;; ls

    ;; The length-using-fold-list procedure computes the number of elements
    ;; in ls, counting duplicates as distinct.

    (define length-using-fold-list
      (fold-list (create 0) (pipe >next add1)))

    ;; ===== sum-using-fold-list ==========================================
    ;; list(number) -> number
    ;; ls

    ;; The sum-using-fold-list procedure computes the sum of the elements of
    ;; ls.

    (define sum-using-fold-list (fold-list (create 0) +))

    ;; ===== catenate-using-fold-list =====================================
    ;; list(alpha), list(alpha) -> list(alpha)
    ;; left         right

    ;; The catenate-using-fold-list procedure constructs a list that
    ;; contains all of the elements of left, in their original order,
    ;; followed by all of the elements of right, in their original order.

    (define (catenate-using-fold-list left right)
      ((fold-list (create right) cons) left))

    ;; ===== alternative-fold-list ========================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            combiner
    ;;                                            (list(beta) -> alpha ...)
    ;;                                             ls

    ;; The alternative-fold-list procedure constructs a procedure that
    ;; returns the results of invoking base if ls is empty.  If ls is
    ;; non-empty, the constructed procedure applies itself recursively to
    ;; the rest of ls and returns the results of applying combiner to the
    ;; first of ls and the results of the recursive invocation.

    ;; Preconditions:
    ;;   combiner can receive the last element of ls and the results of an
    ;;     invocation of base.
    ;;   combiner can receive any but the last element of ls and the
    ;;     results of an invocation of combiner.

    (define (alternative-fold-list base combiner)
      (recur empty-list? (pipe black-hole base) deprepend combiner))

    ;; ===== postpend ====================================================
    ;; alpha, list(alpha) -> list(alpha)
    ;; new    ls

    ;; The postpend procedure constructs a list by adding something as the
    ;; new last element to ls.

    (define (postpend new ls)
      ((fold-list (create (list new)) prepend) ls))

    ;; ===== slow-reverse ================================================
    ;; list(alpha) -> list(alpha)
    ;; revertend

    ;; The slow-reverse procedure constructs a list containing the same
    ;; values as revertend, but in the opposite order.

    (define slow-reverse (fold-list list postpend))

    ;; ===== alternative-reverse =========================================
    ;; list(alpha) -> list(alpha)
    ;; revertend

    ;; The alternative-reverse procedure constructs a list containing the
    ;; same values as revertend, but in the opposite order.

    (define alternative-reverse (process-list list prepend))

    ;; ===== pairs-with-given-sum ========================================
    ;; natural-number -> list(pair(natural-number, natural-number))
    ;; partiend

    ;; The pairs with-given-sum procedure constructs a list of all the
    ;; pairs of natural numbers in which the sum of the components is
    ;; partiend.

    (define (pairs-with-given-sum partiend)
      ((unfold-list (pipe >initial (sect < partiend <>))
                    cons
                    (cross add1 sub1))
       0 partiend))

    ;; ===== square-lister-with-ply-natural ===============================
    ;; natural-number -> list(natural-number)
    ;; bound

    ;; The square-lister-with-ply-natural procedure constructs a list of
    ;; all the squares of positive integers that do not exceed bound.

    (define square-lister-with-ply-natural
      (ply-natural list (pipe (~initial square) prepend)))

    ;; ===== square-lister-with-unfold-list ===============================
    ;; natural-number -> list(natural-number)
    ;; bound

    ;; The square-lister-with-unfold-list procedure constructs a list of
    ;; all the squares of positive integers that do not exceed bound.

    (define square-lister-with-unfold-list (unfold-list zero? square sub1))

    ;; ===== neutral-square-lister ========================================
    ;; natural-number -> list(natural-number)
    ;; bound

    ;; The neutral-square-lister procedure constructs a list of all the
    ;; squares of positive integers that do not exceed bound.

    (define neutral-square-lister
      (rec (helper counter)
        (if (zero? counter)
            (list)
            (prepend (square counter) (helper (sub1 counter))))))

    ;; ===== alternative-append ===========================================
    ;; list(alpha) ...   -> list(alpha)
    ;; lists

    ;; The alternative-append procedure constructs a list comprising the
    ;; elements of the elements of lists, retaining both the relative order
    ;; of the elements and the relative order of the elements within each
    ;; element.

    (define alternative-append
      (pipe list (fold-list (create (list)) catenate)))

    ;; ===== alternative-map ==============================================
    ;; (alpha, beta ... -> gamma), list(alpha), list(beta) ... ->
    ;; procedure                   initial-list other-lists
    ;;                                                          list(gamma)

    ;; The alternative-map procedure applies procedure to corresponding
    ;; elements of initial-list and of the elements of other-lists,
    ;; collecting the results in a list.

    ;; Preconditions:
    ;;   procedure can receive any element of initial-list and the elements
    ;;     in corresponding positions in the elements of other-lists.
    ;;   The length of initial-list is equal to the length of each element
    ;;     of other-lists.

    (define (alternative-map procedure initial-list . other-lists)
      (if (null? initial-list)
          (list)
          (prepend (apply procedure (first initial-list)
                                    (alternative-map first other-lists))
                   (apply alternative-map procedure
                                          (rest initial-list)
                                          (alternative-map rest
                                                           other-lists)))))

    ;; ===== alternative-list-ref =========================================
    ;; list, natural-number -> any
    ;; ls    num

    ;; The alternative-list-ref procedure extracts the element at position
    ;; num in ls.

    ;; Precondition:
    ;;   num is less than the length of ls.

    (define alternative-list-ref (pipe drop first))

    ;; ===== all-even? ====================================================
    ;; list(integer) -> Boolean
    ;; ls

    ;; The all-even? predicate determines whether all of the elements of ls
    ;; are even.

    (define all-even? (check empty-list? (pipe first even?) rest))

    ;; ===== all-zeroes ===================================================
    ;; source(natural-number)

    ;; The all-zeroes source contains 0 as each of its infinitely many
    ;; elements.

    (define all-zeroes (source (values 0 all-zeroes)))

    ;; ===== source-ref ===================================================
    ;; source(alpha), natural-number -> alpha
    ;; src            num

    ;; The source-ref procedure extracts the element at position num in src.

    (define (source-ref src position)
      (receive (initial others) (tap src)
        (if (zero? position)
            initial
            (source-ref others (sub1 position)))))

    ;; ===== alternator ===================================================
    ;; source(alpha) -> source(alpha)
    ;; src

    ;; The alternator procedure constructs a source of which the elements
    ;; are the elements in even-numbered positions in src.

    (define (alternator src)
      (source (receive (initial others) (tap src)
                (receive (next still-others) (tap others)
                  (values initial (alternator still-others))))))

    ;; ===== disinterleave ================================================
    ;; source(alpha) -> source(alpha), source(alpha)
    ;; src

    ;; The disinterleave procedure constructs two sources, one containing
    ;; the elements in even-numbered positions of src, the other those in
    ;; odd-numbered positions.

    (define (disinterleave src)
      (values (alternator src)
              (source (tap (alternator (source-drop src 1))))))

    ;; ===== alternative-natural-number-source ============================
    ;; source(natural-number)

    ;; The alternative-natural-number-source source contains the natural
    ;; numbers, in ascending order, as elements.

    (define alternative-natural-number-source
      (source (values 0 (map-source add1 natural-number-source))))

    ;; ===== Fibonacci ====================================================
    ;; source(natural-number)

    ;; The Fibonacci source contains the values of the Fibonacci sequence, in
    ;; ascending order.

    (define Fibonacci
      ((unfold-source >initial (dispatch >next +)) 0 1))

    ;; ===== empty-source? ================================================
    ;; source(any) -> Boolean
    ;; src

    ;; The empty-source? predicate determines whether src is empty.

    (define empty-source? (run tap >initial end-of-source?))

    ;; ===== alternative-star =============================================

    ;; The following construction implements a tuple type, star, with
    ;; components for the name, apparent magnitude, distance (in parsecs),
    ;; and spectral class of a star.

    (define alternative-make-star (sect list <> <> <> <>))
    (define alternative-star-name (sect list-ref <> 0))
    (define alternative-star-magnitude (sect list-ref <> 1))
    (define alternative-star-distance (sect list-ref <> 2))
    (define alternative-star-spectral-class (sect list-ref <> 3))
    (define alternative-destar delist)
    (define alternative-star?
      (^and list?
            (pipe length (sect = <> 4))
            (pipe alternative-star-name string?)
            (pipe alternative-star-magnitude number?)
            (pipe alternative-star-distance number?)
            (pipe alternative-star-spectral-class symbol?)))
    (define alternative-star=?
      (^and (compare-by alternative-star-name string=?)
            (compare-by alternative-star-magnitude =)
            (compare-by alternative-star-distance =)
            (compare-by alternative-star-spectral-class symbol=?)))

    ;; ===== star =========================================================

    ;; The following construction implements a tuple type, star, with
    ;; components for the name, apparent magnitude, distance (in parsecs),
    ;; and spectral class of a star.

    (define-record-type star
      (make-star name magnitude distance spectral-class)
      proto-star?
      (name star-name)
      (magnitude star-magnitude)
      (distance star-distance)
      (spectral-class star-spectral-class))

    (define destar
      (dispatch star-name
                star-magnitude
                star-distance
                star-spectral-class))

    (define star? (^and proto-star?  
                        (pipe star-name string?)
                        (pipe star-magnitude number?)
                        (pipe star-distance number?)
                        (pipe star-spectral-class symbol?)))

    (define star=? (^and (compare-by star-name string=?)
                         (compare-by star-magnitude =)
                         (compare-by star-distance =)
                         (compare-by star-spectral-class symbol=?)))

    ;; ===== brighter? ====================================================
    ;; star, star  -> Boolean
    ;; left  right

    ;; The brighter? procedure determines whether left is brighter (i.e.,
    ;; has a lesser apparent magnitude) than right.

    (define brighter? (compare-by star-magnitude <))

    ;; ===== filter-by-spectral-class =====================================
    ;; symbol, list(star) -> list(star)
    ;; sc      catalog

    ;; The filter-by-spectral-class procedure takes a spectral class (a
    ;; symbol) and a list of stars as arguments and returns a list of the
    ;; stars from the given list that belong to that spectral class.

    (define (filter-by-spectral-class sc catalog)
      (filter (pipe star-spectral-class (sect symbol=? <> sc)) catalog))

    ;; ===== nearest-star =================================================
    ;; star,   star ... -> star
    ;; initial others

    ;; The nearest-star procedure returns whichever of initial and the
    ;; elements of others is least distant.

    (define nearest-star
      (extend-to-positive-arity (^if (compare-by star-distance <=)
                                     >initial
                                     >next)))

    ;; ===== tree-of-numbers? =============================================
    ;; any       -> Boolean
    ;; something

    ;; The tree-of-numbers? predicate determines whether something is a tree
    ;; containing only numbers.

    (define (tree-of-numbers? something)
      (or (empty-tree? something)
          (and (non-empty-tree? something)
               (number? (non-empty-tree-root something))
               (tree-of-numbers? (non-empty-tree-left something))
               (tree-of-numbers? (non-empty-tree-right something)))))

    ;; ===== tree-of-numbers=? ============================================
    ;; tree(number), tree(number) -> Boolean
    ;; left          right

    ;; The tree-of-numbers=? predicate determines whether left and right
    ;; are the same tree of numbers, that is, whether they have the same
    ;; structure and contain equal numbers at the corresponding positions.

    (define (tree-of-numbers=? left right)
      (or (and (empty-tree? left)
               (empty-tree? right))
          (and (non-empty-tree? left)
               (non-empty-tree? right)
               (= (non-empty-tree-root left)
                  (non-empty-tree-root right))
               (tree-of-numbers=? (non-empty-tree-left left)
                                  (non-empty-tree-left right))
               (tree-of-numbers=? (non-empty-tree-right left)
                                  (non-empty-tree-right right)))))

    ;; ===== alternative-list->ordered-tree ===============================
    ;; list(number) -> tree(number)
    ;; ls

    ;; The alternative-list->ordered-tree procedure constructs a tree
    ;; containing the elements of ls, with the additional constraint that,
    ;; unless the tree is empty, all of the numbers in its left subtree are
    ;; less than or equal to the number at its root, which in turn is less
    ;; than all of the numbers in its right subtree.

    (define (alternative-list->ordered-tree ls)
      (if (empty-list? ls)
          (make-empty-tree)
          (make-non-empty-tree (first ls)
            (alternative-list->ordered-tree (filter (sect <= <> (first ls))
                                                    (rest ls)))
            (alternative-list->ordered-tree (filter (sect < (first ls) <>)
                                                    (rest ls))))))

    ;; ===== list->ordered-tree ===========================================
    ;; list(number) -> tree(number)
    ;; ls

    ;; The list->ordered-tree procedure constructs a tree containing the
    ;; elements of ls, with the additional constraint that, unless the tree
    ;; is empty, all of the numbers in its left subtree are less than or
    ;; equal to the number at its root, which in turn is less than all of
    ;; the numbers in its right subtree.

    (define list->ordered-tree
      (unfold-tree empty-list?
                   first
                   (run deprepend (~initial (curry (converse <=))) filter)
                   (run deprepend (~initial (curry <)) filter)))

    ;; ===== iota-bush ====================================================
    ;; natural-number -> bush(natural-number)
    ;; num

    ;; The iota-bush procedure constructs a bush that has num as its root
    ;; and num children, each of which is itself an iota bush of the next
    ;; lesser magnitude.

    (define iota-bush
      (unfold-bush (constant #f)
                   identity
                   (lower-ply-natural black-hole values)))

    ;; ===== alternative-put-into-bag =====================================
    ;; alpha, bag(alpha) -> bag(alpha)
    ;; new    aro

    ;; The alternative-put-into-bag procedure constructs a bag containing
    ;; new, in addition to all of the values in aro.

    (define (alternative-put-into-bag new aro)
      (receive aro-contents (debag aro)
        (apply bag new aro-contents)))

    ;; ===== alternative-take-from-bag ====================================
    ;; bag(alpha) -> alpha, bag(alpha)
    ;; aro

    ;; The alternative-take-from-bag procedure returns one of the values in
    ;; aro and a bag containing all of the other values in aro.

    ;; Precondition:
    ;;   aro is not empty.

    (define alternative-take-from-bag
      (pipe debag (dispatch >initial (pipe >all-but-initial bag))))

    ;; ===== spectrum =====================================================
    ;; bag(alpha) -> table(alpha, natural-number)
    ;; aro

    ;; The spectrum procedure constructs a table in which the keys are the
    ;; values in aro and the entries are their multiplicities in aro.

    (define spectrum
      (fold-bag table (lambda (chosen subspectrum)
                        (put-into-table chosen
                                        (add1 (lookup chosen subspectrum 0))
                                        subspectrum))))

    ;; ===== naive-binary-search-tree-invariant? ==========================
    ;; (alpha, alpha -> Boolean), tree(alpha) -> Boolean
    ;; may-precede?               tree

    ;; The naive-binary-search-tree-invariant? predicate determines whether
    ;; tree satisfies the binary-search-tree invariant with respect to
    ;; may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values in tree.

    (define (naive-binary-search-tree-invariant? may-precede? tr)
      ((rec (ok? subtree)
         (or (empty-tree? subtree)
             (receive (root left right) (de-non-empty-tree subtree)
               (and (for-all-in-tree? (sect may-precede? <> root) left)
                    (for-all-in-tree? (sect may-precede? root <>) right)
                    (ok? left)
                    (ok? right)))))
       tr))

    ;; ===== heap-list-folder =============================================
    ;; (alpha, alpha -> Boolean) -> (list(bush(alpha)) -> bush(alpha))
    ;; may-precede?                  ls

    ;; The heap-list-folder procedure constructs a procedure that, in turn,
    ;; constructs a bush that satisfies the heap invariants with respect to
    ;; may-precede? and contains every element of every element of ls.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? accepts any elements of elements of ls.
    ;;   Every element of ls satisfies the heap invariants with respect to
    ;;     may-precede?.

    (define (heap-list-folder may-precede?)
      (fold-list empty-heap (merge-heaps may-precede?)))

    ;; ===== heap-list-catenator ==========================================
    ;; (alpha, alpha -> Boolean) -> (list(bush(alpha)) -> bush(alpha))
    ;; may-precede?                  ls

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? accepts any elements of elements of ls.
    ;;   No element of ls is empty.
    ;;   Every element of ls satisfies the heap invariants with respect to
    ;;     may-precede?.

    (define (heap-list-catenator may-precede?)
      (let ((extract-from-list (extreme-and-others-in-list
                                 (compare-by bush-root may-precede?))))
        (lambda (ls)
          (if (empty-list? ls)
              (empty-heap)
              (receive (leader others) (extract-from-list ls)
                (receive (new-root . new-children) (debush leader) 
                  (apply bush new-root (catenate new-children others))))))))

    ;; ===== star<=? ======================================================
    ;; star, star  -> Boolean
    ;; left  right

    ;; The star<=? predicate compares two "alternative star" tuples, first
    ;; by name, and then, as between stars with the same name, by magnitude
    ;; (placing the brighter one before the dimmer one).

    (define star<=? (fixed-list-lex string<=? <=))

    ;; ===== order-statistic-by-sorting ===================================
    ;; (alpha, alpha -> Boolean), bag(alpha), natural-number -> alpha
    ;; may-precede?               aro         index

    ;; The order-statistic-by-sorting procedure finds and returns the value
    ;; that in position index in a list of the values in aro ordered
    ;; according to may-precede?.  In other words, index values in aro bear
    ;; may-precede? to the value returned, and the value returned bears
    ;; may-precede?  to at as many values as the amount by which the
    ;; cardinality of aro exceeds index.

    (define (order-statistic-by-sorting may-precede? aro index)
      (list-ref (sort may-precede? aro) index))

    ;; ===== alternative-sublists =========================================
    ;; list(alpha) -> bag(list(alpha))
    ;; ls

    ;; The alternative-sublists procedure constructs a bag containing all
    ;; of the sublists of ls, including an empty sublist before and after
    ;; each element.

    (define alternative-sublists
      (pipe suffixes (fold-bag bag (pipe (~initial prefixes) bag-union))))

    ;; ===== alternative-subsequences =====================================
    ;; list(alpha) -> bag(list(alpha))
    ;; ls

    ;; The alternative-subsequences procedure constructs a bag containing
    ;; the subsequences of ls, that is, the lists whose elements are drawn
    ;; from ls, appearing in the same relative order as in ls.

    (define (alternative-subsequences ls)
      (map-bag (list-of-Booleans->subsequence ls)
               (Cartesian-power (list #f #t) (length ls))))

    ;; ===== alternative-related-by =======================================
    ;; graph(alpha, beta) -> (alpha, alpha -> Boolean)
    ;; graph                  tail   head

    ;; The alternative-related-by procedure constructs a predicate that
    ;; determines whether there is an arc from tail to head in graph.

    (define (alternative-related-by graph)
      (lambda (tail head)
        (exists-in-set? (^et (pipe arc-tail (equal-to tail))
                             (pipe arc-head (equal-to head)))
                        (arcs graph))))

    ;; ===== alternative-graph-converse ===================================
    ;; graph(alpha, beta) -> graph(alpha, beta)
    ;; graph

    ;; The alternative-graph-converse procedure constructs a graph similar
    ;; to graph, but with all of its arcs reversed.

    (define (alternative-graph-converse graph)
      (relation-graph (vertices graph) (converse (related-by graph))))

    ;; ===== slow-neighbors ===============================================
    ;; graph(alpha, beta) -> (alpha -> set(alpha))
    ;; graph                  tail

    ;; The slow-neighbors procedure constructs a procedure that, in turn,
    ;; constructs the set of neighbors of tail in graph -- that is, the set
    ;; of heads of arcs in graph that have tail as their tail.

    ;; Precondition:
    ;;   tail is a vertex of graph.

    (define slow-neighbors
      (pipe arcs-leaving (sect fast-map-set arc-head <>)))

    ;; ===== slow-spanning-tree ===========================================
    ;; graph(alpha, beta) -> graph(alpha, beta)
    ;; graph

    ;; The slow-spanning-tree procedure constructs a spanning tree for
    ;; graph.

    ;; Preconditions:
    ;;   graph is undirected.
    ;;   graph is path-connected.

    (define (slow-spanning-tree graph)
      ((fold-set (create (arcless-graph (vertices graph)))
                 (lambda (edge spanner)
                   (receive (end-0 end-1) (ends edge)
                     (if (list? ((path-finder spanner) end-0 end-1))
                         spanner
                         (add-labeled-edge end-0 end-1 (edge-label edge)
                                           spanner)))))
       (edges graph)))

    ;; ===== nucleotide-value =============================================
    ;; symbol     -> natural-number
    ;; nucleotide

    ;; The nucleotide-value procedure returns the numeric value arbitrarily
    ;; assigned to nucleotide.

    ;; Precondition:
    ;;   nucleotide is one of the symbols A, C, G, T.

    (define nucleotide-value
      (let ((nucleotide-list (list 'A 'C 'G 'T)))
        (sect position-in <> nucleotide-list)))

    ;; ===== dna-numeric-value ============================================
    ;; list(symbol) -> natural-number
    ;; ls

    ;; The dna-numeric-value procedure returns the numeric value of ls,
    ;; considered as a base-four numeral in which the nucleotides are the
    ;; digits.

    ;; Precondition:
    ;;   Every element of ls is one of the symbols A, C, G, T.

    (define dna-numeric-value
      (process-list (create 0)
                    (pipe (cross nucleotide-value (sect * <> 4))
                          +)))

    ;; ===== dna-modular-numeric-value ====================================
    ;; list(symbol) -> natural-number
    ;; ls

    ;; The dna-numeric-value procedure returns the numeric value of ls,
    ;; considered as a base-four numeral in which the nucleotides are the
    ;; digits, modulo a large prime (chosen so as to keep all the
    ;; computations within the range of fixnums).

    ;; Precondition:
    ;;   Every element of ls is one of the symbols A, C, G, T.

    (define dna-modular-numeric-value
      (let ((modulus 134217689))
        (process-list (create 0)
                      (run (cross nucleotide-value (sect * <> 4))
                           +
                           (sect mod <> modulus)))))))

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
