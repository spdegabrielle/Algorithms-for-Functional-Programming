;;; Lists

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created July 31, 1998
;;; last revised January 9, 2017

(define-library (afp lists)
  (export first rest empty-list? non-empty-list? prepend deprepend list=?
          list-of list-of= sum catenate fold-list process-list unfold-list
          adjacent-pairs conditionally-unfold-list ^and
          extend-to-variable-arity ^or run extend-to-positive-arity
          extend-like-subtraction filter remp partition extract drop take
          sublist adapter for-all? exists? every at-least-one transpose zip
          unzip collect-map dispatch-return-all cross-return-all all-alike
          all-different position-in positional-weights separate-indices
          all-but-position matches-prefix?)
  (import (afp primitives)
          (only (afp arithmetic) add1 sub1)
          (only (afp procedure-sections) curry)
          (only (afp constant-procedures) values? constant create)
          (only (afp couplers) pipe dispatch cross)
          (only (afp adapters)
                >initial >next >all-but-initial converse ~initial ~each)
          (afp recursion-managers)
          (only (afp predicate-operations)
                ^not ^et ^vel ^if conditionally-combine)
          (only (afp natural-numbers) fold-natural)
          (only (afp pairs) decons)
          (only (afp boxes) box box? debox))
  (begin

    ;; ===== first ========================================================
    ;; list(alpha) -> alpha
    ;; ls

    ;; The first procedure selects the initial element of ls.

    ;; Precondition:
    ;;   ls is not empty.

    (define first car)

    ;; ===== rest =========================================================
    ;; list(alpha) -> list(alpha)
    ;; ls

    ;; The rest procedure return a list similar to ls, but lacking
    ;; its initial element.

    ;; Precondition:
    ;;   ls is not empty.

    (define rest cdr)

    ;; ===== empty-list? ==================================================
    ;; list(any) -> Boolean
    ;; ls

    ;; The empty-list? predicate determines whether ls is empty (that is,
    ;; whether it has no elements).

    (define empty-list? null?)

    ;; ===== non-empty-list? ==============================================
    ;; list(any) -> Boolean
    ;; ls

    ;; The non-empty-list? predicate determines whether ls is non-empty
    ;; (that is, whether it has at least one element).

    (define non-empty-list? pair?)

    ;; ===== prepend ======================================================
    ;; alpha,    list(alpha) -> list(alpha)
    ;; something ls

    ;; The prepend procedure constructs a list with something as its
    ;; initial element and the elements of ls, in order, as its remaining
    ;; elements.

    (define prepend cons)

    ;; ===== deprepend ====================================================
    ;; list(alpha) -> alpha, list(alpha)
    ;; ls

    ;; The deprepend procedure returns the initial element of ls and a list
    ;; similar to ls, but lacking its initial element.

    ;; Precondition:
    ;;   ls is not empty.

    (define deprepend decons)

    ;; ===== list=? =======================================================
    ;; list(any), list(any) -> Boolean
    ;; left       right

    ;; The list=? procedure determines whether the elements of left and
    ;; right are equal in number and whether corresponding elements are the
    ;; same.

    (define (list=? left right)
      (or (and (empty-list? left)
               (empty-list? right))
          (and (non-empty-list? left)
               (non-empty-list? right)
               (equal? (first left) (first right))
               (list=? (rest left) (rest right)))))

    ;; ===== list-of ======================================================
    ;; (any -> Boolean)       -> (any       -> Boolean)
    ;; right-type-of-element?     something

    ;; The list-of procedure constructs a predicate that determines whether
    ;; something is a list, every element of which satisfies
    ;; right-type-of-element?.

    ;; Precondition:
    ;;   right-type-of-element? can receive any value.

    (define (list-of right-type-of-element?)
      (check null? (^et pair? (pipe car right-type-of-element?)) cdr)) 

    ;; ===== list-of= =====================================================
    ;; (alpha, beta -> Boolean) -> (list(alpha), list(beta) -> Boolean)
    ;; element=?                    left         right

    ;; The list-of= procedure constructs a predicate that determines
    ;; whether the elements of left and right are equal in number and
    ;; whether corresponding elements satisfy element=?.

    ;; Precondition:
    ;;   element=? can receive any element of left and any element of
    ;;   right.

    (define (list-of= element=?)
      (rec (equivalent? left right)
        (or (and (empty-list? left)
                 (empty-list? right))
            (and (non-empty-list? left)
                 (non-empty-list? right)
                 (element=? (first left) (first right))
                 (equivalent? (rest left) (rest right))))))

    ;; ===== sum ==========================================================
    ;; list(number) -> number
    ;; ls

    ;; The sum procedure computes the sum of the elements of ls.

    (define (sum ls)
      (if (empty-list? ls)
          0
          (+ (first ls) (sum (rest ls)))))

    ;; ===== catenate =====================================================
    ;; list(alpha), list(alpha) -> list(alpha)
    ;; left         right

    ;; The catenate procedure constructs a list that contains all of the
    ;; elements of left, in their original order, followed by all of the
    ;; elements of right, in their original order.

    (define (catenate left right)
      (if (empty-list? left)
          right
          (prepend (first left) (catenate (rest left) right))))

    ;; ===== fold-list ====================================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            combiner
    ;;                                            (list(beta) -> alpha ...)
    ;;                                             ls

    ;; The fold-list procedure constructs a procedure that returns the
    ;; results of invoking base if ls is empty.  If ls is non-empty, the
    ;; constructed procedure applies itself recursively to the rest of ls
    ;; and returns the results of applying combiner to the first of ls and
    ;; the results of the recursive invocation.

    ;; Preconditions:
    ;;   If ls is non-empty, then combiner can receive the last element of
    ;;     ls and the results of an invocation of base.
    ;;   If ls is non-empty, then combiner can receive any but the last
    ;;     element of ls and the results of an invocation of combiner.

    (define (fold-list base combiner)
      (rec (folder ls)
        (if (empty-list? ls)
            (base)
            (receive recursive-results (folder (rest ls))
              (apply combiner (first ls) recursive-results)))))

    ;; ===== process-list =================================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            combiner
    ;;                                            (list(beta) -> alpha ...)
    ;;                                             ls

    ;; The process-list procedure constructs a procedure that iteratively
    ;; applies combiner to an element of ls and the results of the previous
    ;; iteration (or to the results of invoking base, if there was no
    ;; previous iteration).  The constructed procedure returns the results
    ;; of the last application of combiner.

    ;; Preconditions:
    ;;   If ls is non-empty, then combiner can receive the initial element
    ;;     of ls and the results of an invocation of base.
    ;;   If ls is non-empty, combiner can receive any but the initial
    ;;     element of ls and the results of an invocation of combiner.

    (define (process-list base combiner)
      (pipe (lambda (ls)
              (receive starters (base)
                (apply values ls starters)))
            (pipe (iterate (pipe >initial empty-list?)
                           (lambda (sublist . results-so-far)
                             (receive new-results
                                     (apply combiner (first sublist)
                                                     results-so-far)
                               (apply values (rest sublist) new-results))))
                  >all-but-initial)))

    ;; ===== unfold-list ==================================================
    ;; (alpha ... -> Boolean), (alpha ... -> beta),
    ;; final?                  producer
    ;;                (alpha ... -> alpha ...) -> (alpha ... -> list(beta))
    ;;                step                         arguments

    ;; The unfold-list procedure constructs a procedure that first
    ;; determines whether the elements of arguments satisfy final?.  If so,
    ;; the constructed procedure returns the empty list.  Otherwise, it
    ;; returns a non-empty list in which the initial element is the result
    ;; of applying producer to the elements of arguments, and the remainder
    ;; of the list is the result of first applying step to the elements of
    ;; arguments and then applying the constructed procedure recursively to
    ;; the results.

    ;; Preconditions:
    ;;   final? can receive the elements of arguments.
    ;;   final? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy final?, then producer
    ;;     can receive them.
    ;;   If the results of an invocation of step do not satisfy final?,
    ;;     then producer can receive them.
    ;;   If the elements of arguments do not satisfy final?, then step can
    ;;     receive them.
    ;;   If the results of an invocation of step do not satisfy final?,
    ;;     then step can receive them.

    (define (unfold-list final? producer step)
      (build final? (constant (list)) producer step prepend))

    ;; ===== adjacent-pairs ===============================================
    ;; list(alpha) -> list(pair(alpha, alpha))
    ;; ls

    ;; The adjacent-pairs procedure constructs a list containing pairs of
    ;; adjacent elements of ls.

    ;; Precondition:
    ;;   ls is not empty.

    (define adjacent-pairs
      (unfold-list (pipe rest empty-list?)
                   (pipe (dispatch first (pipe rest first)) cons)
                   cdr))

    ;; ===== conditionally-unfold-list ====================================
    ;; (alpha ... -> Boolean), (alpha ... -> beta), (beta -> Boolean),
    ;; final?                  producer             condition-met?
    ;;                (alpha ... -> alpha ...) -> (alpha ... -> list(beta))
    ;;                step                         arguments

    ;; The conditionally-unfold-list procedure constructs a procedure that
    ;; first determines whether the elements of arguments satisfy final?.
    ;; If so, the constructed procedure returns the empty list.  Otherwise,
    ;; it applies producer to those arguments and determines whether the
    ;; result satisfies condition-met?.  If so, the constructed procedure
    ;; returns a non-empty list in which the initial element is the result
    ;; of the invocation of producer, and the remainder of the list is the
    ;; result of first applying step to the elements of arguments and then
    ;; applying the constructed procedure recursively to the results.  If
    ;; the result of the invocation of producer does not satisfy
    ;; condition-met?, then the constructed procedure simply returns the
    ;; result of the recursive invocation described above.

    ;; Preconditions:
    ;;   final? can receive the elements of arguments.
    ;;   final? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy final?, then producer
    ;;     can receive them.
    ;;   If the results of an invocation of step do not satisfy final?,
    ;;     then producer can receive them.
    ;;   condition-met? can receive the result of any invocation of
    ;;     producer.
    ;;   If the elements of arguments do not satisfy final?, then step can
    ;;     receive them.
    ;;   If the results of an invocation of step do not satisfy final?,
    ;;     then step can receive them.

    (define (conditionally-unfold-list final? condition-met? producer step)
      (build final?
             (constant (list))
             (^if condition-met? (pipe producer box) (constant #f))
             step
             (conditionally-combine box? (pipe (~initial debox) prepend))))

    ;; ===== ^and =========================================================
    ;; (alpha ... -> Boolean) ... -> (alpha ... -> Boolean)
    ;; predicates                     arguments

    ;; The ^and procedure constructs a predicate that determines whether
    ;; the elements of arguments satisfy every element of predicates.

    ;; Precondition:
    ;;   Every element of predicates can receive the elements of arguments.

    (define ^and (pipe list (fold-list (create values?) ^et)))

    ;; ===== extend-to-variable-arity =====================================
    ;; alpha, (beta, alpha -> alpha) -> (beta ...  -> alpha)
    ;; id     combiner                   arguments

    ;; The extend-to-variable-arity procedure constructs a procedure of
    ;; variable arity that returns id if arguments has no elements and
    ;; otherwise returns the result of applying combiner to the initial
    ;; element of arguments and the result of applying itself recursively
    ;; to the remaining elements of arguments.

    ;; Precondition:
    ;;   combiner can receive any element of arguments as its first
    ;;     argument.
    ;;   combiner can receive id as its second argument.
    ;;   combiner can receive any result of combiner as its second
    ;;     argument.

    (define (extend-to-variable-arity id combiner)
      (pipe list (fold-list (create id) combiner)))

    ;; ===== ^or ==========================================================
    ;; (alpha ... -> Boolean) ... -> (alpha ... -> Boolean)
    ;; predicates                     arguments

    ;; The ^or procedure constructs a predicate that determines whether the
    ;; elements of arguments satisfy at least one element of predicates.

    ;; Precondition:
    ;;   Every element of predicates can receive the elements of arguments.

    (define ^or (extend-to-variable-arity (constant #f) ^vel))

    ;; ===== run ==========================================================
    ;; (alpha ... -> alpha ...) ... -> (alpha ... -> alpha ...)
    ;; sequence                         arguments

    ;; The run procedure constructs a procedure that returns the elements
    ;; of arguments if sequence has no elements, and otherwise applies
    ;; itself recursively to all but the initial element of sequence,
    ;; applies the procedure resulting from that recursive invocation to
    ;; the results of applying the initial element of sequence to the
    ;; elements of arguments, and returns the results of that final
    ;; invocation.

    ;; Preconditions:
    ;;   If sequence has any elements, then its initial element can receive
    ;;     the elements of arguments.
    ;;   If sequence has any elements, then every element of sequence
    ;;     except the initial one can receive the results of any invocation
    ;;     of the preceding element of sequence.

    (define run (extend-to-variable-arity values pipe))

    ;; ===== extend-to-positive-arity =====================================
    ;; (alpha, alpha -> alpha) -> (alpha,  alpha ... -> alpha)
    ;; combiner                    initial others

    ;; The extend-to-positive-arity procedure constructs a procedure that
    ;; returns initial if given one argument and otherwise returns the
    ;; result of applying combiner to initial and the results of applying
    ;; itself recursively to others.

    ;; Preconditions:
    ;;   If others is not empty, then combiner can receive initial as its
    ;;     first argument.
    ;;   If others is not empty, then combiner can receive any element of
    ;;     others except the last as its first argument.
    ;;   If others is not empty, then combiner can receive the last element
    ;;     of others as its second argument.
    ;;   combiner can receive the result of any invocation of combiner as
    ;;     its second argument.

    (define (extend-to-positive-arity combiner)
      (pipe list (recur (pipe rest empty-list?) first deprepend combiner)))

    ;; ===== extend-like-subtraction ======================================
    ;; (alpha -> alpha), (alpha, beta -> alpha) ->
    ;; unary             binary
    ;;                                          (alpha,  beta ... -> alpha)
    ;;                                           initial others

    ;; The extend-like-subtraction procedure constructs a procedure that,
    ;; given one argument, applies unary to that argument and returns the
    ;; result; given two or more arguments, the constructed procedure
    ;; applies binary iteratively, operating at each step on the result of
    ;; the previous iteration (or on initial, if there was no previous
    ;; iteration) and the next element of others.

    ;; Preconditions:
    ;;   If others has no elements, then unary can receive initial.
    ;;   If others has at least one element, then binary can receive
    ;;     initial as its first argument.
    ;;   If others has at least one element, then binary can receive the
    ;;     result of any invocation of binary as its first argument.
    ;;   If others has at least one element, then binary can receive any
    ;;     element of others as its second argument.

    (define (extend-like-subtraction unary binary)
      (lambda (initial . others)
        (if (empty-list? others)
            (unary initial)
            ((process-list (create initial) (converse binary)) others))))

    ;; ===== filter =======================================================
    ;; (alpha -> Boolean), list(alpha) -> list(alpha)
    ;; keep?               ls

    ;; The filter procedure constructs a list comprising the elements of ls
    ;; that satisfy keep?.

    ;; Precondition:
    ;;   keep? can receive any element of ls.

    (define (filter keep? ls)
      ((fold-list list (conditionally-combine keep? prepend)) ls))

    ;; ===== remp =========================================================
    ;; (alpha -> Boolean), list(alpha) -> list(alpha)
    ;; exclude?            ls

    ;; The remp procedure constructs a list comprising the elements of ls
    ;; that do not satisfy exclude?.

    ;; Precondition:
    ;;   exclude? can receive any element of ls.

    (define remp (pipe (~initial ^not) filter))

    ;; ===== partition ====================================================
    ;; (alpha -> Boolean), list(alpha) -> list(alpha), list(alpha)
    ;; condition-met?      ls

    ;; The partition procedure constructs two lists, the first comprising
    ;; the elements of ls that satisfy condition-met?, the second
    ;; comprising the elements of ls that do not satisfy condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any element of ls.

    (define (partition condition-met? ls)
      ((fold-list (create (list) (list))
                  (lambda (candidate ins outs)
                    (if (condition-met? candidate)
                        (values (prepend candidate ins) outs)
                        (values ins (prepend candidate outs)))))
       ls))

    ;; ===== extract ======================================================
    ;; (alpha -> Boolean), list(alpha) ->
    ;; condition-met?      ls
    ;;                                  (box(alpha) | Boolean), list(alpha)

    ;; The extract procedure searches ls for an element that satisfies
    ;; condition-met?.  If it finds one, it returns a box containing that
    ;; element and a list of the other elements of ls; otherwise, it
    ;; returns #f and ls.

    ;; Precondition:
    ;;   condition-met? can receive any element of ls.

    (define (extract condition-met? ls)
      ((rec (extracter sublist)
         (if (empty-list? sublist)
             (values #f sublist)
             (receive (chosen others) (deprepend sublist)
               (if (condition-met? chosen)
                   (values (box chosen) others)
                   (receive (sought relicts) (extracter others)
                     (values sought (cons chosen relicts)))))))
       ls))

    ;; ===== drop =========================================================
    ;; list(alpha), natural-number -> list(alpha)
    ;; ls           count

    ;; The drop procedure returns a list similar to ls, but with the first
    ;; count elements removed.

    ;; Precondition:
    ;;   count is less than or equal to the length of ls.

    (define (drop ls count)
      ((fold-natural (create ls) rest) count))

    ;; ===== take =========================================================
    ;; list(alpha), natural-number -> list(alpha)
    ;; ls           count

    ;; The take procedure constructs a list comprising count elements from
    ;; the beginning of ls.

    ;; Precondition:
    ;;   count is less than or equal to the length of ls.

    (define take (unfold-list (pipe >next zero?)
                              (pipe >initial first)
                              (cross rest sub1)))

    ;; ===== sublist ======================================================
    ;; list(alpha), natural-number, natural-number -> list(alpha)
    ;; ls           start           finish

    ;; The sublist procedure returns the sublist of ls that starts at the
    ;; (zero-based) position specified by start and leaves off before the
    ;; position specified by finish.

    ;; Preconditions:
    ;;   start is less than or equal to finish.
    ;;   finish is less than or equal to the length of ls.

    (define (sublist ls start finish)
      (take (drop ls start) (- finish start)))

    ;; ===== adapter ======================================================
    ;; natural-number ... -> (alpha ...   -> alpha ...)
    ;; positions              arguments

    ;; The adapter procedure constructs an adapter procedure that returns
    ;; the values that appear in the (zero-based) positions of arguments
    ;; specified by the elements of positions.

    ;; Precondition:
    ;;   Every element of positions is less than the number of elements of
    ;;   arguments.

    (define (adapter . positions)
      (lambda arguments
        (delist (map (sect list-ref arguments <>) positions))))

    ;; ===== for-all? =====================================================
    ;; (alpha -> Boolean), list(alpha) -> Boolean
    ;; condition-met?      ls

    ;; The for-all? predicate determines whether all of the elements of ls
    ;; satisfy condition-met?.

    (define (for-all? condition-met? ls)
      ((check empty-list? (pipe first condition-met?) rest) ls))

    ;; ===== exists? ======================================================
    ;; (alpha -> Boolean), list(alpha) -> Boolean
    ;; condition-met?      ls

    ;; The exists? predicate determines whether at least one element of ls
    ;; satisfies condition-met?.

    (define exists? (pipe (~initial ^not) (pipe for-all? not)))

    ;; ===== every ========================================================
    ;; (alpha -> Boolean) -> (alpha ... -> Boolean)
    ;; condition-met?         arguments

    ;; The every procedure constructs a predicate that determines whether
    ;; every element of arguments satisfies condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any element of arguments.

    (define (every condition-met?)
      (lambda arguments
        (for-all? condition-met? arguments)))

    ;; ===== at-least-one =================================================
    ;; (alpha -> Boolean) -> (alpha ... -> Boolean)
    ;; condition-met?         arguments

    ;; The at-least-one procedure constructs a predicate that determines
    ;; whether at least one element of arguments satisfies condition-met?.

    ;; Precondition:
    ;;   condition-met? can receive any element of arguments.

    (define (at-least-one condition-met?)
      (lambda arguments
        (exists? condition-met? arguments)))

;; ===== transpose ====================================================
    ;; list(list(alpha)) -> list(list(alpha))
    ;; ls

    ;; The transpose procedure constructs the transpose of ls, that is, the
    ;; list of lists of elements in corresponding positions in the elements
    ;; of ls.

    ;; Preconditions:
    ;;   ls is not empty.
    ;;   The lengths of the elements of ls are equal.

    (define transpose (sect apply map list <>))

    ;; ===== zip ==========================================================
    ;; list(alpha), list(alpha) ... -> list(list(alpha))
    ;; initial      others

    ;; The zip procedure constructs a list of lists, each comprising
    ;; corresponding elements of initial and of the elements of others.

    ;; Preconditions:
    ;;   The length of initial is equal to the length of every element of
    ;;     others.

    (define zip (pipe list transpose))

    ;; ===== unzip =======================================================
    ;; list(list(alpha)) -> list(alpha), list(alpha) ...

    ;; The unzip procedure returns the lists of corresponding elements in
    ;; the elements of ls.

    ;; Preconditions:
    ;;   ls is not empty.
    ;;   The lengths of the elements of ls are equal.

    (define unzip (pipe transpose delist))

    ;; ===== collect-map ==================================================
    ;; (alpha, beta ... -> gamma ...), list(alpha), list(beta) ... ->
    ;; procedure                       initial      others
    ;;                                                          list(gamma)

    ;; The collect-map procedure applies procedure to corresponding
    ;; elements of one or more lists (initial and the elements of others),
    ;; collecting all of the results in a list.

    ;; Preconditions:
    ;;   The length of initial and the lengths of the elements of others
    ;;     are equal.
    ;;   procedure can receive corresponding elements of initial and of the
    ;;     elements of others.

    (define collect-map
      (run (~initial (sect pipe <> list)) map delist append))

    ;; ===== dispatch-return-all ==========================================
    ;; (alpha ... -> beta ...) ... -> (alpha ... -> beta ...)
    ;; procedures                      arguments

    ;; The dispatch-return-all procedure constructs a procedure that
    ;; applies every element of procedures to the elements of arguments,
    ;; returning all of the results.

    ;; Preconditions:
    ;;   Every element of procedures can receive the elements of arguments.

    (define dispatch-return-all
      (run (~each (sect pipe <> list))
           dispatch
           (sect run <> append delist)))

    ;; ===== cross-return-all =============================================
    ;; (alpha -> beta ...) ... -> (alpha ... -> beta ...)
    ;; procedures                  arguments

    ;; The cross-return-all procedure constructs a procedure that applies
    ;; each element of procedures to the corresponding element of
    ;; arguments, returning all of the results.

    ;; Preconditions:
    ;;   Every element of procedures can receive the corresponding element
    ;;     of arguments.

    (define cross-return-all
      (run (~each (sect pipe <> list)) cross (sect run <> append delist)))

    ;; ===== all-alike ===================================================
    ;; (alpha, alpha -> Boolean) -> (alpha ... -> Boolean)
    ;; equivalent?                   arguments

    ;; The all-alike procedure constructs a predicate that the elements of
    ;; arguments satisfy if, and only if, every two of them satisfy
    ;; equivalent?.

    ;; Preconditions:
    ;;   equivalent? is an equivalence relation.
    ;;   equivalent? can receive any elements of arguments.

    (define (all-alike equivalent?)
      (lambda arguments
        (or (empty-list? arguments)
            (receive (initial remaining) (deprepend arguments)
              (for-all? (sect equivalent? initial <>) remaining)))))

    ;; ===== all-different ===============================================
    ;; (alpha, alpha -> Boolean) -> (alpha ... -> Boolean)
    ;; equivalent?                   arguments

    ;; The all-different procedure constructs a predicate that the elements
    ;; of arguments satisfy if, and only if, no two of them satisfy
    ;; equivalent?.

    ;; Preconditions:
    ;;   equivalent? is symmetric.
    ;;   equivalent? can receive any elements of arguments.

    (define (all-different equivalent?)
      (pipe list
            (check empty-list?
                   (run deprepend
                        (~initial (curry equivalent?))
                        exists?
                        not)
                   rest)))

    ;; ===== position-in ==================================================
    ;; alpha, list(alpha) -> natural-number | Boolean
    ;; val    ls

    ;; The position-in procedure determines the least position at which val
    ;; occurs in ls, returning #f if there is no such position.

    (define (position-in val ls)
      ((rec (searcher sublist count)
         (if (empty-list? sublist)
             #f
             (if (equal? (first sublist) val)
                 count
                 (searcher (rest sublist) (add1 count)))))
       ls 0))

    ;; ===== positional-weights ===========================================
    ;; list(natural-number) -> list(natural-number)
    ;; numlist

    ;; The positional-weights procedure computes a list of the positional
    ;; weights of the digits in a mixed-base numeral, given a list,
    ;; numlist, of the bases.

    ;; Precondition:
    ;;   Every element of numlist is positive.

    (define positional-weights
      (fold-list (create (list 1))
                 (lambda (element weights)
                   (cons (* element (first weights)) weights))))

    ;; ===== separate-indices =============================================
    ;; natural-number, list(natural-number) -> list(natural-number)
    ;; number          weights

    ;; The separate-indices procedure constructs a list of values of the
    ;; digits for the numeral for number in a mixed-base system of
    ;; numeration, using weights as the positional weights.

    ;; Precondition:
    ;;   Every element of weights is positive.

    (define (separate-indices number weights)
      (if (empty-list? weights)
          (list)
          (receive (quot rem) (div-and-mod number (first weights))
            (cons quot (separate-indices rem (rest weights))))))

    ;; ===== all-but-position =============================================
    ;; list(alpha), natural-number -> list(alpha)
    ;; ls           position

    ;; The all-but-position procedure constructs a list comprising all of
    ;; the elements of ls except the one at position position.

    ;; Preconditions:
    ;;   ls is not empty.
    ;;   position is less than the length of ls.

    (define (all-but-position ls position)
      (if (zero? position)
          (rest ls)
          (prepend (first ls)
                   (all-but-position (rest ls) (sub1 position)))))

    ;; ===== matches-prefix? ==============================================
    ;; list(alpha), list(alpha) -> Boolean
    ;; pattern      text

    ;; The matches-prefix? predicate determines whether each element of
    ;; pattern is the same as the corresponding element of text.

    ;; Precondition:
    ;;   The length of pattern is less than or equal to the length of text.

    (define matches-prefix?
      (check (pipe >initial empty-list?)
             (pipe (~each first) equal?)
             (~each rest)))))

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
