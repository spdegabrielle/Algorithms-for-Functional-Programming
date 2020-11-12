;;; Sources

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 26, 2011 
;;; last revised January 5, 2017

(define-library (afp sources)
  (export tap natural-number-source source->list source-drop
          constant-source cyclic-source interleave integer-source
          map-source process-source unfold-source list->finite-source
          finite-source finite-source->list catenate-sources append-sources
          fold-finite-source map-finite-source
          ordered-prepend-to-each-source
          ordered-prepend-each-to-each-source)
  (import (afp primitives)
          (only (afp arithmetic) add1 sub1)
          (only (afp constant-procedures) constant create)
          (only (afp procedure-sections) invoke)
          (only (afp couplers) pipe)
          (only (afp adapters) >next identity)
          (only (afp predicate-operations) ^if)
          (only (afp natural-numbers) fold-natural)
          (only (afp lists)
                first rest empty-list? prepend deprepend
                extend-to-variable-arity))
  (begin

    ;; ===== tap ==========================================================
    ;; source(alpha) -> alpha, source(alpha)
    ;; src

    ;; The tap procedure returns the initial element of src and a source
    ;; containing the remaining elements of src.

    (define tap invoke)

    ;; ===== natural-number-source ========================================
    ;; source(natural-number)

    ;; The natural-number-source source contains the natural numbers, in
    ;; ascending order, as elements.

    (define natural-number-source
      ((rec (up-from nat)
         (source (values nat (up-from (add1 nat)))))
       0))

    ;; ===== source->list =================================================
    ;; source(alpha), natural-number -> list(alpha), source(alpha)
    ;; src            number

    ;; The source->list procedure constructs a list of the first number
    ;; elements of src, returning both the list and a source containing the
    ;; rest of the elements of src.

    (define (source->list src number)
      (if (zero? number)
          (values (list) src)
          (receive (initial others) (tap src)
            (receive (rest-of-list depleted)
                     (source->list others (sub1 number))
              (values (prepend initial rest-of-list) depleted)))))

    ;; ===== source-drop ==================================================
    ;; source(alpha), natural-number -> source(alpha)
    ;; src            num

    ;; The source-drop procedure discards num elements from src and returns
    ;; a source containing the rest of the elements of src.

    (define (source-drop src number)
      ((fold-natural (create src) (pipe tap >next)) number))

    ;; ===== constant-source ==============================================
    ;; alpha   -> source(alpha)
    ;; element

    ;; The constant-source procedure constructs a source with element as
    ;; each of its elements.

    (define (constant-source element)
      (rec repeater (source (values element repeater))))

    ;; ===== cyclic-source ================================================
    ;; alpha,  alpha ... -> source(alpha)
    ;; initial others

    ;; The cyclic-source procedure constructs a source with initial and the
    ;; elements of others as its elements, starting over with initial after
    ;; each has appeared.

    (define (cyclic-source initial . others)
      ((rec (cycler ls)
         (source (if (empty-list? ls)
                     (values initial (cycler others))
                     (values (first ls) (cycler (rest ls))))))
       (prepend initial others)))

    ;; ===== interleave ===================================================
    ;; source(alpha), source(alpha) -> source(alpha)
    ;; left           right

    ;; The interleave procedure constructs a source whose elements are
    ;; taken alternately from left and right.

    (define (interleave left right)
      (source (receive (initial new-right) (tap left)
                (values initial (interleave right new-right)))))

    ;; ===== integer-source ===============================================
    ;; source(integer)

    ;; The integer-source source contains all of the integers as elements,
    ;; in the order 0, -1, 1, -2, 2, -3, 3, ....

    (define integer-source
      (interleave natural-number-source
                  ((rec (down-from number)
                     (source (values number (down-from (sub1 number)))))
                   -1)))

    ;; ===== map-source ===================================================
    ;; (alpha ... -> beta), source(alpha) ... -> source(beta)
    ;; procedure            sources

    ;; The map-source procedure constructs a source in which each element
    ;; is the result of applying procedure to corresponding elements of
    ;; sources.

    ;; Precondition:
    ;;   procedure can receive corresponding elements of sources.

    (define (map-source procedure . sources)
      ((rec (mapper srcs)
         (source (let ((tap-pairs (map (pipe tap cons) srcs)))
                   (values (apply procedure (map car tap-pairs))
                           (mapper (map cdr tap-pairs))))))
       sources))

    ;; ===== process-source ===============================================
    ;; (-> alpha), (beta, alpha -> alpha) -> (source(beta) -> source(alpha))
    ;; base        combiner                   src

    ;; The process-source procedure constructs a procedure that iteratively
    ;; applies combiner to an element of src and the result of the previous
    ;; iteration (or to the result of invoking base, if there was no
    ;; previous iteration).  The constructed procedure returns a source
    ;; containing the results of successive iterations.

    ;; Preconditions:
    ;;   combiner can receive the initial element of src and the result of
    ;;     an invocation of base.
    ;;   combiner can receive any but the initial element of src and the
    ;;     result of any invocation of combiner.

    (define (process-source base combiner)
      (lambda (src)
        (source (let ((base-value (base)))
                  (values
                    base-value
                    ((rec (processor src previous)
                       (source (receive (initial others) (tap src)
                                 (let ((new (combiner initial previous)))
                                   (values new (processor others new))))))
                     src base-value))))))

    ;; ===== unfold-source ================================================
    ;; (alpha ... -> beta), (alpha ... -> alpha ...) ->
    ;; producer             step
    ;;                                          (alpha ... -> source(beta))
    ;;                                           arguments

    ;; The unfold-list procedure constructs a procedure that, in turn,
    ;; constructs a source in which the initial element is the result of
    ;; applying producer to the elements of arguments, and the remainder of
    ;; the source is the result of first applying step to the elements of
    ;; arguments and then applying the constructed procedure recursively to
    ;; the results.

    ;; Preconditions:
    ;;   producer can receive the elements of arguments.
    ;;   producer can receive the results of any invocation of step.
    ;;   step can receive the elements of arguments.
    ;;   step can receive the results of any invocation of step.

    (define (unfold-source producer step)
      (rec (unfolder . arguments)
        (source (values (apply producer arguments)
                        (apply (pipe step unfolder) arguments)))))

    ;; ===== list->finite-source ==========================================
    ;; list(alpha) -> source(alpha)
    ;; ls

    ;; The list->finite-source procedure constructs a finite source with
    ;; the elements of ls as its elements.

    (define list->finite-source
      (unfold-source (^if empty-list? (constant (end-of-source)) first)
                     (^if empty-list? identity rest)))

    ;; ===== finite-source ================================================
    ;; alpha ... -> source(alpha)
    ;; elements

    ;; The finite-source procedure constructs a finite source with the
    ;; elements of elements as its elements.

    (define (finite-source . elements)
      (list->finite-source elements))

    ;; ===== finite-source->list ==========================================
    ;; source(alpha) -> list(alpha)
    ;; src

    ;; The finite-source->list procedure constructs a list with the
    ;; elements of src as its elements.

    ;; Precondition:
    ;;   src is a finite source.

    (define (finite-source->list src)
      (receive (initial others) (tap src)
        (if (end-of-source? initial)
            (list)
            (prepend initial (finite-source->list others)))))

    ;; ===== catenate-sources =============================================
    ;; source(alpha), source(alpha) -> source(alpha)
    ;; left           right

    ;; The catenate-sources procedure constructs a source containing the
    ;; (pre-sentinel) elements of left, in order, followed by the elements
    ;; of right, in order.

    (define (catenate-sources left right)
      ((rec (advancer rest-of-left)
         (source (receive (initial others) (tap rest-of-left)
                   (if (end-of-source? initial)
                       (tap right)
                       (values initial (advancer others))))))
       left))

    ;; ===== append-sources ===============================================
    ;; source(alpha) ... -> source(alpha)
    ;; sources

    ;; The append-sources procedure constructs a source containing the
    ;; elements of the elements of sources, retaining both the relative
    ;; order of the elements and the relative order of the elements within
    ;; each element.

    (define append-sources
      (extend-to-variable-arity (constant-source (end-of-source))
                                catenate-sources))

    ;; ===== fold-finite-source ===========================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base            combiner
    ;;                                          (source(beta) -> alpha ...)
    ;;                                           src

    ;; The fold-finite-source procedure constructs a procedure that returns
    ;; the results of invoking base if src is empty.  If src is non-empty,
    ;; the constructed procedure applies itself recursively to the source
    ;; that results from tapping src and returns the results of applying
    ;; combiner to the first result of tapping src and the results of the
    ;; recursive invocation.

    ;; Preconditions:
    ;;   combiner can receive the last (pre-sentinel) element of src and
    ;;     the results of an invocation of base.
    ;;   combiner can receive any but the last (pre-sentinel) element of
    ;;     src and the results of any invocation of combiner.
    ;;   src is a finite source.

    (define (fold-finite-source base combiner)
      (rec (folder src)
        (receive (initial others) (tap src)
          (if (end-of-source? initial)
              (base)
              (receive recursive-results (folder others)
                (apply combiner initial recursive-results))))))

    ;; ===== map-finite-source ============================================
    ;; (alpha -> beta), source(alpha) -> source(beta)
    ;; procedure        src

    ;; The map-finite-source procedure constructs a source (a finite
    ;; source, if src is finite) comprising the results applying procedure
    ;; to successive elements of src.

    ;; Precondition:
    ;;   procedure can receive any element of src.

    (define (map-finite-source procedure src)
      ((rec (mapper subsource)
         (source (receive (chosen others) (tap subsource)
                   (if (end-of-source? chosen)
                       (values (end-of-source) subsource)
                       (values (procedure chosen) (mapper others))))))
       src))

    ;; ===== ordered-prepend-to-each-source ===============================
    ;; alpha,      source(list(alpha)) -> source(list(alpha))
    ;; new-initial src

    ;; The ordered-prepend-to-each-source constructs a finite source
    ;; containing the results of prepending new-initial to each of the
    ;; lists from src, in order.

    ;; Precondition:
    ;;   src is a finite source.

    (define (ordered-prepend-to-each-source new-initial src)
      (map-finite-source (sect prepend new-initial <>) src))

    ;; ===== ordered-prepend-each-to-each-source ==========================
    ;; list(alpha), source(list(alpha)) -> source(list(alpha))
    ;; initials     src

    ;; The ordered-prepend-each-to-each-source procedure constructs a
    ;; source containing the results of prepending each element of initials
    ;; to each of the lists in src, in lexicographic order.

    ;; Precondition:
    ;;   src is a finite source.

    (define (ordered-prepend-each-to-each-source initials src)
      ((rec (folder sublist)
         (if (empty-list? sublist)
             (finite-source)
             (source
               (receive (new-initial others) (deprepend sublist)
                 (tap (catenate-sources
                        (ordered-prepend-to-each-source new-initial src)
                        (folder others)))))))
       initials))))

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
