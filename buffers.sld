;;; Buffers

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created November 22, 1998
;;; last revised January 9, 2017

(define-library (afp buffers)
  (export buffer? buffer put-into-buffer take-from-buffer empty-buffer?
          buffer-of= buffer=? unfold-buffer process-buffer)
  (import (afp primitives)
          (only (afp constant-procedures) constant)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters)
                >initial >next >all-but-initial ~initial ~next)
          (only (afp recursion-managers) build iterate)
          (only (afp predicate-operations) ^if)
          (only (afp pairs) decons pair-of)
          (only (afp lists)
                first rest empty-list? prepend deprepend run every))
  (begin

    ;; ===== buffer? ======================================================
    ;; any       -> Boolean
    ;; something

    ;; The buffer? predicate determines whether something is a buffer.

    (define buffer? (pair-of list? list?))

    ;; ===== buffer =======================================================
    ;; alpha ... -> buffer(alpha)
    ;; arguments

    ;; The buffer procedure constructs a buffer containing the elements of
    ;; arguments.

    (define buffer (pipe list (sect cons <> (list))))

    ;; ===== put-into-buffer ==============================================
    ;; alpha, buffer(alpha) -> buffer(alpha)
    ;; item   buf

    ;; The put-into-buffer procedure constructs a buffer containing the
    ;; values in buf and, in addition, item.

    (define (put-into-buffer item buf)
      (cons (car buf) (prepend item (cdr buf))))

    ;; ===== take-from-buffer =============================================
    ;; buffer(alpha) -> alpha, buffer(alpha)
    ;; buf

    ;; The take-from-buffer procedure takes a value from buf, returning the
    ;; value and a buffer similar to buf, but without the returned value.

    ;; Precondition:
    ;;   buf is not empty.

    (define take-from-buffer
      (^if (pipe car empty-list?)
           (run cdr reverse deprepend (~next (sect cons <> (list))))
           (pipe decons (dispatch (pipe >initial first)
                                  (pipe (~initial rest) cons)))))

    ;; ===== empty-buffer? ================================================
    ;; buffer(any) -> Boolean
    ;; buf

    ;; The empty-buffer? procedure determines whether buf is empty.

    (define empty-buffer? (pipe decons (every empty-list?)))

    ;; ===== buffer-of= ===================================================
    ;; (alpha, beta -> Boolean) -> (buffer(alpha), buffer(beta) -> Boolean)
    ;; element=?                    left           right

    ;; The buffer-of= procedure constructs a predicate that determines
    ;; whether left and right are the same, that is, whether they contain
    ;; the same values in the same order, using element=? as the criterion
    ;; of sameness.

    ;; Precondition:
    ;;   element=? can receive any value in left as its first argument and
    ;;     any value in right as its second.

    (define (buffer-of= element=?)
      (rec (equivalent? left right)
        (or (and (empty-buffer? left)
                 (empty-buffer? right))
            (and (not (empty-buffer? left))
                 (not (empty-buffer? right))
                 (receive (left-item new-left) (take-from-buffer left)
                   (receive (right-item new-right) (take-from-buffer right)
                     (and (element=? left-item right-item)
                          (equivalent? new-left new-right))))))))

    ;; ===== buffer=? =====================================================
    ;; buffer(any), buffer(any) -> Boolean
    ;; left         right

    ;; The buffer=? determines whether left and right contain the same
    ;; values in the same order.

    (define buffer=? (buffer-of= equal?))

    ;; ===== unfold-buffer ================================================
    ;; (alpha ... -> Boolean), (alpha ... -> beta),
    ;; final?                  producer
    ;;              (alpha ... -> alpha ...) -> (alpha ... -> buffer(beta))
    ;;              step                         arguments

    ;; The unfold-buffer procedure constructs a procedure that first
    ;; determines whether the elements of arguments satisfy final?.  If so,
    ;; the constructed procedure returns the empty buffer.  Otherwise, it
    ;; returns a non-empty buffer in which the foremost value is the result
    ;; of applying producer to the elements of arguments, and the rest of
    ;; the values are stored in a buffer that is the result of first
    ;; applying step to the elements of arguments and then applying the
    ;; constructed procedure recursively to the results.

    ;; Preconditions:
    ;;   final? can receive the elements of arguments.
    ;;   final? can receive the results of any invocation of step.
    ;;   If the elements of arguments do not satisfy final?, then producer
    ;;     can receive them.
    ;;   If the results of an invocation of step do not satisfy final?, then
    ;;     producer can receive them.
    ;;   If the elements of arguments do not satisfy final?, then step can
    ;;     receive them.
    ;;   If the results of an invocation of step do not satisfy final?, then
    ;;     step can receive them.

    (define (unfold-buffer final? producer step)
      (build final? (constant (buffer)) producer step put-into-buffer))

    ;; ===== process-buffer ===============================================
    ;; (-> alpha ...), (beta, alpha ... -> alpha ...) ->
    ;; base          combiner
    ;;                                          (buffer(beta) -> alpha ...)
    ;;                                           buf

    ;; The process-buffer procedure constructs a procedure that iteratively
    ;; applies combiner to a value from buf and the results of the previous
    ;; iteration (or to the results of invoking base, if there was no
    ;; previous iteration).  The constructed procedure returns the results
    ;; of the last application of combiner.

    ;; Preconditions:
    ;;   combiner can receive the foremost value in buf and the results of
    ;;     an invocation of base.
    ;;   combiner can receive any but the foremost value in buf and the
    ;;     results of any invocation of combiner.

    (define (process-buffer base combiner)
      (run (lambda (buf)
             (receive starters (base)
               (apply values buf starters)))
           (iterate (pipe >initial empty-buffer?)
                    (lambda (subbuf . results-so-far)
                      (receive (item new-subbuf) (take-from-buffer subbuf)
                        (receive new-results
                                 (apply combiner item results-so-far)
                           (apply values new-subbuf new-results)))))
           >all-but-initial))))

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
