;;; Arithmetic procedures

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 13, 1999
;;; last revised January 9, 2017

(define-library (afp arithmetic)
  (export arithmetic-mean lesser factorial power-of-two add1 sub1
          double power-of-two? halve div mod greatest-odd-divisor
          ceiling-of-log-two divisible-by? greater-and-lesser
          greatest-common-divisor termial antitermial binomial)
  (import (afp primitives)
          (only (afp constant-procedures) constant)
          (only (afp adapters) >initial >next identity)
          (only (afp couplers) pipe cross dispatch)
          (only (afp recursion-managers) recur iterate))
  (begin

    ;; ===== arithmetic-mean ==============================================
    ;; number, number -> number
    ;; a       b

    ;; The arithmetic-mean procedure computes the arithmetic mean of a and
    ;; b.

    (define (arithmetic-mean a b)
      (/ (+ a b) 2))

    ;; ===== lesser =======================================================
    ;; number, number -> number
    ;; left    right

    ;; The lesser procedure returns left or right, whichever is lesser.

    (define (lesser left right)
      (if (< left right) left right))

    ;; ===== factorial ====================================================
    ;; natural-number -> natural-number
    ;; number

    ;; The factorial procedure computes the product of the positive
    ;; integers up to and including number.

    (define (factorial number)
      (if (zero? number)
          1
          (* number (factorial (- number 1)))))

    ;; ===== double =======================================================
    ;; number -> number
    ;; number

    ;; The double procedure computes the double of number.

    (define (double number)
      (+ number number))

    ;; ===== power-of-two =================================================
    ;; integer  -> number
    ;; exponent

    ;; The power-of-two procedure raises 2 to the power specified by
    ;; exponent.

    (define power-of-two (sect expt 2 <>))

    ;; ===== add1 =========================================================
    ;; number     -> number
    ;; increscend

    ;; The add1 procedure adds 1 to increscend.

    (define add1 (sect + <> 1))

    ;; ===== sub1 =========================================================
    ;; number     -> number
    ;; decrescend

    ;; The sub1 procedure subtracts 1 from decrescend.

    (define sub1 (sect - <> 1))

    ;; ===== div ==========================================================
    ;; integer,  integer  -> integer
    ;; dividend  divisor

    ;; The div procedure performs a number-theoretic integer division of
    ;; its first argument by its second, returning a quotient such that the
    ;; product of the quotient and the divisor is less than or equal to the
    ;; dividend and differs from it by an amount that is less than the
    ;; absolute value of the divisor.

    ;; Precondition: divisor is not zero.

    (define div (pipe div-and-mod >initial))

    ;; ===== mod ==========================================================
    ;; integer,  integer  -> natural
    ;; dividend  divisor

    ;; The mod procedure performs a number-theoretic integer division of
    ;; its first argument by its second, returning a remainder indicating
    ;; the amount by which dividend exceeds the greatest multiple of
    ;; divisor less than or equal to dividend.

    ;; Precondition: divisor is not zero.

    (define mod (pipe div-and-mod >next))

    ;; ===== power-of-two? ================================================
    ;; natural-number -> Boolean
    ;; candidate

    ;; The power-of-two? predicate determines whether candidate is an exact
    ;; power of two.

    ;; Precondition:
    ;;   candidate is positive.

    (define (power-of-two? candidate)
      (or (= candidate 1)
          (and (even? candidate)
               (power-of-two? (halve candidate)))))

    ;; ===== halve ========================================================
    ;; integer -> integer
    ;; dividend

    ;; The halve procedure divides dividend by 2 and returns the quotient,
    ;; discarding any remainder.

    (define halve (sect div <> 2))

    ;; ===== greatest-odd-divisor =========================================
    ;; natural-number -> natural-number
    ;; number

    ;; The greatest-odd-divisor procedure computes the greatest odd natural
    ;; number that evenly divides number.

    ;; Precondition:
    ;;   number is positive.

    (define greatest-odd-divisor (iterate odd? halve))

    ;; ===== ceiling-of-log-two ===========================================
    ;; natural-number -> natural-number
    ;; bound

    ;; The ceiling-of-log-two procedure computes the ceiling of the
    ;; base-two logarithm of bound, or in other words the number of
    ;; doubling operations that must performed in succession, starting from
    ;; 1, to obtain a number that equals or exceeds bound.

    ;; Precondition:
    ;;   bound is positive.

    (define (ceiling-of-log-two bound)
      ((pipe (iterate (pipe >initial (sect >= <> bound))
                      (cross double add1))
             >next)
       1 0))

    ;; ===== divisible-by? ================================================
    ;; integer, integer -> Boolean
    ;; dividend, candidate

    ;; The divisible-by? predicate determines whether dividend is evenly
    ;; divisible by candidate.

    ;; Precondition:
    ;;   candidate is not 0.

    (define divisible-by? (pipe mod zero?))

    ;; ===== greater-and-lesser ===========================================
    ;; number, number -> number, number
    ;; left    right

    ;; The greater-and-lesser procedure returns its arguments, the greater
    ;; one first.

    (define (greater-and-lesser left right)
      (if (< left right)
          (values right left)
          (values left right)))

    ;; ===== greatest-common-divisor ======================================
    ;; natural-number, natural-number -> natural-number
    ;; left            right

    ;; The greatest-common-divisor procedure computes the greatest positive
    ;; integer that is a divisor of both left and right.  This implementation
    ;; uses Euclid's algorithm.

    ;; Preconditions:
    ;;   left is positive.
    ;;   right is positive.

    (define greatest-common-divisor
      (pipe greater-and-lesser
            (pipe (iterate divisible-by? (dispatch >next mod)) >next)))

    ;; ===== termial ======================================================
    ;; natural-number -> natural-number
    ;; number

    ;; The termial procedure computes the sum of the natural numbers up to
    ;; and including number.

    (define (termial number)
      (halve (* number (add1 number))))

    ;; ===== antitermial ==================================================
    ;; natural-number -> natural-number, natural-number
    ;; number

    ;; The antitermial procedure computes the greatest natural number whose
    ;; termial does not exceed number and the amount by which number exceeds
    ;; that termial.

    (define (antitermial number)
      ((rec (converge lower upper)
         (if (= (add1 lower) upper)
             (values lower (- number (termial lower)))
             (let ((mid (halve (+ lower upper))))
               (if (< number (termial mid))
                   (converge lower mid)
                   (converge mid upper)))))
       0 (add1 number)))

    ;; ===== binomial =====================================================
    ;; natural-number, natural-number -> natural-number
    ;; n               k

    ;; The binomial procedure computes the binomial coefficient of n and k,
    ;; that is, the number of distinct ways of choosing k objects from a
    ;; collection of n distinguishable objects.

    ;; Precondition:
    ;;   k is less than or equal to n.

    (define (binomial n k)
      (let ((short (lesser k (- n k))))
        (div ((recur (sect < n <>) (constant 1) (dispatch identity add1) *)
              (add1 (- n short)))
             (factorial short))))))

;;; copyright (C) 2011, 2016 John David Stone

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
