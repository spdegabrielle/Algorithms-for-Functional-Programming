;;; Partitions

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created February 11, 2000
;;; last revised January 6, 2017

(define-library (afp partitions)
  (export extend-partition partitions bounded-number-partitions
          number-partitions)
  (import (afp primitives)
          (only (afp arithmetic) sub1)
          (only (afp constant-procedures) create)
          (only (afp procedure-sections) curry)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) ~initial)
          (only (afp lists) run)
          (only (afp bags)
                bag put-into-bag fold-bag bag-union map-bag
                remove-bag-from-bag))
  (begin

    ;; ===== extend-partition =============================================
    ;; alpha, bag(bag(alpha)) -> bag(bag(bag(alpha)))
    ;; new    araro

    ;; The extend-partition procedure constructs a bag containing
    ;; variations of araro.  In each variation, new has either been put
    ;; into one of the elements of araro or (in one case) put into a bag by
    ;; itself, and this bag put into a bag with the elements of araro.

    (define (extend-partition new araro)
      (put-into-bag (put-into-bag (bag new) araro)
                    (map-bag (pipe (dispatch
                                     (sect put-into-bag new <>)
                                     (sect remove-bag-from-bag <> araro))
                                   put-into-bag)
                             araro)))

    ;; ===== partitions ===================================================
    ;; bag(alpha) -> bag(bag(bag(alpha)))
    ;; aro

    ;; The partitions procedure constructs a bag containing all of the
    ;; partitions of aro, each partition being a bag containing the subbags
    ;; of aro into which the values in aro have been distributed.

    (define partitions
      (fold-bag (create (bag (bag)))
                (run (~initial (curry extend-partition))
                     map-bag
                     (fold-bag bag bag-union))))

    ;; ===== bounded-number-partitions ====================================
    ;; natural-number, natural-number -> bag(bag(natural-number))
    ;; number          bound

    ;; The bounded-number-partitions procedure constructs a bag containing
    ;; all of the partitions of number in which each value is less than or
    ;; equal to bound.

    (define (bounded-number-partitions number bound)
      (if (zero? number)
          (bag (bag))
          (if (zero? bound)
              (bag)
              (if (< number bound)
                  (bounded-number-partitions number (sub1 bound))
                  (bag-union (bounded-number-partitions number
                                                        (sub1 bound))
                             (map-bag (sect put-into-bag bound <>)
                                      (bounded-number-partitions
                                        (- number bound) bound)))))))

    ;; ===== number-partitions ============================================
    ;; natural-number -> bag(bag(natural-number))
    ;; number

    ;; The number-partitions procedure constructs a bag containing the
    ;; partitions of number: bags of positive integers that add up to
    ;; number.

    (define (number-partitions number)
      (bounded-number-partitions number number))))

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
