;;; Order statistics

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 27, 1999
;;; last revised January 6, 2017

(define-library (afp order-statistics)
  (export order-statistic)
  (import (afp primitives)
          (only (afp arithmetic) add1)
          (only (afp bags) take-from-bag partition-bag-with-count))
  (begin

    ;; ===== order-statistic ==============================================
    ;; (alpha, alpha -> Boolean), bag(alpha), natural-number -> alpha
    ;; may-precede?               aro         index

    ;; The order-statistic procedure returns a value such that more than
    ;; index values in aro bear may-precede? to it, and it bears
    ;; may-precede?  to at least as many values as the amount by which the
    ;; cardinality of aro exceeds index.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values in aro.
    ;;   index is less than the cardinality of aro.

    (define (order-statistic may-precede? aro index)
      ((rec (partitioner areto index)
         (receive (pivot others) (take-from-bag areto)
           (receive (count ins outs)
                    (partition-bag-with-count (sect may-precede? <> pivot)
                                              others)
             (if (< index count)
                 (partitioner ins index)
                 (if (< count index)
                     (partitioner outs (- index (add1 count)))
                     pivot)))))
       aro index))))

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
