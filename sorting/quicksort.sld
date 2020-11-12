;;; Quicksort

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created April 21, 1999
;;; last revised January 6, 2017

(define-library (afp sorting quicksort)
  (export sort)
  (import (afp primitives)
          (only (afp lists) prepend catenate)
          (only (afp bags) empty-bag? take-from-bag partition-bag))
  (begin

    ;; ===== sort =========================================================
    ;; (alpha, alpha -> Boolean), bag(alpha) -> list(alpha)
    ;; may-precede?               aro

    ;; The sort procedure constructs a list that has the values in aro as
    ;; its elements and is ordered by may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values in aro.

    (define (sort may-precede? aro)
      ((rec (sorter areto)
         (if (empty-bag? areto)
             (list)
             (receive (pivot others) (take-from-bag areto)
               (receive (fore aft)
                        (partition-bag (sect may-precede? <> pivot) others)
                 (catenate (sorter fore) (prepend pivot (sorter aft)))))))
       aro))))

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
