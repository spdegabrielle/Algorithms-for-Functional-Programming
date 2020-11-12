;;; Mergesort

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 27, 1999
;;; last revised January 6, 2017

(define-library (afp sorting mergesort)
  (export split-bag-evenly merge-lists sort)
  (import (afp primitives)
          (only (afp constant-procedures) create)
          (only (afp couplers) pipe dispatch)
          (only (afp adapters) >next)
          (only (afp lists) first rest empty-list? prepend adapter)
          (only (afp bags)
                bag empty-bag? put-into-bag take-from-bag fold-bag)) 
  (begin

    ;; ===== split-bag-evenly =============================================
    ;; bag(alpha) -> bag(alpha), bag(alpha)
    ;; aro

    ;; The split-bag-evenly procedure constructs two bags, differing in
    ;; cardinality by at most one, the bag-union of which is aro.

    (define split-bag-evenly
      (fold-bag (create (bag) (bag))
                (dispatch (pipe (adapter 0 2) put-into-bag) >next)))

    ;; ===== merge-lists ==================================================
    ;; (alpha, alpha -> Boolean), list(alpha), list(alpha) -> list(alpha)
    ;; may-precede?               left         right

    ;; The merge-list procedure constructs a list, ordered with respect to
    ;; may-precede?, that contains all of the elements of left and all of
    ;; the elements of right.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any element of left and any element of
    ;;     right.
    ;;   left is ordered with respect to may-precede?.
    ;;   right is ordered with respect to may-precede?.

    (define (merge-lists may-precede? left right)
      ((rec (merger subleft subright)
         (if (empty-list? subleft)
             subright
             (if (empty-list? subright)
                 subleft
                 (if (may-precede? (first subleft) (first subright))
                     (prepend (first subleft)
                              (merger (rest subleft) subright))
                     (prepend (first subright)
                              (merger subleft (rest subright)))))))
       left right))

    ;; ===== sort =========================================================
    ;; (alpha, alpha -> Boolean), bag(alpha) -> list(alpha)
    ;; may-precede?               aro

    ;; The sort procedure constructs a list that has the values in aro as
    ;; its elements and is ordered by may-precede?.

    ;; Preconditions:
    ;;   may-precede? is an ordering relation.
    ;;   may-precede? can receive any values in aro.

    (define (sort may-precede? aro)
      (if (empty-bag? aro)
          (list)
          ((rec (sorter areto)
             (receive (chosen others) (take-from-bag areto)
               (if (empty-bag? others)
                   (list chosen)
                   (receive (left right) (split-bag-evenly areto)
                     (merge-lists may-precede?
                                  (sorter left)
                                  (sorter right))))))
           aro)))))

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
