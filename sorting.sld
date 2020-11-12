;;; Sorting

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created September 27, 2011 
;;; last revised January 6, 2017

(define-library (afp sorting)
  (export sort-by-insertion sort-by-selection quicksort mergesort
          binary-search-tree-sort heapsort)
  (import (rename (afp sorting insertion-sort) (sort sort-by-insertion))
          (rename (afp sorting selection-sort) (sort sort-by-selection))
          (rename (afp sorting quicksort) (sort quicksort))
          (rename (afp sorting mergesort) (sort mergesort))
          (rename (afp sorting binary-search-tree-sort)
                  (sort binary-search-tree-sort))
          (rename (afp sorting heapsort) (sort heapsort)))
  (begin))

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
