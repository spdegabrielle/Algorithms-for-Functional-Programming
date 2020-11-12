;;; Primitive procedures and syntax

;;; John David Stone
;;; Department of Computer Science
;;; Grinnell College

;;; created August 17, 2011
;;; last revised January 9, 2017

;;; This library defines the primitive procedures and syntax extensions
;;; used in _Algorithms for functional programming_.

(define-library (afp primitives)
  (export quote + - * / div-and-mod expt square lambda apply list values
          delist zero? positive? negative? even? odd? < <= = > >= boolean?
          not number? integer? char? string? symbol? procedure? boolean=?
          char=? string=? symbol=? equal? if and or define let rec
          receive map sect length natural-number? null null? pair? cons 
          car cdr list? list-ref reverse append min max source
          end-of-source end-of-source? define-record-type char-ci=?
          string<=? char<=? string>=? char>=? string-ref string-length
          char->integer)
  (import (rename (scheme base) (expt r7rs-expt))
          (only (scheme char) char-ci=?))
  (begin

    ;; ===== div-and-mod ==================================================
    ;; integer,  integer  -> integer, natural
    ;; dividend  divisor

    ;; The div-and-mod procedure performs a number-theoretic integer
    ;; division of its first argument by its second, returning a quotient
    ;; and remainder such that (a) the remainder is a natural number less
    ;; than or equal to the absolute value of the divisor, and (b) the
    ;; dividend is equal to the sum of the remainder and the product of the
    ;; quotient and the divisor.

    ;; Precondition: divisor is not zero.

    (define (div-and-mod dividend divisor)
      (let-values (((near-quot near-rem) (floor/ dividend divisor)))
        (if (negative? near-rem)
            (values (+ near-quot 1) (- near-rem divisor))
            (values near-quot near-rem))))

    ;; ===== expt =========================================================
    ;; number, integer  -> number
    ;; base    exponent
    
    ;; The expt procedure raises base to the power of exponent.

    ;; This version of the expt procedure requires the exponent to be an
    ;; integer, so that the result is always rational if the base is
    ;; rational.

    ;; Precondition:
    ;;   Either base is not zero or exponent is not negative.

    (define (expt base exponent)
      (unless (integer? exponent)
        (error "expt: non-integer exponent"))
      (r7rs-expt base exponent))

    ;; ===== delist =======================================================
    ;; list(alpha) -> alpha ...
    ;; ls
    
    ;; The delist procedure returns the elements of ls.

    (define (delist ls)
      (apply values ls))

    ;; ===== rec ==========================================================

    ;; The syntax extension with the keyword `rec' provides a simple,
    ;; readable expression syntax for recursively defined procedures.

    ;; I am indebted to Al Petrovsky for the definition of this syntax
    ;; extension.  Mr. Petrovsky published this definition in the SRFI-31
    ;; discussion list on August 8, 2002:
    ;; 
    ;;  http://srfi.schemers.org/srfi-31/mail-archive/msg00009.html
    ;;
    ;; and the author of SRFI 31, Mirko Luedde, incorporated it into the
    ;; final version (December 12, 2002):
    ;; 
    ;;           http://srfi.schemers.org/srfi-31/srfi-31.html
    
    ;; The rec syntax can be used either to bind a name only, in the form
    ;;
    ;;                     (rec <name> <expression>)
    ;;
    ;; or with a "formals list" that also binds the parameters of the
    ;; recursively defined procedure, as in a short-form procedure
    ;; definition:
    ;;
    ;;    (rec (<name> <param> ...) <expression> ...)
    ;;    (rec (<name> <param> ... . <rest-param>) <expression> ...)
    ;;
    ;; (The first of these forms is for recursive procedures of fixed arity,
    ;; the second for procedures of variable arity.)

    (define-syntax rec
      (syntax-rules ()
        ((rec (name . variables) . body)
         (letrec ((name (lambda variables . body))) name))
        ((rec name expression)
         (letrec ((name expression)) name))))

    ;; ===== receive ======================================================

    ;; The syntax extension with the keyword `receive' provides a concise
    ;; and readable syntax for binding identifiers to the values of a
    ;; multiple-valued expression.

    ;; The general form of a use of the receive macro is
    ;;
    ;;              (receive <formals> <expression> <body>)
    ;;
    ;; <Formals>, <expression>, and <body> are as described in section 11.3
    ;; and 11.4 of R6RS.  Specifically, <formals> can have any of three
    ;; forms:
    ;;
    ;; * (<variable-1> ... <variable-n>):  The environment in which the
    ;;   receive-expression is evaluated is extended by binding
    ;;   <variable-1>, ..., <variable-n> to fresh locations.  The
    ;;   <expression> is evaluated, and its values are stored into those
    ;;   locations.  It is an error if <expression> does not have exactly n
    ;;   values.
    ;;
    ;; * <variable>:  The environment in which the receive-expression is
    ;;   evaluated is extended by binding <variable> to a fresh location.
    ;;   The <expression> is evaluated, its values are converted into a
    ;;   newly allocated list, and the list is stored in the location to
    ;;   which <variable> is bound.
    ;;
    ;; * (<variable-1> ... <variable-n> . <variable-(n + 1)>):  The
    ;;   environment in which the receive-expression is evaluated is
    ;;   extended by binding <variable-1>, ..., <variable-(n + 1)> to fresh
    ;;   locations.  The <expression> is evaluated.  Its first n values are
    ;;   stored into the locations bound to <variable-1>, ...,
    ;;   <variable-n>.  Any remaining values are converted into a newly
    ;;   allocated list, which is stored into the location bound to
    ;;   <variable-(n + 1)>.  It is an error if <expression> does not have
    ;;   at least n values.
    ;;
    ;; In any case, the expressions in <body> are evaluated sequentially in
    ;; the extended environment.  The results of the last expression in the
    ;; body are the values of the receive-expression.

    (define-syntax receive
      (syntax-rules ()
        ((receive formals expression body ...)
         (call-with-values (lambda () expression)
                           (lambda formals body ...)))))

    ;; ===== sect =========================================================

    ;; I am indebted to Al Petrovsky for the definition of the syntax
    ;; extension for procedure sections that I have used here, in slightly
    ;; adapted form.  Mr. Petrovsky published this definition in the
    ;; SRFI-26 discussion list on June 4 and 5, 2002:
    ;; 
    ;;  http://srfi.schemers.org/srfi-26/mail-archive/msg00070.html
    ;;  http://srfi.schemers.org/srfi-26/mail-archive/msg00072.html
    ;;
    ;; He contributed this code to the public domain:
    ;;
    ;;  http://srfi.schemers.org/srfi-26/mail-archive/msg00077.html

    ;; The syntax extension with the keyword `sect' provides a concise
    ;; notation for "procedure sections," that is, specializations of a
    ;; multi-argument procedure formed by substituting fixed expressions
    ;; for some of its parameters and removing them from the parameter
    ;; list.

    ;; A sect-expression consists of the keyword sect and one or more
    ;; subexpressions, each of which can be either an ordinary Scheme
    ;; expression or the literal <> ("slot").  The value of the
    ;; sect-expression is a procedure with a parameter for each slot in the
    ;; sect-expression and a body consisting of the subexpressions of the
    ;; sect-expression, but with each slot replaced by the corresponding
    ;; parameter.

    ;; The last subexpression in a sect-expression can also be the literal
    ;; <...> ("rest-slot").  In this case, the value of the sect-expression
    ;; is a variable-arity procedure with a "rest" parameter.  When and if
    ;; the procedure is called, the values of any residual arguments are
    ;; bundled as a list, which becomes the value of the rest parameter.

    ;; This construction was originally proposed (under the name `cut') in
    ;; Scheme Request for Implementation 26
    ;; (http://srfi.schemers.org/srfi-26/srfi-26.html), which was finalized
    ;; and adopted on June 14, 2002.  In that document, the designer of the
    ;; construction, Sebastian Egner, offered the following examples of its
    ;; use:
    ;;
    ;;      (sect cons (+ a 1) <>) expands to
    ;;           (lambda (x2) (cons (+ a 1) x2))
    ;;
    ;;      (sect list 1 <> 3 <> 5) expands to
    ;;           (lambda (x2 x4) (list 1 x2 3 x4 5))
    ;;
    ;;      (sect list) expands to
    ;;           (lambda () (list))
    ;;
    ;;      (sect list 1 <> 3 <...>) expands to
    ;;           (lambda (x2 . xs) (apply list 1 x2 3 xs))
    ;;
    ;;      (sect <> a b) expands to
    ;;           (lambda (f) (f a b))
    ;;
    ;;      Double every element of a given list:
    ;;          (map (sect * 2 <>) '(1 2 3 4))
    ;;
    ;;      Destructively zero out selected positions in a vector:
    ;;          (map (sect vector-set! x <> 0) indices)
    ;;
    ;;      Write the values of several expressions to an output port:
    ;;          (for-each (sect write <> port) exprs)
    ;;
    ;;      List the least and greatest of certain values:
    ;;          (map (sect <> x y z) (list min max))
    ;;
    ;;      Invoke a sequence of thunks:
    ;;          (for-each (sect <>) thunks)

    ;; The internal-sect macro keeps track of the information needed at
    ;; intermediate stages of expansion of a sect-expression.  The plan is
    ;; to process the subexpressions of the sect-expression one by one.
    ;; When a slot is encountered, a new parameter is generated and added
    ;; both the parameter list and to the body of the procedure that is
    ;; being constructed; a non-slot subexpression is simply transferred to
    ;; the body unchanged.

    ;; The general form of a use of this macro is
    ;;
    ;;            (internal-sect slot-names combination . cs)
    ;;
    ;; Here slot-names is the list of parameters that have been generated
    ;; so far, combination is the part of the (single-expression) procedure
    ;; body that has been generated so far, and cs comprises the
    ;; subexpressions of the original sect-expression that have not yet
    ;; been processed.  Initially, slot-names and combination are empty
    ;; lists, and all of the subexpressions of the original sect-expression
    ;; are stored in cs.
    ;;
    ;; Once all of the subexpressions of the original sect-expression have
    ;; been processed (except possibly for a trailing <...>), a
    ;; lambda-expression for the procedure is assembled, and it becomes the
    ;; result of the expansion.
    ;;
    ;; The identifier x is used for each parameter that corresponds to a
    ;; slot as it is encountered, but Scheme's hygienic macro system
    ;; ensures that different replacement identifiers are generated
    ;; automatically during expansion.

    (define-syntax internal-sect
      (syntax-rules (<> <...>)

        ;; The first two syntax rules construct fixed- and variable-arity
        ;; procedures, respectively, once all of the subexpressions of the
        ;; original sect-expression have been processed.

        ;; The unusual-looking form "(begin proc)" ensures that a
        ;; non-expression in operator position, such as "(sect quote <>)",
        ;; is reported as an error.

        ((internal-sect (slot-name ...) (proc arg ...))
         (lambda (slot-name ...) ((begin proc) arg ...)))
        ((internal-sect (slot-name ...) (proc arg ...) <...>)
         (lambda (slot-name ... . rest-slot)
           (apply proc arg ... rest-slot)))

        ;; The third and fourth syntax rules process a single subexpression
        ;; of the original sect-expression.  If the subexpression is a
        ;; slot, a parameter is generated and appended to the parameter
        ;; list and to the future procedure body:

        ((internal-sect (slot-name ...) (position ...) <> . cs)
         (internal-sect (slot-name ... x) (position ... x) . cs))

        ;; If it is not a slot, it is just transferred to the procedure
        ;; body.

        ((internal-sect (slot-name ...) (position ...) const . cs)
         (internal-sect (slot-name ...) (position ... const) . cs))))

    ;; A sect-expression expands into a use of internal-sect with
    ;; appropriate initial subexpressions.

    (define-syntax sect
      (syntax-rules ()
        ((sect . consts-or-slots)
         (internal-sect () () . consts-or-slots))))

    ;; ===== natural-number? ==============================================
    ;; any       -> Boolean
    ;; something

    ;; The predicate natural-number? determines whether its argument is a
    ;; natural number -- in Scheme terminology, an exact non-negative
    ;; integer.

    (define (natural-number? something)
      (and (integer? something)
           (exact? something)
           (not (negative? something))))

    ;; ===== null =========================================================
    ;; null

    ;; null is the null value -- the unique value of the null data type.
    ;; In Scheme it also serves as the empty list.

    (define null '())

    ;; ===== source =======================================================

    ;; The source syntax expands into a lambda-expression with no arguments
    ;; and the body of the source-expression as its body.  It is used to
    ;; produce sources, as described in the section "Sources" of
    ;; _Algorithms for functional programming.

    (define-syntax source
      (syntax-rules ()
        ((_ body)
         (lambda () body))))

    ;; ===== end-of-source ================================================

    ;; A source that contains no useful value can instead signal this fact
    ;; by producing an artifically selected sentinel.  This record type
    ;; definition provides such a signal, which can be created with the
    ;; constructor end-of-source and detected with the classification
    ;; predicate end-of-source?.

    (define-record-type end-of-source-record
      (end-of-source)
      end-of-source?)))
             
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
