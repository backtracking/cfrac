(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Continued fractions.

    This module implements simple continued fractions, which means
    it represents a real number x as

                     1
      x = a0 + --------------------
                       1
               a1 + ---------------
                            1
                    a2 + ----------
                            ...

    where a0 and a1,a2,... > 0 are integers, called the terms
    of the CF. This sequence is written [a0; a1, a2, ...].

    The sequence is finite iff the number x is rational.
    When it is finite, the last term is > 1 (canonical CF).

    When it is infinite, it is periodic (after some initial terms) iff
    the number is an irrational real root of a quadratic equation
    ax^2+bx+c=0.
*)

type t
(** The type of a continued fraction. *)

val terms: t -> Z.t Seq.t
(** The terms of the continued fraction. *)

val floor: t -> Z.t
(** The floor function.
    This is the very first term of the continued fraction. *)

val convergents: t -> Q.t Seq.t
(** The convergents of the continued fraction.

    This is a sequence of rational numbers, obtained by using more and more
    terms from the continued fraction.
    The very first convergent is the integer part, and thus has
    denominator 1.
    The sequence of denominators is increasing (if we omit the very first
    denominator, which is 1 and can also appear as denominator of the
    second convergent).
    The convergents are successive approximations of the real number x
    represented by the continued fraction, of increasing precision.
    A convergent of even (resp. odd) order is lower (resp. greater) than x.

    Each convergent p/q is in its lowest terms, i.e. gcd(p,q)=1.
    The following inequality holds: |x - p/q| < 1/q^2.
    Thus, to get an approximation of x at 1e-6, it suffices to stop
    computing convergents as soon as q >= 1000.

    A convergent p/q is the best rational approximation of x with
    a denominator no greater than q. See function [best_approx] below. *)

val nth_convergent: int -> t -> Q.t
(** Merely a convenience function built on top of [convergents].
    Raises [Invalid_argument] is there is no such convergent, i.e. if
    the sequence of convergents has fewer elements.

    Note: [nth_convergent 0] returns the integer part of the
    continued fraction, and thus is identical to [floor]. *)

val best_approx: Z.t -> t -> Q.t
(** [best_approx d x] returns the best rational approximation of [x] with a
    denominator up to [d], for a given [d>0].
    Said otherwise, if [best_approx] returns p/q, then we have
    |qx-p| < |bx-a| for any other approximation a/b of x with b <= d. *)

val to_float: t -> float
(** Converts to a floating-point number.
    The result is the floating-point number closest to the given
    rational; ties break to even mantissa. *)

val print: prec:int -> Format.formatter -> t -> unit
(** Print a continued fraction as "[a0; a1, a2, ...]" up to [prec+1] terms.
    If there are more terms, an ellipsis "..." is printed.
    Otherwise, the list ends with the last term. *)

val print_decimals: prec:int -> Format.formatter -> t -> unit
(** Print the first [prec] decimals.
    If there are more decimals to come, an ellipsis "..." is printed.
    Otherwise, the list ends with the last decimal.

    Whenever the number if rational, with infinitely many decimals,
    decimals are periodic and [print_decimals] shows the period
    whenever it fits within the required precision [prec].
    For instance, 1/28 and 1/29 are printed as

      0.03(571428)*
      0.0(3448275862068965517241379310)*

    respectively. (Note that the period displayed for 1/29 could be shifted
    one position to the left. This happens when the very first decimal is 0
    and is part of the periodic sequence.) If you ever print 1/29 with too
    few decimals, say 15, then the period is not shown anymore (and instead
    we get the ellipsis "..."):

      0.034482758620689...
*)

val print_convergents: prec:int -> Format.formatter -> t -> unit
(** Print the sequence of convergents, up to [prec] fractions.
    If there are more convergents to come, an ellipsis "..." is printed.
    Otherwise, the list ends with the last convergent.

    Merely a convenient wrapper over [convergents]. *)

(** {2 Some constructors}

  Notes:

  - Function [of_q] returns a finite sequence --- rational numbers are exactly
    the real numbers with finite continued fractions --- whose last term is
    not 1. For instance, the continued fraction of 355/113 will be [3; 7, 16]
    and not [3; 7, 15, 1], even if the latter could be considered legitimate
    as well.

  - Functions such as [of_seq], [of_fun], [of_list], etc., raise
    [Invalid_argument] when a term is nonpositive (apart from the very
    first one, which can be any).  But the verification is only
    performed when the term is accessed, so it could be much later, or
    even never.  *)

val zero: t
val one : t

val of_int: int -> t
val of_z  : Z.t -> t
val of_q  : Q.t -> t
val of_float: float -> t

val of_qstring: string -> t
(** Shortcut for [of_q (Q.of_string s)]. *)

val of_seq: Z.t Seq.t -> t
(** Will raise [Invalid_argument] if any term is nonpositive (apart from the
    very first one). *)

val of_fun: (int -> Z.t) -> t
(** The continued fraction [f(0); f(1), f(2), ...].
    Will raise [Invalid_argument] if any term is negative (apart from the
    very first one).
    The terms will stop at the first i>0 such that f(i)=0, if any,
    so that it can also be used to describe a finite continued fraction. *)

val of_list: Z.t list -> t
(** Will raise [Invalid_argument] if any other term is nonpositive
   (apart from the very first one). *)

val of_ilist: int list -> t

val periodic: Z.t list -> (int -> Z.t list) -> t
(** [periodic prefix f] builds the continued fraction obtained by
   appending the lists [prefix], [f 0], [f 1], etc.  Each list must be
   nonempty. The terms must be positive (apart from the very first
   one). Otherwise, [Invalid_argument] is raised. *)

val memo: t -> t
(** Returns an identical CF, but with memoization of intermediate
    computations of terms. Can speed up computations significantly.
    But this trades time for space, as terms are now kept in memory
    instead of being recomputed. *)

(** {2 Homographic functions (Bill Gosper, 1972)} *)

val homography: ?a:Z.t -> ?b:Z.t -> ?c:Z.t -> ?d:Z.t -> t -> t
(** [homography a b c d x] returns (a+bx)/(c+dx).
    Values of a, b, c, d default to 0.
    Will raise [Invalid_argument] if both [c] and [d] are 0. *)

val ihomography: ?a:int -> ?b:int -> ?c:int -> ?d:int -> t -> t

val zadd: Z.t -> t -> t
val iadd: int -> t -> t

val zmul: Z.t -> t -> t
val imul: int -> t -> t
val neg: t -> t

val zdiv: t -> Z.t -> t
val idiv: t -> int -> t

val inv: t -> t
val iinv: int -> t

val bihomography:
  ?a:Z.t -> ?b:Z.t -> ?c:Z.t -> ?d:Z.t ->
  ?e:Z.t -> ?f:Z.t -> ?g:Z.t -> ?h:Z.t ->
  t -> t -> t
(** [bihomography a b c d e f g h x y] returns (a+bx+cy+dxy)/(e+fx+gy+hxy).
    Values of a, b, c, d, e, f, g, h default to 0.
    Will raise [Invalid_argument] if [e], [f], [g], and [h] are all 0. *)

val ibihomography:
  ?a:int -> ?b:int -> ?c:int -> ?d:int ->
  ?e:int -> ?f:int -> ?g:int -> ?h:int ->
  t -> t -> t

val add: t -> t -> t
val sub: t -> t -> t
val mul: t -> t -> t
val div: t -> t -> t
val sqr: t -> t

val generalized: ?a:Z.t -> ?b:Z.t -> ?c:Z.t -> ?d:Z.t -> Z.t Seq.t -> t
(** ...
    Default values for a,b,c,d are 0,1,1,0 (identity). *)

(** {2 Some continued fractions} *)

val phi: t
(** The golden ratio i.e. (1+sqrt(5))/2. *)

val pi: t
(** Does this really need an explanation?
    Already memoized. *)

val e: t
(** Euler's number. *)

val exp_iinv: int -> t
(** exp(1/n) for n>1 *)

val sqrt2: t
(** Square root of 2. *)

val sqrt3: t
(** Square root of 3. *)

val tan1: t
(** tan(1) *)

val tan_iinv: int -> t
(** tan(1/n) for n>1 *)

(** {2 Semi-computable functions}

    When implementing real numbers in a computer, there are several operations
    we cannot implement, such as comparing two numbers, etc. This could run
    forever. Yet, there are cases where the computation could terminate.
    Comparison, for instance, will terminate if the two arguments happen to be
    different.

    Below are some functions in this category. Their running time is
    controlled by a [fuel] argument, which is decreased at each step in the
    internal computation.
    If ever the fuel reaches 0, the computation stops, with answer [CantDecide].
    If ever the answer can be computed before we run out of fuel, it is
    returned as [Sure r] where [r] is the answer.

    The default value for the fuel is 20.

    CAVEAT: It is already possible to obtain non-terminating computations using
    the functions above. For instance, if we build [mul sqrt2 sqrt2],
    any attempt at observing it, even with [floor], will run forever. *)

type 'a semi = Sure of 'a | CantDecide

val compare: ?fuel:int -> t -> t -> int semi
(** [compare x y] returns either [-1] (which means [x < y]), [0] (which means
    [x = y]), or [1] (which means [x > y]). The answer [0] is only possible
    when both [x] and [y] are rational numbers (and that we can figure this
    out before we run out of fuel). *)

val equal: ?fuel:int -> t -> t -> bool semi

val is_rational: ?fuel:int -> t -> bool semi
