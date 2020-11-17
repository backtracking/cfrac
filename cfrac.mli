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

    This is one (cute) way of implementing real numbers.

    The current implementation is limited to nonnegative real numbers.
    (It would be easy to add a sign on top of this code.)
*)

type t
(** The type of a continued fraction. *)

val terms: t -> Z.t Seq.t
(** The terms of the continued fraction. *)

val floor: t -> Z.t
(** The floor function.
    This is the very first term of the continued fraction. *)

val convergents: t -> Q.t Seq.t
(** The successive convergents of the continued fraction.
    Each convergent is in its lowest terms.
    The very first convergent is the integer part, and thus has
    denominator 1. *)

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
val to_float1: t -> float
val to_float2: t -> float
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
    Otherwise, the list ends with the last decimal. *)

val print_convergents: prec:int -> Format.formatter -> t -> unit
(** Print the sequence of convergents, up to [prec] fractions.
    If there are more convergents to come, an ellipsis "..." is printed.
    Otherwise, the list ends with the last convergent.

    Merely a convenient wrapper over [convergents]. *)

(** {2 Some constructors}

  Notes:

  - Functions such as [of_int], [of_z], etc., raise [Invalid_argument]
    if they are called with a negative argument.

  - Function [of_q] returns a finite sequence --- rational numbers are exactly
    the real numbers with finite continued fractions --- whose last term is not 1.
    For instance, the continued fraction of 355/113 will be [3; 7, 16] and
    not [3; 7, 15, 1], even if the latter could be considered legitimate as well.

  - Functions such as [of_seq], [of_fun], [of_list], etc., raise
    [Invalid_argument] when a term is nonpositive (apart from the very
    first one, which can be zero).  But the verification is only
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
(** Will raise [Invalid_argument] if the first term is negative,
    or if any of the next terms is nonpositive. *)

val of_fun: (int -> Z.t) -> t
(** The continued fraction [f(0); f(1), f(2), ...].
    Will raise [Invalid_argument] if any term is negative.
    The terms will stop at the first i>0 such that f(i)=0, if any,
    so that it can also be used to describe a finite continued fraction. *)

val of_list: Z.t list -> t
(** Will raise [Invalid_argument] if the first term is negative or if any
    other term is nonpositive. *)

val of_ilist: int list -> t

val periodic: Z.t list -> (int -> Z.t list) -> t
(** [periodic prefix f] builds the continued fraction obtained by appending
    the lists [prefix], [f 0], [f 1], etc.
    Each list must be nonempty. The first term must be nonnegative, and next
    terms must be positive. Otherwise, [Invalid_argument] is raised. *)

(** {2 Homographic functions (Bill Gosper, 1972)} *)

val homography: ?a:Z.t -> ?b:Z.t -> ?c:Z.t -> ?d:Z.t -> t -> t
(** [homography a b c d x] returns (a+bx)/(c+dx).
    Values of a, b, c,d default to 0.
    Will raise [Invalid_argument] if both [c] and [d] are 0. *)

val ihomography: ?a:int -> ?b:int -> ?c:int -> ?d:int -> t -> t

val zadd: Z.t -> t -> t
val iadd: int -> t -> t

val zmul: Z.t -> t -> t
val imul: int -> t -> t

val zdiv: t -> Z.t -> t
val idiv: t -> int -> t

val inv: t -> t

val bihomography:
  ?a:Z.t -> ?b:Z.t -> ?c:Z.t -> ?d:Z.t ->
  ?e:Z.t -> ?f:Z.t -> ?g:Z.t -> ?h:Z.t ->
  t -> t -> t
(** [bihomography a b c d e f g h x y] returns (a+bx+cy+dxy)/(e+fx+gy+hxy).
    Will raise [Invalid_argument] if [e], [f], [g], and [h] are all 0. *)

val ibihomography:
  ?a:int -> ?b:int -> ?c:int -> ?d:int ->
  ?e:int -> ?f:int -> ?g:int -> ?h:int ->
  t -> t -> t

val add: t -> t -> t
val sub: t -> t -> t
val mul: t -> t -> t
val div: t -> t -> t

(** {2 Some continued fractions} *)

val phi: t
(** The golden ratio i.e. (1+sqrt(5))/2. *)

val pi: t
(** Only contains the first 100 terms of the continued fraction of pi.
    Fails if we try to access terms beyond. *)

val e: t
(** Euler's number. *)

val sqrt2: t
(** Square root of 2. *)

val sqrt3: t
(** Square root of 3. *)

(** {2 Semi-computable functions}

    When implementing real numbers in a computer, there are several operations we
    cannot implement, such as comparing two numbers, etc. This could run forever.
    Yet, there are cases where the computation could terminate. Comparison, for
    instance, will terminate if the two arguments happen to be different.

    Below are some functions in this category. Their running time is controlled with
    a [fuel] argument, which is decreased at each step in the internal computation.
    If ever the fuel reaches 0, the computation stops, with answer [CantDecide].
    If ever the answer can be computed before we run out of fuel, it is returned
    as [Sure r] where [r] is the answer.

    The default value for the fuel is 20.

    CAVEAT: It is already possible to obtain non-terminating computations using
    the functions above. For instance, if we build [mul sqrt2 sqrt2], any attempt
    at observing it, even with [floor], will run forever.
*)

type 'a semi = Sure of 'a | CantDecide

val compare: ?fuel:int -> t -> t -> int semi
(** [compare x y] returns either [-1] (which means [x < y]), [0] (which means [x = y]),
    or [1] (which means [x > y]). The answer [0] is only possible when both [x] and
    [y] are rational numbers (and that we can figure this out before we run out of
    fuel). *)

val equal: ?fuel:int -> t -> t -> bool semi

val is_rational: ?fuel:int -> t -> bool semi
