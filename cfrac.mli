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

val int_part: t -> Z.t
(** The integer part of the continued fraction, i.e. the very first term .*)

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
    continued fraction, and thus is identical to [int_part]. *)

val print: Format.formatter -> t -> unit
(** Print a continued fraction as "[a0; a1, a2, ...]" up to some term given by
    some internal precision (5 by default, but this can be changed using
    function [set_print_precision] below).
    If there are more terms, an ellipsis "..." is printed.
    Otherwise, the list ends with the last term. *)

val set_print_precision: int -> unit
(** Set the precision used by [print]. *)

(** {2 Some constructors}

  Notes:

  - Functions such as [of_int], [of_z], etc., raise [Invalid_argument]
    if they are called with a negative argument.

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

val periodic: Z.t list -> (int -> Z.t list) -> t
(** [periodic prefix f] builds the continued fraction obtained by appending
    the lists [prefix], [f 0], [f 1], etc.
    Each list must be nonempty. The first term must be nonnegative, and next
    terms must be positive. Otherwise, [Invalid_argument] is raised. *)

(** {2 Homographic functions (Bill Gosper, 1972)} *)

val homography: ?a:Z.t -> ?b:Z.t -> ?c:Z.t -> ?d:Z.t -> t -> t
(** [homography a b c d x] returns (a+bx)/(c+dx).
    Values of a, b, c,d default to 0. *)

val ihomography: ?a:int -> ?b:int -> ?c:int -> ?d:int -> t -> t

val inv: t -> t

val zmul: Z.t -> t -> t
val imul: int -> t -> t

val zdiv: t -> Z.t -> t
val idiv: t -> int -> t

(*
val bihomography:
  ?a:Z.t -> ?b:Z.t -> ?c:Z.t -> ?d:Z.t ->
  ?e:Z.t -> ?f:Z.t -> ?g:Z.t -> ?h:Z.t ->
  t -> t -> t
 *)
(** [bihomography a b c d e f g h x y] returns (a+bx+cy+dxy)/(e+fx+gy+hxy). *)

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
