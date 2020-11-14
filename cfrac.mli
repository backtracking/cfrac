
type t
(** The type of a continued fraction. *)

val int_part: t -> Z.t
(** The integer part i.e. the very first term .*)

val convergents: t -> Q.t Seq.t

val print: Format.formatter -> t -> unit
(** Print a continued fraction as [a0;a1,a2,...] up to some term given by
    some internal precision (5 by default, but can be changed using
    function [set_print_precision] below).
    If there are more terms, an ellipsis "..." is printed.
    Otherwise, the list ends with the last term. *)

val set_print_precision: int -> unit
(** Set the precision used by [print]. *)

(** {2 Some continued fractions} *)

val phi: t
(** the golden ratio i.e. (1+sqrt(5))/2 *)

val pi: t
(** Only contains the first 100 terms of the continued fraction of pi.
    Fails if we try to access terms beyond. *)

