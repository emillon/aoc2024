type t = int * int [@@deriving sexp]

include Comparable.S with type t := t
include Hashable.Key with type t := t

val zero : t
val one : t
val i : t
val add : t -> t -> t
val sub : t -> t -> t
val cmul : t -> t -> t
val smul : t -> int -> t
val l1_norm : t -> int
val neighbours4 : t -> t list

type bounding_box =
  { min : t
  ; max : t
  }
[@@deriving sexp]

val in_bounds : bounding_box -> t -> bool
val bounding_box_map : (t, _, comparator_witness) Map.t -> bounding_box
