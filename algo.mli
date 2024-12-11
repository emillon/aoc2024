val sum : int list -> int
val product : int list -> int
val permutations : 'a list -> 'a list list
val sublists : 'a list -> 'a list list
val legs : 'a list -> ('a * 'a) list
val iter_bytes : f:(bytes -> bool) -> start:string -> min:char -> max:char -> string
val guard : bool -> unit list
val gcd : int -> int -> int
val lcm : int -> int -> int

(** Return g = gcd, s, t such that as+bt = g *)
val egcd : int -> int -> int * int * int

val sole : 'a list -> 'a
val digits10 : int -> int
