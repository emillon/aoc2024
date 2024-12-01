val number : int Angstrom.t
val signed_number : int Angstrom.t
val word : string Angstrom.t
val parse_using : 'a Angstrom.t -> string -> 'a
val parse_lines_using : 'a Angstrom.t -> string -> 'a list
val enum : (string * 'a) list -> 'a Angstrom.t
