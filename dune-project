(lang dune 3.15)

(package (name aoc2024))

(dialect
 (name hex)
 (implementation
  (extension hex)
  (preprocess (run input/input.exe decode %{input-file}))))
