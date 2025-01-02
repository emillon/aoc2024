open Stdio

let () =
  for i = 1 to int_of_string Sys.argv.(1) do
    printf
      {|
(executable
 (name run%02d)
 (modules run%02d)
 (libraries aoc2024))

(rule
 (write-file run%02d.ml "let () = Aoc2024.Day%02d.run ()"))

(rule
 (with-stdout-to
  day%02d_p1.txt.gen
  (run ./run%02d.exe --part 1)))

(rule
  (with-stdout-to
   day%02d_p2.txt.gen
   (run ./run%02d.exe --part 2)))
  |}
      i
      i
      i
      i
      i
      i
      i
      i
  done;
  printf
    {|
  (rule
   (alias runtest)
   (action
    (diff ../solutions.txt solutions.txt.gen)))

  (rule
   (with-stdout-to solutions.txt.gen
    (progn
     |};
  for i = 1 to 25 do
    printf
      {|
     (bash "echo -n 'Day %02d part 1: '")
     (cat day%02d_p1.txt.gen)
     (bash "echo -n 'Day %02d part 2: '")
     (cat day%02d_p2.txt.gen)
     |}
      i
      i
      i
      i
  done;
  printf
    {|
     )))
      |}
;;
