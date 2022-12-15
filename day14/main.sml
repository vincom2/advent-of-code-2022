(* i want to time this but am too lazy to figure out how to use the Timer module *)

local
  open ArraySolution
  (* open MapSolution *)
in
  fun main () = let
        val [filename, part] = CommandLine.arguments ()
        val output = if part = "part1" then process filename else process2 filename
      in
        print (Int.toString output)
      end
end

val () = main ()