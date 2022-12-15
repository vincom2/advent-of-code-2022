local
  open Solution
in
  fun main () = let
        val [filename, n, part] = CommandLine.arguments ()
        val n' = valOf (Int.fromString n)
        val output = if part = "part1" then Int.toLarge (process filename n')
        else if part = "part2" then valOf (process2 filename n')
        else raise Fail part
      in
        print (IntInf.toString output); print "\n"
      end
end

val () = main ()