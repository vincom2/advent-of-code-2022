val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid

structure M = IntRedBlackMap

datatype instruction =
  Noop
| Addx of int

fun line_to_instruction line = case String.tokens Char.isSpace line of
      [_] => Noop
    | [_, s] => Addx (Option.valOf (Int.fromString s))
    | _ => raise Invalid

fun process_instruction (inst, (cycle, reg_x, values)) = case inst of
      Noop => let
          val values' = M.insert (values, cycle, reg_x)
        in (cycle + 1, reg_x, values') end
    | Addx n => let
        val values' = M.insert (values, cycle, reg_x)
        val values'' = M.insert (values', cycle + 1, reg_x)
      in (cycle + 2, reg_x + n, values'') end

val interesting = [20, 60, 100, 140, 180, 220]

fun process filename = let
      val input = read_file filename
      val lines = String.tokens (fn c => c = #"\n") input
      val instructions = List.map line_to_instruction lines
      val (_, _, values) = List.foldl process_instruction (1, 1, M.empty) instructions
    in
      List.foldl (fn (cycle, acc) => acc + (M.lookup (values, cycle) * cycle)) 0 interesting
    end

fun lit pixel reg_x = pixel = reg_x orelse pixel = reg_x - 1 orelse pixel = reg_x + 1
  
fun process2 filename = let
      val input = read_file filename
      val lines = String.tokens (fn c => c = #"\n") input
      val instructions = List.map line_to_instruction lines
      val (_, _, values) = List.foldl process_instruction (1, 1, M.empty) instructions
      val (_, crt) = M.foldl (fn (reg_x, (cycle, crt)) => if lit (cycle mod 40) reg_x then (cycle+1, #"#" :: crt) else (cycle+1, #"." :: crt)) (0, []) values
    in
      List.rev crt
    end

fun scan' width curr crt = case crt of
      [] => ()
    | c :: cs => if curr < width then let
          val () = print (Char.toString c)
        in scan' width (curr + 1) cs end
      else let
          val () = print "\n"
        in scan' width 0 crt end

fun scan width crt = (scan' width 0 crt; print "\n")