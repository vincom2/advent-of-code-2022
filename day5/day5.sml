(**************************************************
            [G] [W]         [Q]    
[Z]         [Q] [M]     [J] [F]    
[V]         [V] [S] [F] [N] [R]    
[T]         [F] [C] [H] [F] [W] [P]
[B] [L]     [L] [J] [C] [V] [D] [V]
[J] [V] [F] [N] [T] [T] [C] [Z] [W]
[G] [R] [Q] [H] [Q] [W] [Z] [G] [B]
[R] [J] [S] [Z] [R] [S] [D] [L] [J]
 1   2   3   4   5   6   7   8   9 
****************************************************)

val stack1 = [#"Z", #"V", #"T", #"B", #"J", #"G", #"R"]
val stack2 = [#"L", #"V", #"R", #"J"]
val stack3 = [#"F", #"Q", #"S"]
val stack4 = [#"G", #"Q", #"V", #"F", #"L", #"N", #"H", #"Z"]
val stack5 = [#"W", #"M", #"S", #"C", #"J", #"T", #"Q", #"R"]
val stack6 = [#"F", #"H", #"C", #"T", #"W", #"S"]
val stack7 = [#"J", #"N", #"F", #"V", #"C", #"Z", #"D"]
val stack8 = [#"Q", #"F", #"R", #"W", #"D", #"Z", #"G", #"L"]
val stack9 = [#"P", #"V", #"W", #"B", #"J"]

val read_file = TextIO.inputAll o TextIO.openIn

fun parse_line line = let
      val words = String.tokens Char.isSpace line
      val 6 = List.length words
      val count = Option.valOf (Int.fromString (List.nth (words, 1)))
      val src = Option.valOf (Int.fromString (List.nth (words, 3)))
      val dst = Option.valOf (Int.fromString (List.nth (words, 5)))
    in
      (count, src, dst)
    end

fun process_move f state (count, src, dst) = let
      val src_stack = IntListMap.lookup (state, src)
      val dst_stack = IntListMap.lookup (state, dst)
      val src_stack' = List.drop (src_stack, count)
      val to_be_moved = List.take (src_stack, count)
      val dst_stack' = (f to_be_moved) @ dst_stack
      val state' = IntListMap.insert (state, src, src_stack')
      val state'' = IntListMap.insert (state', dst, dst_stack')
    in
      state''
    end

(* f: crate list -> crate list
 * f tells us what to do with a stack of crates that's to be moved 
 * use List.rev for part 1 and id for part 2 *)
fun process f filename = let
      val text = read_file filename
      val moves = String.tokens (fn c => c = #"\n") text
      val initial_state = IntListMap.insert (IntListMap.empty, 1, stack1)
      val initial_state2 = IntListMap.insert (initial_state, 2, stack2)
      val initial_state3 = IntListMap.insert (initial_state2, 3, stack3)
      val initial_state4 = IntListMap.insert (initial_state3, 4, stack4)
      val initial_state5 = IntListMap.insert (initial_state4, 5, stack5)
      val initial_state6 = IntListMap.insert (initial_state5, 6, stack6)
      val initial_state7 = IntListMap.insert (initial_state6, 7, stack7)
      val initial_state8 = IntListMap.insert (initial_state7, 8, stack8)
      val initial_state9 = IntListMap.insert (initial_state8, 9, stack9)
      val final_state = List.foldl (fn (move, state) => process_move f state (parse_line move)) initial_state9 moves
      val crates = IntListMap.foldl (fn (crate :: _, crates) => crate :: crates) [] final_state
    in
      String.implode (List.rev crates)
    end
