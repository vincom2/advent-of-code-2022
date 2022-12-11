val read_file = TextIO.inputAll o TextIO.openIn

val split_into_elves' = List.foldl
  (fn (curr, (curr_elf, acc)) => case Int.fromString curr of
        SOME n => (n :: curr_elf, acc)
      | NONE => ([], curr_elf :: acc))
  ([], [])
val split_into_elves = List.rev o #2 o split_into_elves'

fun process filename = let
      val text = read_file filename
      val strings = String.fields (fn c => c = #"\n") text
      val elves = split_into_elves strings
      val indexed_elves = ListPair.zip (elves, List.tabulate (List.length elves, fn n => n))
    in
      List.foldl (fn ((elf, idx), acc as (curr_max, curr_max_idx)) => let
            val weight = List.foldl op+ 0 elf
          in
            if weight > curr_max then (weight, idx) else acc
          end) (0, 0) indexed_elves
    end

fun process2 filename = let
      val text = read_file filename
      val strings = String.fields (fn c => c = #"\n") text
      val elves = split_into_elves strings
      val indexed_elves = ListPair.zip (elves, List.tabulate (List.length elves, fn n => n))
    in
      List.foldl (fn ((elf, idx), acc as (curr_max1, curr_max2, curr_max3)) => let
            val weight = List.foldl op+ 0 elf
          in
            if weight > curr_max1 then (weight, curr_max1, curr_max2)
            else if weight > curr_max2 then (curr_max1, weight, curr_max2)
              else if weight > curr_max3 then (curr_max1, curr_max2, weight)
                else acc
          end) (0, 0, 0) indexed_elves
    end
