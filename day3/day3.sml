val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid

structure CharMap = RedBlackMapFn(struct
    type ord_key = char
    val compare = Char.compare
  end)

fun priority item = if Char.isLower item then Char.ord item - 96
    else if Char.isUpper item then Char.ord item - 38
      else raise Invalid

fun compartments rucksack = let
      val rucksack' = String.explode rucksack
      val half = (List.length rucksack') div 2
    in
      (List.take (rucksack', half), List.drop (rucksack', half))
    end

fun process_rucksack rucksack = let
      val (c1, c2) = compartments rucksack
      val c1_map = List.foldl (fn (item, acc) => CharMap.insert (acc, item, ())) CharMap.empty c1
      val c2_map = List.foldl (fn (item, acc) => CharMap.insert (acc, item, ())) CharMap.empty c2
      val both_map = CharMap.intersectWith (fn (_, _) => ()) (c1_map, c2_map)
      val 1 = CharMap.numItems both_map
    in
      case CharMap.firsti both_map of
        NONE => raise Invalid
      | SOME (item, _) => priority item
    end

fun process filename = let
      val text = read_file filename
      val rucksacks = String.tokens (fn c => c = #"\n") text
    in
      List.foldl (fn (rucksack, acc) => acc + process_rucksack rucksack) 0 rucksacks
    end

fun split_into_threes l = let
      val (a, b) = List.foldl
        (fn (rucksack, ([], acc)) => ([rucksack], acc)
          | (rucksack, (curr as [_], acc)) => (rucksack :: curr, acc)
          | (rucksack, (curr as [_, _], acc)) => (rucksack :: curr, acc)
          | (rucksack, (curr, acc)) => ([rucksack], curr :: acc)) ([], []) l
    in
      a :: b
    end

fun process_group [r1, r2, r3] = let
      val r1' = String.explode r1
      val r2' = String.explode r2
      val r3' = String.explode r3
      val r1_map = List.foldl (fn (item, acc) => CharMap.insert (acc, item, ())) CharMap.empty r1'
      val r2_map = List.foldl (fn (item, acc) => CharMap.insert (acc, item, ())) CharMap.empty r2'
      val r3_map = List.foldl (fn (item, acc) => CharMap.insert (acc, item, ())) CharMap.empty r3'
      val nothing = fn (_, _) => ()
      val all_map = CharMap.intersectWith nothing (r3_map, CharMap.intersectWith nothing (r1_map, r2_map))
      val 1 = CharMap.numItems all_map
    in
      case CharMap.firsti all_map of
        NONE => raise Invalid
      | SOME (item, _) => priority item
    end
  | process_group _ = raise Invalid


fun process2 filename = let
      val text = read_file filename
      val rucksacks = String.tokens (fn c => c = #"\n") text
      val groups = split_into_threes rucksacks
    in
      List.foldl (fn (group, acc) => acc + process_group group) 0 groups
    end