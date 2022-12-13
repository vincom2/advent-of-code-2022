val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid

structure IntPair = struct
  type ord_key = int * int

  fun compare ((x1, x2), (y1, y2)) = case Int.compare (x1, y1) of
        EQUAL => Int.compare (x2, y2)
      | cmp => cmp
end

structure IntPairMap = RedBlackMapFn(IntPair)
structure I = IntPairMap

structure IntPairSet = RedBlackSetFn(IntPair)
structure S = IntPairSet

fun can_move src dst = let
      val src' = case src of
          #"S" => #"a"
        | #"E" => #"z"
        | _ => src
      val dst' = case dst of
          #"S" => #"a"
        | #"E" => #"z"
        | _ => dst
      val src_n = Char.ord src'
      val dst_n = Char.ord dst'
    in
      dst_n - src_n <= 1
    end

(* climb : S.set -> int I.map -> int I.map -> int * int -> int I.map *)
fun climb visited heightmap stepsmap (loc as (x, y)) =
    if S.member (visited, loc) then (visited, stepsmap) else let
        val visited' = S.add (visited, loc)
        val curr = I.lookup (heightmap, loc)
        val stepsmap = if curr = #"E" then I.insert (stepsmap, loc, 0) else stepsmap
        fun step_to visited'' stepsmap' loc' = case I.find (heightmap, loc') of
              NONE => (visited'', stepsmap') (* invalid loc' *)
            | SOME next => if can_move curr next then let
                  val (visited''', stepsmap'') = climb visited'' heightmap stepsmap' loc'
                  val curr_steps = I.lookup (stepsmap'', loc) handle NotFound => (Option.valOf Int.maxInt) - 1
                  val next_steps = I.lookup (stepsmap'', loc') handle NotFound => (Option.valOf Int.maxInt) - 1
                in (visited''', I.insert (stepsmap'', loc, Int.min (curr_steps, next_steps+1))) end
              else (visited'', stepsmap')
        val (visited_up, stepsmap_up) = step_to visited' stepsmap (x, y-1)
        val (visited_down, stepsmap_down) = step_to visited_up stepsmap_up (x, y+1)
        val (visited_left, stepsmap_left) = step_to visited_down stepsmap_down (x-1, y)
        val (visited'', stepsmap') = step_to visited_left stepsmap_left (x+1, y)
      in
        (visited'', stepsmap')
      end

(* put_row_into_heightmap : char I.map -> int -> char list -> int * char I.map *)
fun put_row_into_heightmap heightmap  y lines = let
      val (_, heightmap') = List.foldl (fn (c, (x, heightmap')) => (x+1, I.insert (heightmap', (x, y), c))) (0, heightmap) lines
    in heightmap' end

(* we run `climb` to quiescence because with the stupid way i wrote it, the number of steps for a position
 * is not necessarily optimal on the first pass through, but if we do it until the `stepsmap` stops changing,
 * then we're fine. *)
(* is the problem that i did a DFS instead of a BFS? lmaoooooooooo *)
(* omg it is. i am so stupid. years of graph theory for this.
 * let's pretend this is DFS-ID but without the "iterative" part *)
(* i also bet if you start from E instead of S then it would just work out but i cannot be bothered anymore *)
fun process' heightmap stepsmap start_pos = let
      val (_, stepsmap') = climb S.empty heightmap stepsmap start_pos
    in
      if I.equiv op= (stepsmap, stepsmap') then stepsmap'
      else process' heightmap stepsmap' start_pos
    end

fun process psp_pred filename = let
      val input = read_file filename
      val lines = String.tokens (fn c => c = #"\n") input
      val lines' = List.map String.explode lines
      val (_, heightmap) = List.foldl (fn (row, (y, heightmap')) => (y+1, put_row_into_heightmap heightmap' y row)) (0, I.empty) lines'
      val possible_start_positions = I.listKeys (I.filter psp_pred heightmap)
      val stepsmap = process' heightmap I.empty (0,0)
    in
      List.foldl (fn (start_pos, curr_max) => Int.min (curr_max, I.lookup (stepsmap, start_pos))) (Option.valOf Int.maxInt) possible_start_positions
    end

fun part1 c = c = #"S"
fun part2 c = c = #"S" orelse c = #"a"