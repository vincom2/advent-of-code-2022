val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid
exception Invalid2 of int * int

structure IntPair = struct
  type ord_key = int * int

  fun compare ((x1, x2), (y1, y2)) = case Int.compare (x1, y1) of
        EQUAL => Int.compare (x2, y2)
      | cmp => cmp
end

structure IntPairSet = RedBlackSetFn(IntPair)
structure I = IntPairSet

datatype Move = Right of int | Up of int | Left of int | Down of int | UpLeft | UpRight | DownLeft | DownRight

type position = int * int (* row x col *)
type positions = position * position (* head x tail *)

fun close_enough ((h1, h2), (t1, t2)) = Int.abs (h1 - t1) < 2 andalso Int.abs (h2 - t2) < 2

fun what_did_tail_do ((t1, t2), (t1', t2')) =
    case (t1' - t1, t2' - t2) of
      (~1, ~1) => DownLeft
    | (~1, 0) => Down 1
    | (~1, 1) => DownRight
    | (0, ~1) => Left 1
    | (0, 0) => raise Invalid
    | (0, 1) => Right 1
    | (1, ~1) => UpLeft
    | (1, 0) => Up 1
    | (1, 1) => UpRight
    | (x, y) => raise Invalid2 (x, y)

(* move : IntPairSet.set -> positions -> Move -> Move list -> IntPairSet.set * positions * Move list *)
fun move tail_locations (state as ((h1, h2), tail as (t1, t2))) mv tail_moves = case mv of
      Right n => if n = 0 then (tail_locations, state, tail_moves)
        else let
            val head' = (h1, h2+1)
            val state' = (head', tail)
          in
            if close_enough state' then move tail_locations state' (Right (n-1)) tail_moves
            else let
                val tail' = (h1, t2+1)
                val tail_locations' = I.add (tail_locations, tail')
                val state'' = (head', tail')
                val tail_move = what_did_tail_do (tail, tail')
              in move tail_locations' state'' (Right (n-1)) (tail_move :: tail_moves) end
          end
    | Left n => if n = 0 then (tail_locations, state, tail_moves)
      else let
          val head' = (h1, h2-1)
          val state' = (head', tail)
        in
          if close_enough state' then move tail_locations state' (Left (n-1)) tail_moves
          else let
              val tail' = (h1, t2-1)
              val tail_locations' = I.add (tail_locations, tail')
              val state'' = (head', tail')
              val tail_move = what_did_tail_do (tail, tail')
            in move tail_locations' state'' (Left (n-1)) (tail_move :: tail_moves) end
        end
    | Up n => if n = 0 then (tail_locations, state, tail_moves)
      else let
          val head' = (h1+1, h2)
          val state' = (head', tail)
        in
          if close_enough state' then move tail_locations state' (Up (n-1)) tail_moves
          else let
              val tail' = (t1+1, h2)
              val tail_locations' = I.add (tail_locations, tail')
              val state'' = (head', tail')
              val tail_move = what_did_tail_do (tail, tail')
            in move tail_locations' state'' (Up (n-1)) (tail_move :: tail_moves) end
        end
    | Down n => if n = 0 then (tail_locations, state, tail_moves)
      else let
          val head' = (h1-1, h2)
          val state' = (head', tail)
        in
          if close_enough state' then move tail_locations state' (Down (n-1)) tail_moves
          else let
              val tail' = (t1-1, h2)
              val tail_locations' = I.add (tail_locations, tail')
              val state'' = (head', tail')
              val tail_move = what_did_tail_do (tail, tail')
            in move tail_locations' state'' (Down (n-1)) (tail_move :: tail_moves) end
        end
    | UpLeft => let
        val head' = (h1+1, h2-1)
        val state' = (head', tail)
      in
        if close_enough state' then (tail_locations, state', tail_moves)
        else let
            val tail' = (Int.min (#1 head', t1+1),Int.max (t2-1, #2 head'))
            val tail_locations' = I.add (tail_locations, tail')
            val state'' = (head', tail')
            val tail_move = what_did_tail_do (tail, tail')
          in (tail_locations', state'', [tail_move]) end
      end
    | UpRight => let
        val head' = (h1+1, h2+1)
        val state' = (head', tail)
      in
        if close_enough state' then (tail_locations, state', tail_moves)
        else let
            val tail' = (Int.min (#1 head', t1+1), Int.min (t2+1, #2 head'))
            val tail_locations' = I.add (tail_locations, tail')
            val state'' = (head', tail')
            val tail_move = what_did_tail_do (tail, tail')
          in (tail_locations', state'', [tail_move]) end
      end
    | DownLeft => let
        val head' = (h1-1, h2-1)
        val state' = (head', tail)
      in
        if close_enough state' then (tail_locations, state', tail_moves)
        else let
            val tail' = (Int.max (#1 head', t1-1), Int.max (t2-1, #2 head'))
            val tail_locations' = I.add (tail_locations, tail')
            val state'' = (head', tail')
            val tail_move = what_did_tail_do (tail, tail')
          in (tail_locations', state'', [tail_move]) end
      end
    | DownRight => let
        val head' = (h1-1, h2+1)
        val state' = (head', tail)
      in
        if close_enough state' then (tail_locations, state', tail_moves)
        else let
            val tail' = (Int.max (#1 head', t1-1), Int.min (t2+1, #2 head'))
            val tail_locations' = I.add (tail_locations, tail')
            val state'' = (head', tail')
            val tail_move = what_did_tail_do (tail, tail')
          in (tail_locations', state'', [tail_move]) end
      end

fun line_to_move line = let
      val [move, count'] = String.tokens Char.isSpace line
      val count = Option.valOf (Int.fromString count')
    in case move of
        "U" => Up count
      | "D" => Down count
      | "L" => Left count
      | "R" => Right count
    end

val process_moves = List.foldl
  (fn (mv, (tail_locations, state, tail_moves)) => let
        val (tail_locations', state', tail_moves') = move tail_locations state mv []
      in (tail_locations', state', tail_moves' @ tail_moves) end)

fun process filename = let
      val text = read_file filename
      val lines = String.tokens (fn c => c = #"\n") text
      val tl_init = I.add (I.empty, (0,0))
      val (tail_locations, _, _) = process_moves (tl_init, ((0,0),(0,0)), []) (List.map line_to_move lines)
    in I.numItems tail_locations end

fun process2 filename = let
      val text = read_file filename
      val lines = String.tokens (fn c => c = #"\n") text
      val tl_init = I.add (I.empty, (0,0))
      val knots = List.tabulate (9, fn _ => (0,0))
      val (tail_locations, _, _) = List.foldl
        (fn (_, (_, _, moves)) => let
              val (tail_locations', (_, tail'), moves') = process_moves (tl_init, ((0,0), (0,0)), []) moves
            in (tail_locations', tail', List.rev moves') end)
        (tl_init, (0,0), List.map line_to_move lines) knots
    in I.numItems tail_locations end