structure Util = struct
  infixr 0 $
  fun f $ x = f x

  val read_file = TextIO.inputAll o TextIO.openIn

  exception Invalid
end

functor Solution(G : GRID_2D) = struct
  open Util

  type point = int * int (* (x, y) *)
  type rock = point list

  fun parse_point point = let
        val point' = String.tokens (fn c => c = #",") point
        val [x, y] = List.map (Option.valOf o Int.fromString) point'
      in (x, y) end

  fun line_to_rock line = let
        val items = String.tokens Char.isSpace line
        val items' = List.filter (fn item => item <> "->") items
      in
        List.map parse_point items'
      end

  (* if coord doesn't exist, then Empty *)
  datatype object = Rock | Sand | Empty | Source

  (* fill_line : object G.grid -> point -> point -> object G.grid *)
  fun fill_line grid (x1, y1) (x2, y2) = let
        fun fill_vertical grid (start as (x, y)) n curr = if curr > n then grid
            else let
                val grid' = G.insert grid (x, y+curr) Rock
              in
                fill_vertical grid' start n (curr+1)
              end
        fun fill_horizontal grid (start as (x, y)) n curr = if curr > n then grid
            else let
                val grid' = G.insert grid (x+curr, y) Rock
              in
                fill_horizontal grid' start n (curr+1)
              end
      in
        if x1 = x2 then let
            val head = Int.min (y1, y2)
            val tail = Int.max (y1, y2)
          in
            fill_vertical grid (x1, head) (tail - head) 0
          end
        else let
            val head = Int.min (x1, x2)
            val tail = Int.max (x1, x2)
          in
            fill_horizontal grid (head, y1) (tail - head) 0
          end
      end

  (* fill_grid_with_rock : object M.map -> rock -> object M.map *)
  fun fill_grid_with_rock grid [] = grid
    | fill_grid_with_rock grid [_] = grid
    | fill_grid_with_rock grid (p :: ps) = let
        val (_, grid') = List.foldl (fn (to, (from, grid')) => (to, fill_line grid' from to)) (p, grid) ps
      in grid' end

  (* Moved (new sand location, grid)
   * Resting (grid) *)
  datatype step = Moved of point * object G.grid | Resting of object G.grid
  
  fun step grid (sand as (x, y)) = let
        val down = (x, y+1)
        val downleft = (x-1, y+1)
        val downright = (x+1, y+1)
      in
        case G.find grid down of
          (* ugh i should just use or-patterns but they piss off my autoformatter LOL *)
          NONE => let
              val grid' = G.insert grid down Sand
              val grid'' = G.insert grid' sand Empty
            in Moved (down, grid'') end
        | SOME Empty => let
            val grid' = G.insert grid down Sand
            val grid'' = G.insert grid' sand Empty
          in Moved (down, grid'') end
        | SOME _ => (case G.find grid downleft of
              NONE => let
                  val grid' = G.insert grid downleft Sand
                  val grid'' = G.insert grid' sand Empty
                in Moved (downleft, grid'') end
            | SOME Empty => let
                val grid' = G.insert grid downleft Sand
                val grid'' = G.insert grid' sand Empty
              in Moved (downleft, grid'') end
            | SOME _ => (case G.find grid downright of
                  NONE => let
                      val grid' = G.insert grid downright Sand
                      val grid'' = G.insert grid' sand Empty
                    in Moved (downright, grid'') end
                | SOME Empty => let
                    val grid' = G.insert grid downright Sand
                    val grid'' = G.insert grid' sand Empty
                  in Moved (downright, grid'') end
                | SOME _ => Resting grid))
      end

  fun process filename = let
        val input = read_file filename
        val lines = String.tokens (fn c => c = #"\n") input
        val rocks = List.map line_to_rock lines
        val empty_grid = G.new ((0, 1000), (0, 500)) Empty
        val grid_with_source = G.insert empty_grid (500,0) Source
        val grid = List.foldl (fn (rock, grid') => fill_grid_with_rock grid' rock) grid_with_source rocks

        (* check cave properties here *)
        (* oof *)
        val max_rocks = List.map 
          (List.foldl (fn ((x, y), (max_x, max_y)) => (Int.max (x, max_x), Int.max (y, max_y))) (valOf Int.minInt, valOf Int.minInt)) rocks
        val (_, lowest_y) =
          List.foldl (fn ((x, y), (max_x, max_y)) => (Int.max (x, max_x), Int.max (y, max_y))) (valOf Int.minInt, valOf Int.minInt) max_rocks

        fun simulate grid source n =
            case step grid source of
              Moved (source' as (_, y), grid') => if y > lowest_y then n
                else simulate grid' source' n
            | Resting grid' => simulate grid' (500, 0) (n+1)
      in
        simulate grid (500, 0) 0
      end

  fun step2 grid (sand as (_, y)) floor = if y + 1 = floor then Resting grid else step grid sand

  fun process2 filename = let
        val input = read_file filename
        val lines = String.tokens (fn c => c = #"\n") input
        val rocks = List.map line_to_rock lines

        (* check cave properties here *)
        (* oof *)
        val max_rocks = List.map 
          (List.foldl (fn ((x, y), (max_x, max_y)) => (Int.max (x, max_x), Int.max (y, max_y))) (valOf Int.minInt, valOf Int.minInt)) rocks
        val (rightest_x, lowest_y) =
          List.foldl (fn ((x, y), (max_x, max_y)) => (Int.max (x, max_x), Int.max (y, max_y))) (valOf Int.minInt, valOf Int.minInt) max_rocks
        (* oofie *)
        val x_init = rightest_x + lowest_y

        val empty_grid = G.new ((~x_init, x_init), (~lowest_y - 2, lowest_y + 2)) Empty
        val grid_with_source = G.insert empty_grid (500,0) Source
        val grid = List.foldl (fn (rock, grid') => fill_grid_with_rock grid' rock) grid_with_source rocks

        val floor = lowest_y + 2
        fun simulate grid source n =
            case step2 grid source floor of
              Moved (source' as (x, y), grid') =>
                simulate grid' source' n
            | Resting grid' => if source = (500, 0) then n
              else simulate grid' (500, 0) (n+1)
      in
        (simulate grid (500, 0) 0) + 1 (* last grain of sand blocks the source, whatever *)
      end
end

structure MapSolution = Solution(IntPairRedBlackTreeMapGrid)

(* this is very clearly not the way to use arrays. you should probably keep a ref
 * in the simulate function (or somewhere else, idk i didn't think very hard) and update the array.
 * but i have no interest in designing that solution and shared interface for the grid.
 * Anyway, just note that the ArraySolution is unusably slow in SML/NJ lmao.
 * it's significantly faster than the MapSolution in MLton though.
 * Maybe using the ref will make it usable in SML/NJ but i do not care. *)
structure ArraySolution = Solution(ArrayGrid)