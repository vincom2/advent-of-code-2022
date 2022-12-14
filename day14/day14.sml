infixr 0 $
fun f $ x = f x

val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid

structure IntPair = struct
  type ord_key = int * int

  fun compare ((x1, x2), (y1, y2)) = case Int.compare (x1, y1) of
        EQUAL => Int.compare (x2, y2)
      | cmp => cmp
end

structure IntPairMap = RedBlackMapFn(IntPair)
structure M = IntPairMap

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

(* fill_line : object M.map -> point -> point -> object M.map *)
fun fill_line grid (x1, y1) (x2, y2) = let
      fun fill_vertical grid (start as (x, y)) n curr = if curr > n then grid
          else let
              val grid' = M.insert (grid, (x, y+curr), Rock)
            in
              fill_vertical grid' start n (curr+1)
            end
      fun fill_horizontal grid (start as (x, y)) n curr = if curr > n then grid
          else let
              val grid' = M.insert (grid, (x+curr, y), Rock)
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
datatype step = Moved of point * object M.map | Resting of object M.map
  
fun step grid (sand as (x, y)) = let
      val down = (x, y+1)
      val downleft = (x-1, y+1)
      val downright = (x+1, y+1)
    in
      case M.find (grid, down) of
        (* ugh i should just use or-patterns but they piss off my autoformatter LOL *)
        NONE => let
            val grid' = M.insert (grid, down, Sand)
            val grid'' = M.insert (grid', sand, Empty)
          in Moved (down, grid'') end
      | SOME Empty => let
          val grid' = M.insert (grid, down, Sand)
          val grid'' = M.insert (grid', sand, Empty)
        in Moved (down, grid'') end
      | SOME _ => (case M.find (grid, downleft) of
            NONE => let
                val grid' = M.insert (grid, downleft, Sand)
                val grid'' = M.insert (grid', sand, Empty)
              in Moved (downleft, grid'') end
          | SOME Empty => let
              val grid' = M.insert (grid, downleft, Sand)
              val grid'' = M.insert (grid', sand, Empty)
            in Moved (downleft, grid'') end
          | SOME _ => (case M.find (grid, downright) of
                NONE => let
                    val grid' = M.insert (grid, downright, Sand)
                    val grid'' = M.insert (grid', sand, Empty)
                  in Moved (downright, grid'') end
              | SOME Empty => let
                  val grid' = M.insert (grid, downright, Sand)
                  val grid'' = M.insert (grid', sand, Empty)
                in Moved (downright, grid'') end
              | SOME _ => Resting grid))
    end

fun process filename = let
      val input = read_file filename
      val lines = String.tokens (fn c => c = #"\n") input
      val rocks = List.map line_to_rock lines
      val grid_with_source = M.insert (M.empty, (500,0), Source)
      val grid = List.foldl (fn (rock, grid') => fill_grid_with_rock grid' rock) grid_with_source rocks
      (* surely they wouldn't give us rocks that are all above the source *)
      val lowest_y = M.foldli (fn ((_, y), _, curr) => Int.max (y, curr)) (Option.valOf Int.minInt) grid
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
      val grid_with_source = M.insert (M.empty, (500,0), Source)
      val grid = List.foldl (fn (rock, grid') => fill_grid_with_rock grid' rock) grid_with_source rocks
      (* surely they wouldn't give us rocks that are all above the source *)
      val lowest_y = M.foldli (fn ((_, y), _, curr) => Int.max (y, curr)) (Option.valOf Int.minInt) grid
      val floor = lowest_y + 2
      fun simulate grid source n = if M.lookup (grid, (500, 0)) = Sand then n
          else
            case step2 grid source floor of
              Moved (source' as (x, y), grid') =>
                simulate grid' source' n
            | Resting grid' => if source = (500, 0) then n
              else simulate grid' (500, 0) (n+1)
    in
      (simulate grid (500, 0) 0) + 1 (* last grain of sand blocks the source, whatever *)
    end