(* literally this is just the values i ended up using from RedBlackMapFn *)
signature GRID_2D = sig
  type 'a grid

  exception Missing

  (* (min_x, max_x) (min_y, max_y) init *)
  val new : (int * int) * (int * int) -> 'a -> 'a grid

  val insert : 'a grid -> (int * int) -> 'a -> 'a grid
  val find : 'a grid -> (int * int) -> 'a option

  (* raises Missing *)
  val lookup : 'a grid -> (int * int) -> 'a

  (* wtf lmao *)
  val foldli : ((int * int) * 'a * 'b -> 'b) -> 'b -> 'a grid -> 'b
end

structure IntPairRedBlackTreeMapGrid : GRID_2D = struct
  exception Missing

  structure IntPair = struct
    type ord_key = int * int

    fun compare ((x1, x2), (y1, y2)) = case Int.compare (x1, y1) of
          EQUAL => Int.compare (x2, y2)
        | cmp => cmp
  end
  structure M = RedBlackMapFn(IntPair)

  type 'a grid = 'a M.map

  (* who cares, we're a map *)
  (* this is technically wrong now that we accept an initialiser element but whatever *)
  fun new _ _ = M.empty

  fun insert grid loc e = M.insert (grid, loc, e)

  fun find grid loc = M.find (grid, loc)

  fun lookup grid loc = M.lookup (grid, loc) handle _ => raise Missing

  val foldli = M.foldli
end


structure ArrayGrid :> GRID_2D = struct
  (* conversion function for coordinates *)
  type 'a grid = 'a array array * (int * int -> int * int)

  exception Missing

  fun new ((min_x, max_x), (min_y, max_y)) e = let
        (* if you have -5 as min and ask for -4, you need to look up 1.
         * so you subtract min_x.
         * max_x is just to know how many to allocate *)
        fun conv (x, y) = (x - min_x, y - min_y)
        (* grid[x][y] - is this row major i forgot the terminology *)
        val grid = Array.tabulate (max_x - min_x + 1, fn _ => Array.array (max_y - min_y + 1, e))
      in
        (grid, conv)
      end

  fun insert (grid, conv) (loc as (x, y)) e = let
        val (x', y') = conv loc
        val col = Array.sub (grid, x')
        val () = Array.update (col, y', e)
      in
        (grid, conv)
      end
      handle _ => raise Missing
  
  fun find (grid, conv) loc = let
        val (x', y') = conv loc
        val col = Array.sub (grid, x')
        val item = Array.sub (col, y')
      in
        SOME item
      end
      handle _ => NONE
  
  fun lookup g (loc as (x', y')) = case find g loc of
        SOME item => item
      | NONE => raise Missing
  
  fun foldli f init (grid, conv) = Array.foldli
      (fn (x, col, acc) => Array.foldli (fn (y, e, acc') => let
                val (x', y') = conv (x, y)
              in f ((x', y'), e, acc') end) acc col) init grid
end
