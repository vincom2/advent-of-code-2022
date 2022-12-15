(* let's pretend i intentionally wrote this as a visualiser and did not go down an entirely wrong track at the start *)
structure BadSolution = struct
  open Util
  structure G = IntPairRedBlackTreeMapGrid

  fun parse_line line = let
        val [_, sensor_x, sensor_y, beacon_x, beacon_y] = String.tokens (fn c => c = #"=") line
        val sensor_x' :: _ = String.tokens (fn c => c = #",") sensor_x
        val sensor_y' :: _ = String.tokens (fn c => c = #":") sensor_y
        val beacon_x' :: _ = String.tokens (fn c => c = #",") beacon_x
        val intOf = valOf o Int.fromString
      in
        ((intOf sensor_x', intOf sensor_y'), (intOf beacon_x', intOf beacon_y))
      end
  
  fun manhattan_distance (x1, y1) (x2, y2) = (Int.abs (x2 - x1)) + (Int.abs (y2 - y1))

  datatype space = Sensor | Beacon | Empty | Unknown
  fun space_to_char Sensor = #"S"
    | space_to_char Beacon = #"B"
    | space_to_char Empty = #"#"
    | space_to_char Unknown = #"."

  fun coords_within dist (loc as (x, y)) = let
        val coord_options = List.tabulate (dist * 2, fn d => List.tabulate (dist * 2, fn d' => let
                    val x' = x + d' - dist
                    val y' = y + d - dist
                  in
                    if manhattan_distance loc (x', y') <= dist then SOME (x', y') else NONE
                  end))
        val coord_options' = List.concat coord_options
        val coords = List.filter Option.isSome coord_options'
      in
        List.map Option.valOf coords
      end

  fun mark_empty_up_to grid dist loc = let
        val coords = coords_within dist loc
      in
        List.foldl (fn (coord, grid') => let val () = if coord = (14, 11) then print "oof\n" else () in case G.find grid' coord of
                (* sigh, same bullshit again *)
                NONE => G.insert grid' coord Empty
              | SOME Unknown => G.insert grid' coord Empty
              | SOME _ => grid' end) grid coords
      end
  
  fun process filename interesting_y = let
        val input = read_file filename
        val lines = String.tokens (fn c => c = #"\n") input
        val lines' = List.map parse_line lines
        val empty_grid = G.new ((0, 0), (0, 0)) Unknown (* just use a map and ignore dimensions for now *)
        val grid = List.foldl
          (fn ((sensor, beacon), grid') => let
                val grid'' = G.insert grid' sensor Sensor
              in G.insert grid'' beacon Beacon end) empty_grid lines'
        (* extremely efficient *)
        val grid' = List.foldl
          (fn ((sensor, beacon), grid'') => let
                val dist = manhattan_distance sensor beacon
              in mark_empty_up_to grid'' dist sensor end) grid lines'
        
        val empty_coords = List.map (fn (sensor, beacon) => coords_within (manhattan_distance sensor beacon) sensor) lines'
        val interesting = List.tabulate (21, fn x => case G.find grid' (x, interesting_y) of NONE => #"." | SOME space => space_to_char space)

      in
        print (String.implode interesting); print "\n"
        (* G.foldli (fn ((_, y), e, sum) => if y = interesting_y andalso e = Empty then sum + 1 else sum) 0 grid' *)
        (* List.rev (G.foldli (fn ((_, y), e, row) => if y = interesting_y then e :: row else row) [] grid') *)
      end
end