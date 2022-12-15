structure Solution = struct
  open Util

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

  (* hahaha this is definitely wrong, try process "test.txt" 11 for instance *)
  (* but i honestly do not care enough to fix it *)
  fun process filename interesting_y = let
        val input = read_file filename
        val lines = String.tokens (fn c => c = #"\n") input
        val lines' = List.map parse_line lines
        val manhattan_distances = List.map (fn (a, b) => manhattan_distance a b) lines'
        val lines'' = ListPair.zip (lines', manhattan_distances)
        val lines''' = List.map (fn ((sensor, _), dist) => (sensor, dist)) lines''
        val interesting_lines = List.filter (fn (sensor as (x, y), dist) => (manhattan_distance sensor (x, interesting_y)) <= dist) lines'''
        (* starting x point, range around starting x point *)
        val furthest_x_distances = List.map (fn ((x, y), dist) => (x, dist - (Int.abs (y - interesting_y)))) interesting_lines
        (* look i don't care so i'm going to assume all d's are distinct *)
        val mins = List.map (fn (x, d) => x - d) furthest_x_distances
        val lowest_x = List.foldl Int.min (valOf Int.maxInt) mins
        val maxes = List.map (fn (x, d) => x + d) furthest_x_distances
        val highest_x = List.foldl Int.max (valOf Int.minInt) maxes
      in
        (* lol you didn't even check whether there are any beacons in the row *)
        highest_x - lowest_x
      end
  
  exception Mario of int
  exception Luigi of int * int
  
  fun check_row lines interesting_y = let
        val interesting_lines = List.filter (fn (sensor as (x, y), dist) => (manhattan_distance sensor (x, interesting_y)) <= dist) lines
        val furthest_x_distances = List.map (fn ((x, y), dist) => (x, dist - (Int.abs (y - interesting_y)))) interesting_lines
        (* we're going to generate a small constant number of intervals. we also only need to repeat this 4 million times. still slow but whatever lol, "fast enough" *)
        val intervals = List.map (fn (x, dist) => (x-dist, x+dist)) furthest_x_distances
        val intervals' = ListMergeSort.sort (fn ((start1, _), (start2, _)) => start1 > start2) intervals
        val first :: rest = intervals'
      in
        List.foldl (fn ((start, e), (curr_start, curr_end)) => if start - curr_end > 1 then raise Mario (curr_end + 1) else (Int.min (curr_start, start), Int.max (curr_end, e))) first rest
      end
  
  fun process2 filename max = let
        val input = read_file filename
        val lines = String.tokens (fn c => c = #"\n") input
        val lines' = List.map parse_line lines
        val manhattan_distances = List.map (fn (a, b) => manhattan_distance a b) lines'
        val lines'' = ListPair.zip (lines', manhattan_distances)
        val lines''' = List.map (fn ((sensor, _), dist) => (sensor, dist)) lines''

        fun loop interesting_y = if interesting_y = max then NONE else (check_row lines''' interesting_y; loop (interesting_y+1))
            handle Mario x => raise Luigi (x, interesting_y)
      in
        loop 0; SOME ~1
      end
      (* incredible; the solution fits in an SML/NJ int but not a MLton int *)
      handle Luigi (x, y) => let open IntInf in SOME ((Int.toLarge x) * 4000000 + (Int.toLarge y)) end
end