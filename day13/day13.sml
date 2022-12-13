infixr 0 $
fun f $ x = f x

val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid

structure Data = struct
  datatype t = List of t list | Integer of int

  fun compare (Integer left, Integer right) = Int.compare (left, right)
    | compare (List [], List []) = EQUAL
    | compare (List [], List _) = LESS
    | compare (List _, List []) = GREATER
    | compare (List (l::ls), List (r::rs)) = (case compare (l, r) of
          EQUAL => compare (List ls, List rs)
        | ord => ord)
    | compare (left as (Integer _), right as (List _)) = compare (List [left], right)
    | compare (left as (List _), right as (Integer _)) = compare (left, List [right])
end
open Data

fun parse_int chars = let
      val parse_curr_n = Option.valOf o Int.fromString o String.implode o List.rev
      fun parse_int' _ [] = raise Invalid
        | parse_int' acc (chars' as (c :: cs)) =
          if Char.isDigit c then parse_int' (c :: acc) cs
          else (parse_curr_n acc, chars')
    in
      parse_int' [] chars
    end

(* give it a list of characters without the initial '[' *)
(* parse_list : char list -> data list * char list *)
fun parse_list chars = let
      (* parse_list' : data list -> int option -> char list -> data list * char list *)
      fun parse_list' acc curr_n chars' = let
          in case chars' of
              [] => raise Invalid
            | (c :: cs) => if Char.isDigit c then let
                  val (n, chars'') = parse_int chars'
                in parse_list' acc (SOME n) chars'' end
              else case (c, curr_n) of
                  (#"]", SOME n) => ((Integer n) :: acc, cs)
                | (#"]", NONE) => (acc, cs)
                | (#",", SOME n) => parse_list' ((Integer n) :: acc) NONE cs
                | (#",", NONE) => parse_list' acc NONE cs
                | (#"[", NONE) => let
                    val (list, cs') = parse_list cs
                  in
                    parse_list' (List list :: acc) NONE cs'
                  end
                | _ => raise Invalid
          end
      val (list, cs) = parse_list' [] NONE chars
    in
      (List.rev list, cs)
    end

fun parse_line line = let
      val chars = String.explode line
      val chars' = List.drop (chars, 1) (* drop the first `[' *)
      fun parse_line' acc chars = case chars of
            [] => acc
          | (#"[" :: cs) => let
              val (list, cs') = parse_list cs
            in
              parse_line' ((List list) :: acc) cs'
            end
          | (#"," :: cs) => parse_line' acc cs
          | [#"]"] => acc
          | _ => let
              val (n, chars') = parse_int chars
            in
              parse_line' ((Integer n) :: acc) chars'
            end
    in
      List $ List.rev $ parse_line' [] chars'
    end

fun process filename = let
      val input = read_file filename
      val lines = String.fields (fn c => c = #"\n") input
      val (_, packets') = List.foldl (fn (line, (pair, acc)) =>
            if line = "" then let val [left, right] = List.rev pair in ([], (left, right) :: acc) end
            else let val line' = parse_line line in (line' :: pair, acc) end) ([], []) lines
      val packets = List.rev packets'
      val (_, sum) = List.foldl (fn (pair, (idx, sum)) =>
            case Data.compare pair of
              LESS => (idx + 1, sum + idx)
            | _ => (idx + 1, sum)) (1, 0) packets
    in
      sum
    end

fun process2 filename = let
      val input = read_file filename
      val lines = String.tokens (fn c => c = #"\n") input
      val two = parse_line "[[2]]"
      val six = parse_line "[[6]]"
      val packets = List.map parse_line lines
      val packets' = two :: six :: packets
      val packets'' = ListMergeSort.sort (fn pair => Data.compare pair = GREATER) packets'
      val (_, key) = List.foldl (fn (packet, (idx, key')) =>
            if packet = two orelse packet = six then (idx + 1, key' * idx) else (idx + 1, key')) (1, 1) packets''
    in
      key
    end