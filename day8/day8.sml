val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid

fun transpose [] = []
  | transpose ([] :: _) = []
  | transpose l = (List.map List.hd l) :: (transpose (List.map List.tl l))

datatype Tree = Unviewed of int | Viewed of int

fun process_row cmp = List.foldl
    (fn (t as (Viewed n), (visible_count, curr_max, row_acc)) => (visible_count, Int.max (curr_max, n), t :: row_acc)
      | (t as (Unviewed n), (visible_count, curr_max, row_acc)) =>
        if cmp (n ,curr_max) then (visible_count + 1, n, (Viewed n) :: row_acc) else (visible_count, curr_max, t :: row_acc))
    (0, Option.valOf Int.minInt, [])

fun process filename = let
      val text = read_file filename
      val rows = String.tokens (fn c => c = #"\n") text
      val rows' = List.map String.explode rows
      val numeric_rows = List.map (List.map (fn c => Unviewed (Option.valOf (Int.fromString (str c))))) rows'
      val f = fn (row, (visible_count, rows)) => let
            val (vc, _, row') = process_row op> row
          in (visible_count + vc, row' :: rows) end
      val (visible1, reversed_reversed_rows) = List.foldl f (0, []) numeric_rows
      val (visible2, unreversed_unreversed_rows) = List.foldl f (visible1, []) reversed_reversed_rows
      val col_unreversed_unreversed_rows = transpose unreversed_unreversed_rows
      val (visible3, col_reversed_reversed_rows) = List.foldl f (visible2, []) col_unreversed_unreversed_rows
      val (visible4, _) = List.foldl f (visible3, []) col_reversed_reversed_rows
    in visible4 end

(* bruh i hate this part 2 *)
(* how the fuck are you supposed to build off part 1 lol *)
(* or maybe i did part 1 badly. who knows. i'm bad. *)

exception Taller of int

fun view_to_right_of t ts = List.foldl
    (fn (t', acc) => if t' >= t then raise Taller (acc + 1) else acc + 1) 0 ts
    handle Taller n => n

fun process_row2 [] = []
  | process_row2 (t :: ts) = (view_to_right_of t ts) :: process_row2 ts

(* who cares anymore *)
fun zip4 [] [] [] [] = []
  | zip4 (x :: xs) (y :: ys) (z :: zs) (b :: bs) = [x, y, z, b] :: zip4 xs ys zs bs
  | zip4 _ _ _ _ = raise Invalid

fun zip4' [l1, l2, l3, l4] = zip4 l1 l2 l3 l4

val one = [[1,2],[3,4]]
val two = [[5,6],[7,8]]
val three = [[9,10],[11,12]]
val four = [[13,14],[15,16]]


fun process2 filename = let
      val text = read_file filename
      val rows = String.tokens (fn c => c = #"\n") text
      val rows' = List.map String.explode rows
      val numeric_rows = List.map (List.map (fn c => Option.valOf (Int.fromString (str c)))) rows'
      (* holy shit lmao you are so bad *)
      val scores1 = List.map process_row2 numeric_rows
      val numeric_rows' = List.map List.rev numeric_rows
      val scores2' = List.map process_row2 numeric_rows'
      val scores2 = List.map List.rev scores2'
      val numeric_cols = transpose numeric_rows
      val scores3' = List.map process_row2 numeric_cols
      val scores3 = transpose scores3'
      val numeric_cols' = List.map List.rev numeric_cols
      val scores4'' = List.map process_row2 numeric_cols'
      val scores4' = List.map List.rev scores4''
      val scores4 = transpose scores4'
      val scores = zip4 scores1 scores2 scores3 scores4
      val scores' = List.map zip4' scores
      (* it keeps going *)
      val scores'' = List.map (List.map (List.foldl op* 1)) scores'
      val scores''' = List.concat scores''
    in
      List.foldl Int.max 0 scores'''
    end
  
