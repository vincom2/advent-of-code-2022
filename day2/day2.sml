val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid

fun score #"A" #"X" = 3
  | score #"A" #"Y" = 6
  | score #"A" #"Z" = 0
  | score #"B" #"X" = 0
  | score #"B" #"Y" = 3
  | score #"B" #"Z" = 6
  | score #"C" #"X" = 6
  | score #"C" #"Y" = 0
  | score #"C" #"Z" = 3
  | score _ _ = raise Invalid

fun score_shape #"X" = 1
  | score_shape #"Y" = 2
  | score_shape #"Z" = 3
  | score_shape _ = raise Invalid

fun score1 opp me = score opp me + score_shape me

fun score_round scoring_function round = let
      val [t, m] = String.tokens Char.isSpace round
      val opp = Option.valOf (Char.fromString t)
      val me = Option.valOf (Char.fromString m)
    in
      scoring_function opp me
    end

fun process scoring_function filename = let
      val text = read_file filename
      val rounds = String.tokens (fn c => c = #"\n") text
    in
      List.foldl (fn (round, acc) => acc + scoring_function round) 0 rounds
    end

(* X is lose 0, Y is draw 3, Z is win 6 *)
(* A is rock 1, B is paper 2, C is scissors 3 *)
fun score2 #"A" #"X" = 3
  | score2 #"A" #"Y" = 4
  | score2 #"A" #"Z" = 8
  | score2 #"B" #"X" = 1
  | score2 #"B" #"Y" = 5
  | score2 #"B" #"Z" = 9
  | score2 #"C" #"X" = 2
  | score2 #"C" #"Y" = 6
  | score2 #"C" #"Z" = 7
  | score2 _ _ = raise Invalid
