val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid
exception Overlap

fun fully_contained (o1, e1) (o2, e2) =
    (o1 >= o2 andalso e1 <= e2) orelse (o2 >= o1 andalso e2 <= e1)

fun check_assignment_pair f ap = let
      val [a1, a2] = String.tokens (fn c => c = #",") ap
      val [o1, e1] = String.tokens(fn c => c = #"-") a1
      val [o2, e2] = String.tokens(fn c => c = #"-") a2
      val as1 as (o1', e1') = (Option.valOf (Int.fromString o1), Option.valOf (Int.fromString e1))
      val as2 as (o2', e2') = (Option.valOf (Int.fromString o2), Option.valOf (Int.fromString e2))
    in
      f as1 as2
    end

fun process f filename = let
      val text = read_file filename
      val assignment_pairs = String.tokens (fn c => c = #"\n") text
    in
      List.foldl (fn (ap, acc) => if check_assignment_pair f ap then acc + 1 else acc) 0 assignment_pairs
    end

fun compare_oc ((v1, oc1), (v2, oc2)) =
    if v1 = v2 then
      case (oc1, oc2) of
        (true, false) => false
      | _ => true
    else v1 > v2

fun overlap_exists (o1, e1) (o2, e2) = let
      val l = [o1, e1, o2, e2]
      val oc = [true, false, true, false]
      val l' = ListPair.zip (l, oc)
      val l'' = ListMergeSort.sort compare_oc l'
      val _ = List.foldl
        (fn ((_, true), 0) => 1
          | ((_, true), 1) => raise Overlap
          | ((_, false), 0) => raise Invalid
          | ((_, false), 1) => 0
          | _ => raise Invalid) 0 l''
    in
      false
    end
    handle Overlap => true