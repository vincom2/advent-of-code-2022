type items = int list
type operation = int -> int
type test = int -> int (* monkey to throw to *)

type monkey = operation * test

structure M = IntListMap

(* hardcoded lmao *)
structure RealMonkeys = struct
  val monkey0_items = [52, 78, 79, 63, 51, 94]
  val monkey_items = M.singleton (0, monkey0_items)
  val monkey0 = (fn n => n * 13, fn n => if n mod 5 = 0 then 1 else 6)
  val monkey_map = M.singleton (0, monkey0)

  val monkey1_items = [77, 94, 70, 83, 53]
  val monkey_items = M.insert (monkey_items, 1, monkey1_items)
  val monkey1 = (fn n => n + 3, fn n => if n mod 7 = 0 then 5 else 3)
  val monkey_map = M.insert (monkey_map, 1, monkey1)

  val monkey2_items = [98, 50, 76]
  val monkey_items = M.insert (monkey_items, 2, monkey2_items)
  val monkey2 = (fn n => n * n, fn n => if n mod 13 = 0 then 0 else 6)
  val monkey_map = M.insert (monkey_map, 2, monkey2)

  val monkey3_items = [92, 91, 61, 75, 99, 63, 84, 69]
  val monkey_items = M.insert (monkey_items, 3, monkey3_items)
  val monkey3 = (fn n => n + 5, fn n => if n mod 11 = 0 then 5 else 7)
  val monkey_map = M.insert (monkey_map, 3, monkey3)

  val monkey4_items = [51, 53, 83, 52]
  val monkey_items = M.insert (monkey_items, 4, monkey4_items)
  val monkey4 = (fn n => n + 7, fn n => if n mod 3 = 0 then 2 else 0)
  val monkey_map = M.insert (monkey_map, 4, monkey4)

  val monkey5_items = [76, 76]
  val monkey_items = M.insert (monkey_items, 5, monkey5_items)
  val monkey5 = (fn n => n + 4, fn n => if n mod 2 = 0 then 4 else 7)
  val monkey_map = M.insert (monkey_map, 5, monkey5)

  val monkey6_items = [75, 59, 93, 69, 76, 96, 65]
  val monkey_items = M.insert (monkey_items, 6, monkey6_items)
  val monkey6 = (fn n => n * 19, fn n => if n mod 17 = 0 then 1 else 3)
  val monkey_map = M.insert (monkey_map, 6, monkey6)

  val monkey7_items = [89]
  val monkey_items = M.insert (monkey_items, 7, monkey7_items)
  val monkey7 = (fn n => n + 2, fn n => if n mod 19 = 0 then 2 else 4)
  val monkey_map = M.insert (monkey_map, 7, monkey7)

  (* i will never trust chatGPT again *)
  val lcm = 9699690
end

structure TestMonkeys = struct
  val monkey0_items = [79, 98]
  val monkey_items = M.singleton (0, monkey0_items)
  val monkey0 = (fn n => n * 19, fn n => if n mod 23 = 0 then 2 else 3)
  val monkey_map = M.singleton (0, monkey0)

  val monkey1_items = [54, 65, 75, 74]
  val monkey_items = M.insert (monkey_items, 1, monkey1_items)
  val monkey1 = (fn n => n + 6, fn n => if n mod 19 = 0 then 2 else 0)
  val monkey_map = M.insert (monkey_map, 1, monkey1)

  val monkey2_items = [79, 60, 97]
  val monkey_items = M.insert (monkey_items, 2, monkey2_items)
  val monkey2 = (fn n => n * n, fn n => if n mod 13 = 0 then 1 else 3)
  val monkey_map = M.insert (monkey_map, 2, monkey2)

  val monkey3_items = [74]
  val monkey_items = M.insert (monkey_items, 3, monkey3_items)
  val monkey3 = (fn n => n + 3, fn n => if n mod 17 = 0 then 0 else 1)
  val monkey_map = M.insert (monkey_map, 3, monkey3)

  val lcm = 96577
end

structure Monkeys = RealMonkeys

(* throw_item_to : items M.map -> int -> int -> items M.map *)
fun throw_item_to items_map monkey item = let
      val items = M.lookup (items_map, monkey)
      val items' = items @ [item]
    in
      M.insert (items_map, monkey, items')
    end

(* this doesn't actually need to be explicitly passed items? ah well who cares lol *)
(* process_monkey : items M.map -> int M.map -> int -> operation -> test -> items -> items M.map * int M.map *)
fun process_monkey items_map ic_map monkey _ _ [] = (items_map, ic_map)
  | process_monkey items_map ic_map monkey operation test (item :: items) = let
      val ic = M.lookup (ic_map, monkey)
      val ic_map' = M.insert (ic_map, monkey, ic + 1)
      val item' = operation item
      (* val item'' = item' div 3 *)
      (* if you don't type annotate this it causes value restriction problems for process_monkeys lmao what the fuck *)
      (* val item'' : int = item' *)
      val item'' = item' mod Monkeys.lcm
      val dst_monkey = test item''
      val items_map' = throw_item_to items_map dst_monkey item''
      val items_map'' = M.insert (items_map', monkey, items)
    in
      process_monkey items_map'' ic_map' monkey operation test items
    end

val process_monkeys = M.foldli
  (fn (monkey_no, (operation, test), (items_map, ic_map)) => process_monkey items_map ic_map monkey_no operation test (M.lookup (items_map, monkey_no)))

fun simulate_rounds monkey_map items_map ic_map up_to curr =
    if curr < up_to then let
        val (items_map', ic_map') = process_monkeys (items_map, ic_map) monkey_map
      in
        simulate_rounds monkey_map items_map' ic_map' up_to (curr + 1)
      end
    else ic_map

fun simulate up_to = let
      val monkey_map = Monkeys.monkey_map
      val items_map: int list M.map = Monkeys.monkey_items
      val monkey_nos = List.tabulate (M.numItems monkey_map, fn n => n)
      val ic_map = List.foldl (fn (monkey_no, ic_map') => M.insert (ic_map', monkey_no, 0)) M.empty monkey_nos
      val ic_map' = simulate_rounds monkey_map items_map ic_map up_to 0
      val counts = M.listItems ic_map'
      val counts' = ListMergeSort.sort op< counts
      val [b1, b2] = List.take (counts', 2)
    in
      b1 * b2
    end