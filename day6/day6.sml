val read_file = TextIO.inputAll o TextIO.openIn

signature N_ITEM_BUFFER_CONTENTS = sig
  val n : int
  type t
  val eq : t * t -> bool
end
  
signature N_ITEM_BUFFER = sig
  structure N : N_ITEM_BUFFER_CONTENTS

  type t
  
  val empty: t
  val addAndCheck : t -> N.t -> t * bool
end

functor NItemBuffer(I: N_ITEM_BUFFER_CONTENTS): N_ITEM_BUFFER = struct
  structure N = I
  type t = N.t list

  val empty = []

  (* extremely efficient *)
  fun is_unique l item =
      (List.length (List.filter (fn x => N.eq (x, item)) l)) = 1

  fun addAndCheck buf item =
      if List.length buf < N.n then (item :: buf, false)
      else let
          val buf' = List.take (buf, List.length buf - 1)
          val buf'' = item :: buf'
          (* extremely extremely efficient *)
          val unique = List.all (is_unique buf'') buf''
        in
          (buf'', unique)
        end
end

structure FourItemBufferContents: N_ITEM_BUFFER_CONTENTS = struct
  val n = 4
  type t = char
  val eq = op=
end

structure FourteenItemBufferContents: N_ITEM_BUFFER_CONTENTS = struct
  val n = 14
  type t = char
  val eq = op=
end

functor Solution(I: N_ITEM_BUFFER_CONTENTS where type t = char) = struct
  structure Buffer: N_ITEM_BUFFER = NItemBuffer(I)
  exception Marker of int

  fun addAndRaise buf (c, i) = case Buffer.addAndCheck buf c of
        (_, true) => raise Marker i
      | (buf', false) => buf'

  fun process filename = let
        val text = read_file filename
        val chars = String.explode text
        val indices = List.tabulate (List.length chars, fn i => i+1)
        val items = ListPair.zip (chars, indices)
        val _ = List.foldl (fn (item, buf) => addAndRaise buf item) Buffer.empty items
      in
        NONE
      end
      handle Marker n => SOME n

  fun process_string s = let
        val chars = String.explode s
        val indices = List.tabulate (List.length chars, fn i => i+1)
        val items = ListPair.zip (chars, indices)
        val _ = List.foldl (fn (item, buf) => addAndRaise buf item) Buffer.empty items
      in
        NONE
      end
      handle Marker n => SOME n
end
