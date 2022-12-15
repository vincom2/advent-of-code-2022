structure Util = struct
  infixr 0 $
  fun f $ x = f x

  val read_file = TextIO.inputAll o TextIO.openIn

  exception Invalid
end