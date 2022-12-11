val read_file = TextIO.inputAll o TextIO.openIn

exception Invalid

(* directory name, size, subdirectory list (very efficient) *)
type directory = string * int * string list
(* directory stack, full directory map *)
type environment = string list * directory AtomMap.map

datatype CD = Root | Up | Directory of string
datatype Command = Ls | Cd of CD
datatype Line = Cmd of Command | Dir of string | File of int

(* this interface sucks; why am i peeling off the top dir manually all the time? *)
(* also maybe this should be wrapped in a datatype so we don't use the wrong type of key in the dirmap
 * although maybe the string <> Atom.atom conversion is enough to ensure that? *)
(* key_of : string -> string list -> Atom.atom *)
fun key_of' wd [] = wd
  | key_of' wd (d::ds) = wd ^ (key_of' d ds)
fun key_of wd stack = Atom.atom (key_of' wd stack)


(* this just modifies the environment and returns the directory you're interested in *)
(* cd : environment -> CD -> environment * directory *)
fun cd (_, dirmap) Root = (case AtomMap.find (dirmap, Atom.atom "/") of
        SOME rootdir => ((["/"], dirmap), rootdir)
      | NONE => let
          val rootdir = ("/", 0, [])
          val dirmap' = AtomMap.insert (dirmap, Atom.atom "/", rootdir)
        in
          ((["/"], dirmap'), rootdir)
        end)
  | cd ([], _) Up = raise Invalid
  | cd ([_], _) Up = raise Invalid
  | cd (_ :: d :: ds, dirmap) Up = let
      val env' = (d :: ds, dirmap)
      val dir = AtomMap.lookup (dirmap, key_of d ds)
    in (env', dir) end
  | cd (stack, dirmap) (Directory name) = case AtomMap.find (dirmap, key_of name stack) of
      SOME dir => let
          val env' = (name :: stack, dirmap)
        in (env', dir) end
    | NONE => let
        val dir = (name, 0, [])
        val dirmap' = AtomMap.insert (dirmap, key_of name stack, dir)
        val env' = (name :: stack, dirmap')
      in (env', dir) end

(* this takes in a list containing the remaining lines and processes them until the next $, updating the environment where necessary *)
(* ls : environment -> Line list -> environment * Line list *)
fun ls ([], _) _ = raise Invalid
  | ls env [] = (env, [])
  | ls env (lines as ((Cmd _) :: _)) = (env, lines)
  | ls (stack as (d :: ds), dirmap) ((File size) :: lines) = let
      val dirkey = key_of d ds
      val (dirname, dirsize, dirsubdirs) = AtomMap.lookup (dirmap, dirkey)
      val dir' = (dirname, dirsize + size, dirsubdirs)
      val dirmap' = AtomMap.insert (dirmap, dirkey, dir')
    in
      ls (stack, dirmap') lines
    end
  | ls (stack as (d :: ds), dirmap) ((Dir subdirname) :: lines) = let
      val dirkey = key_of d ds
      val (dirname, dirsize, dirsubdirs) = AtomMap.lookup (dirmap, dirkey)
      val subdirkey = key_of subdirname stack
      val dir' = (dirname, dirsize, subdirkey :: dirsubdirs)
      val dirmap' = AtomMap.insert (dirmap, dirkey, dir')
    in
      ls (stack, dirmap') lines
    end

(* this updates the map of "total sizes" (dir size + subdirectories' sizes) for
 * the directory tree rooted at the directory referenced by dirkey *)
(* traverse : directory AtomMap.map -> int AtomMap.map -> directory -> int AtomMap.map *)
fun traverse dirmap sizemap dirkey = let
      val (dirname, dirsize, subdirs) = AtomMap.lookup (dirmap, dirkey)
      val (sizemap', size) = List.foldl
        (fn (subdirkey, (sizemap', size_acc)) => let
              val (sizemap'', size) = size_of dirmap sizemap' subdirkey
            in
              (sizemap'', size_acc + size)
            end) (sizemap, dirsize) subdirs
      val sizemap'' = AtomMap.insert (sizemap', dirkey, size)
    in sizemap'' end

  (* this updates the size map and also returns the total size of the directory referenced by dirkey *)
  (* size_of : directory AtomMap.map -> int AtomMap.map -> Atom.atom -> int AtomMap.map * int *)
and size_of dirmap sizemap dirkey = case AtomMap.find (sizemap, dirkey) of
      SOME size => (sizemap, size)
    | NONE => let
        val sizemap' = traverse dirmap sizemap dirkey
      in
        (sizemap', AtomMap.lookup (sizemap', dirkey))
      end

fun line_to_Line line = let
      val x1 :: x2 :: xs = String.tokens Char.isSpace line
    in
      case x1 of
        "$" => (case x2 of
              "cd" => let
                  val dirname = List.hd xs
                in
                  case dirname of
                    "/" => Cmd (Cd Root)
                  | ".." => Cmd (Cd Up)
                  | _ => Cmd (Cd (Directory dirname))
                end
            | "ls" => Cmd Ls
            | _ => raise Invalid)
      | "dir" => Dir x2
      | _ => File (Option.valOf (Int.fromString x1))
    end

fun process_Lines env [] = env
  (* the next 2 cases should always be consumed by `ls` *)
  | process_Lines _ ((Dir _) :: _) = raise Invalid
  | process_Lines _ ((File _) :: _) = raise Invalid
  | process_Lines env ((Cmd Ls) :: lines) = let
      val (env', lines') = ls env lines
    in process_Lines env' lines' end
  | process_Lines env ((Cmd (Cd somewhere)) :: lines) = let
      val (env', _) = cd env somewhere
    in process_Lines env' lines end


fun process filename = let
      val text = read_file filename
      val lines = String.tokens (fn c => c = #"\n") text
      val input = List.map line_to_Line lines
      val (_, dirmap) = process_Lines ([], AtomMap.empty) input
      val (sizemap, _) = size_of dirmap AtomMap.empty (Atom.atom "/")
      val smaller_sizemap = AtomMap.filter (fn sz => sz <= 100000) sizemap
    in
      AtomMap.foldl op+ 0 smaller_sizemap
    end

val total_available = 70000000
fun process2 filename = let
      val text = read_file filename
      val lines = String.tokens (fn c => c = #"\n") text
      val input = List.map line_to_Line lines
      val (_, dirmap) = process_Lines ([], AtomMap.empty) input
      val (sizemap, total_used) = size_of dirmap AtomMap.empty (Atom.atom "/")
      val unused = total_available - total_used
      val needed = 30000000 - unused
      val sizemap' = AtomMap.filter (fn sz => sz >= needed) sizemap
      val larger_sizes = AtomMap.listItems sizemap'
      val sorted_larger_sizes = ListMergeSort.sort op> larger_sizes
    in
      List.hd sorted_larger_sizes
    end