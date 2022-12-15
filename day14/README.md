## `GRID_2D`
`GRID_2D` is an extremely stupid signature containing the 5 functions I originally used from `ORD_MAP` (`new` corresponds to `empty`). I wanted to write a 2D grid structure that was backed by mutable arrays instead of a red-black tree to see if that helped performance on part 2 (you'll see why when I post timing data later lmao).

The code now contains a `Solution` functor parameterised over 2D grid implementation. Use `MapSolution.process2` for the red-black-tree-based implementation, and `ArraySolution.process2` for the mutable-array-based implementation.

If you're in SML/NJ, `CM.make "sources.cm"` and then use the appropriate structure as mentioned above. For MLton, unfortunately I have not figured out a nicer way to do this so you have to edit `main.sml` and comment out the structure you don't want, then run `./day14 input.txt part2`.

## Timing data
This data is obtained from MLton because
1. I don't know how to time and profile programs in SML/NJ
1. The array-based implementation is unusably slow in SML/NJ????

### `MapSolution`
```
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> mlton day14.mlb
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> time ./day14 input.txt part2
24958
________________________________________________________
Executed in  886.05 millis    fish           external
   usr time  722.28 millis    0.17 millis  722.11 millis
   sys time   20.38 millis    1.00 millis   19.38 millis
```

```
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> mlton -profile time -profile-branch true day14.mlb
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> ./day14 input.txt part2
24958⏎
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> mlprof ./day14 mlmon.out
0.46 seconds of CPU time (0.21 seconds GC)
                            function                               cur
----------------------------------------------------------------- -----
<gc>                                                              31.3%
_res_RedBlackMapFn.insert.ins                                     23.9%
IntPairRedBlackTreeMapGrid.IntPair.compare                        11.9%
_res_RedBlackMapFn.insert.ins.<case _>                             9.0%
_res_RedBlackMapFn.insert.ins.<case c>                             6.0%
IntPairRedBlackTreeMapGrid.IntPair.compare.<case compare ((  ...>  6.0%
_res_RedBlackMapFn.insert.ins.<case d>                             3.0%
IntPairRedBlackTreeMapGrid.insert.<case insert gri  ...>           3.0%
_res_RedBlackMapFn.insert.ins.<case d>                             1.5%
_res_RedBlackMapFn.insert.ins.<case _>                             1.5%
PosixError.SysCall.simpleResultAux                                 1.5%
_res_RedBlackMapFn.insert.ins.<case c>                             1.5%
```

You can see it spends all its time in either garbage collection or red-black tree operations :/

### `ArraySolution`
```
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> mlton day14.mlb
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> time ./day14 input.txt part2
24958
________________________________________________________
Executed in  220.70 millis    fish           external
   usr time   56.90 millis    0.14 millis   56.76 millis
   sys time    6.70 millis    1.12 millis    5.58 millis
```

```
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> mlton -profile time -profile-branch true day14.mlb
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> ./day14 input.txt part2
24958⏎
vincom2@Vincents-MacBook-Air ~/advent/day14 (main)> mlprof ./day14 mlmon.out
0.06 seconds of CPU time (0.01 seconds GC)
                  function                     cur
--------------------------------------------- -----
_res_Solution.process2.<case process2 f  ...> 57.1%
ArrayGrid.insert                              28.6%
<gc>                                          14.3%
```

yeah idk seems better LMAO

## Miscellaneous
Yeah, I know 56ms is still really slow. whatever lol, at least it's no longer 700ms...

Also I sort of wonder why running the array-based solution in SML/NJ is so slow. I'm too lazy to do this but I wonder if using the array grid "properly" (storing it in a ref somewhere) would help? Maybe SML/NJ is cloning the array when I pass it around? Or maybe the underlying mutable array implementation is slow? Or both? Or something else?