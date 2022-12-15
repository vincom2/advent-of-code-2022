The code here generally uses the SML/NJ library, so you might have to use that compiler (or MLton).

generally,
- `cd` into a directory
- load up an SML/NJ REPL
- `use "dayN.sml"`
- `process "input.txt"` (part 1)
- `process2 "input.txt"` (part 2)

for directories containing a `.cm` file, replace `use "dayN.sml"` above with `CM.make "sources.cm"`.

for directories containing a `.mlb` file, you can also
- run `mlton foo.mlb`
- run `./foo input.txt partX`
