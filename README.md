# Python code generator

An F# combinator to generate randomized Python code. [View example output.](output/example.py)

The parameters can be adjusted in `settings.json`. 

The `"depth"` parameter is the amount of nested statements the code is allowed to have.
The rest are how common each statement should be, in relation to the others. 
If all are set to the same value they have equal chance to occur in the code.

The file `words.txt` is the source for all variable names and strings.

Code is by default outputted to `output/output.py`.

# To-do list
- [x] `if` and `else` statements
- [x] Assignment expressions
- [ ] More syntax (`for`, `while`, etc.)
- [ ] Functions
- [ ] More settings (occurrance of types, length of blocks, expression depth)
- [ ] More reasonable integers
- [ ] Floats
- [ ] Remove char type and integrate it into strings
- [ ] Clean up Variables.fs
- [ ] Comparative expressions (`<`, `>`, `==`, `<=`, `>=`) and overall better boolean expressions
- [ ] Overall more human-like code
- [ ] Monad of Code type
