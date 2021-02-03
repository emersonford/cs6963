# Assignment 1 - Sudoku Solver & Generator
Must have `random` and `random-shuffle` Haskell packages installed.

## Solver
### Easy Test
`make test_solver`

### Manual Use
Run `make solver`. Pass in a board to `./solver` via stdin.

Boards should be of the form:
```
m n
X X X  X X X  X X X ...
X X X  X X X  X X X ...
X X X  X X X  X X X ...

X X X  X X X  X X X ...
X X X  X X X  X X X ...
X X X  X X X  X X X ...

X X X  X X X  X X X ...
X X X  X X X  X X X ...
X X X  X X X  X X X ...
  .      .      .   .
  .      .      .     .
```

Where `X` is either a number (1 to m\*n inclusive) or `_`/`.` to indicate an empty slot.
The lines can be separated by any number of new lines.
Numbers must be separated by at least 1 space.
`m` represents the width of squares.
`n` represents the height of squares.
The grid must have m\*n numbers per line and m\*n lines with numbers.

See `boards/` for examples.

## Generator
### Easy Test
`make test_generator`

### Manual Use
Run `make generator`. Pass in `m n numRemove seed` to `./generator` via stdin.
`m` represents the width of the squares.
`n` represents the height of squares.
`numRemove` is the number of slots to make empty.
`seed` is a random value.
