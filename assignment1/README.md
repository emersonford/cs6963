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

## Examples
```
❯ echo "boards/3x3-1.txt"; cat boards/3x3-1.txt; echo "\nSolution:"; ./solver < boards/3x3-1.txt
boards/3x3-1.txt
3 3
8 2 7  _ _ 3  6 1 _
6 _ _  _ 5 _  _ _ 7
_ _ 5  7 6 _  2 4 _

4 _ _  _ _ _  _ 2 _
_ _ 2  _ 3 _  4 _ _
_ 1 _  _ _ _  _ _ 6

_ 5 6  _ 7 1  9 _ _
9 _ _  _ 2 _  _ _ 3
_ 7 8  9 _ _  1 5 2

Solution:
8 2 7  4 9 3  6 1 5
6 9 4  1 5 2  8 3 7
1 3 5  7 6 8  2 4 9

4 6 9  5 1 7  3 2 8
5 8 2  6 3 9  4 7 1
7 1 3  2 8 4  5 9 6

2 5 6  3 7 1  9 8 4
9 4 1  8 2 5  7 6 3
3 7 8  9 4 6  1 5 2
```

```
❯ echo "3 3 7 1239102" | ./generator
1 2 3  9 4 5  7 6 8
6 9 8  7 1 2  4 3 5
4 5 7  6 8 3  1 2 9

_ 4 1  2 6 9  _ 8 7
8 6 2  3 7 _  9 5 1
7 3 9  _ 5 8  6 4 2

9 8 6  5 3 7  2 1 4
2 1 4  8 9 6  5 _ 3
3 _ 5  4 2 _  8 9 6

Solution:
1 2 3  9 4 5  7 6 8
6 9 8  7 1 2  4 3 5
4 5 7  6 8 3  1 2 9

5 4 1  2 6 9  3 8 7
8 6 2  3 7 4  9 5 1
7 3 9  1 5 8  6 4 2

9 8 6  5 3 7  2 1 4
2 1 4  8 9 6  5 7 3
3 7 5  4 2 1  8 9 6
```
