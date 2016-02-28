# Solve Project Euler problem 96 (sudoku) to test ExactCovers.
#
# The idea here is to reduce sudoku to the exact cover problem and then use
# ExactCovers to solve that. Because we need to solve multiple sudokus, we
# costruct a general sudoku matrix and then, for each sudoku,
# 1. cover the parts of the matrix that correspond to givens,
# 2. find an exact cover for the partially covered matrix, and finally
# 3. uncover in reverse order.
# This is more efficient (and also easier to write down) than creating a
# specialized matrix for each sudoku.
#
# Each row (subset) in the sudoku matrix corresponds to placing a number into a
# square. The columns (elements) correspond to the individual constraints that
# each placement fulfills. These are:
# * The corresponding square is filled.
# * The corresponding row has the placed number.
# * The corresponding column has the placed number.
# * The corresponding box (3x3-subgrid) has the placed number.
#
# Because the sudoku rules dictate that each of these constraints must be
# satisfied exactly once, an exact cover for that matrix is a solution to
# sudoku. Preselecting (covering) rows (subsets) now makes sure that we are
# actually solving the given sudoku instance, instead of an empty grid.

"Creates the `CoverSet` that represents an empty sudoku grid."
function make_sudoku_matrix()
  box_for(row, column) = (div(row - 1, 3) + 1, div(column - 1, 3) + 1)

  constraints_for(row, column, number) = Set(Any[
    (:square_is_filled, row, column),
    (:row_has, row, number),
    (:column_has, column, number),
    (:box_has, box_for(row, column), number)
  ])

  CoverMatrix(Dict([
    (row, column, number) => constraints_for(row, column, number) for
      row in 1:9,
      column in 1:9,
      number in 1:9
  ]))
end

"""
Preselects the given placements from `sudoku` in `matrix`.
"""
function cover_givens!(sudoku, matrix)
  for row in 1:9, column in 1:9
    if sudoku[row, column] > 0
      cover_subset!(matrix, (row, column, sudoku[row, column]))
    end
  end
end

"""
Reverses the preselection from `sudoku`, so that `matrix` is restored to a
pristine sudoku matrix.
"""
function uncover_givens!(sudoku, matrix)
  for row in 9:-1:1, column in 9:-1:1
    if sudoku[row, column] > 0
      uncover_subset!(matrix, (row, column, sudoku[row, column]))
    end
  end
end

"""
Solves `sudoku`, assuming `matrix` is a sudoku matrix representing an empty
grid. `matrix` will be restored to that state when the call returns.
"""
function solve(sudoku, matrix)
  result = copy(sudoku)
  cover_givens!(sudoku, matrix)
  for entry in find_exact_cover(matrix)
    row, column, number = entry
    result[row, column] = number
  end
  uncover_givens!(sudoku, matrix)
  
  result
end

"""
Reads the source file containing the sudoku instances to solve.
"""
sudoku_producer() = @task let
  file = open(joinpath(dirname(@__FILE__), "data", "sudokus.txt"))
  while !eof(file)
    readline(file)
    sudoku = Array(Int8, 9, 9)
    for row in 1:9
        sudoku[row, :] = [c - Int8('0') for c in read(file, Int8, 9)]
        readline(file)
    end

    produce(sudoku)
  end
end

# Actually solve Project Euler problem 96. This means solving the sudokus,
# extracting the first three boxes of each, interpreting them as 3-digit
# numbers and adding them up.
let
  result = 0

  matrix = make_sudoku_matrix()
  for sudoku in sudoku_producer()
    sudoku = solve(sudoku, matrix)
    result += 100sudoku[1, 1] + 10sudoku[1, 2] + sudoku[1, 3]
  end

  @test result == 24702
end
