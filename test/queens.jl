# Solve the n-queens problem to test optional constraints.

"Creates the constraints matrix for the n-queens problem."
function make_queens_matrix(n)
  slash_diagonal_for(row, column) = row + column - 1
  backslash_diagonal_for(row, column) = n + row - column

  constraints_for(row, column) = Set(Any[
    (:row, row),
    (:column, column),
    (:slash, slash_diagonal_for(row, column)),
    (:backslash, backslash_diagonal_for(row, column))
  ])

  CoverMatrix(
    Dict([(row, column) => constraints_for(row, column) for row in 1:n, column in 1:n]),
    optionals = union(
      Set(Any[(:slash, x) for x in 1:(2n-1)]),
      Set(Any[(:backslash, x) for x in 1:(2n-1)])
    )
  )
end

"See [OEIS sequence A000170](http://oeis.org/A000170)."
const oeis_A000170 = [1, 0, 0, 2, 10, 4, 40, 92, 352, 724]

let
  for (queen_count, position_count) in enumerate(oeis_A000170)
    @test exact_cover_count(make_queens_matrix(queen_count)) == position_count
  end
end
