let # empty
  matrix = CoverMatrix(Dict{Any, Set{Any}}())

  @test size(matrix) == (0, 0)
  @test exact_cover_exists(matrix)
  @test exact_cover_count(matrix) == 1
  @test isempty(find_exact_cover(matrix))
end

let # 1-element
  set = CoverMatrix(Dict(1 => Set(23)))

  @test size(set) == (1, 1)
  @test exact_cover_exists(set)
  @test exact_cover_count(set) == 1
  @test find_exact_cover(set) == Set([1])
end

let # twice the same 1-element subset
  set = CoverMatrix(Dict(
    1 => Set(23),
    2 => Set(23)
  ))

  @test size(set) == (2, 1)
  @test exact_cover_exists(set)
  @test exact_cover_count(set) == 2

  Set(exact_cover_producer(set)) == Set([Set(1), Set(2)])
end

let # single subset, 2 elements
  set = CoverMatrix(Dict(1 => Set([23, 42])))

  @test size(set) == (1, 2)
  @test exact_cover_exists(set)
  @test exact_cover_count(set) == 1
  @test find_exact_cover(set) == Set(1)
end

let # 2 subsets of 1 element each
  set = CoverMatrix(Dict(
    1 => Set(23),
    2 => Set(42)
  ))

  @test size(set) == (2, 2)
  @test exact_cover_exists(set)
  @test exact_cover_count(set) == 1
  @test find_exact_cover(set) == Set([1, 2])
end

let # trivial decision
  set = CoverMatrix(Dict(
    1 => Set(23),
    2 => Set([23, 42])
  ))

  @test size(set) == (2, 2)
  @test exact_cover_exists(set)
  @test exact_cover_count(set) == 1
  @test find_exact_cover(set) == Set(2)
end

let # impossible decision
  set = CoverMatrix(Dict(
    1 => Set([12, 23]),
    2 => Set([23, 42])
  ))

  @test size(set) == (2, 3)
  @test !exact_cover_exists(set)
  @test exact_cover_count(set) == 0
  @test find_exact_cover(set) == nothing
  @test set |> exact_cover_producer |> collect |> length == 0
end

let # more complex key types
  set = CoverMatrix(Dict(
    1 => Set(23),
    :symbol => Set(42),
    (:tagged, 1, 2) => Set(123),
    [1, 2, 3] => Set(321)
  ))

  @test size(set) == (4, 4)
  @test exact_cover_exists(set)
  @test exact_cover_count(set) == 1
  @test Set(find_exact_cover(set)) == Set(Any[1, :symbol, (:tagged, 1, 2), [1, 2, 3]])
end

let # more complex value types
  set = CoverMatrix(Dict(
    1 => Set(Any[(:tagged, 1)]),
    2 => Set(Any[(:tagged, 2), (:tagged, 2)]),
    3 => Set(Any[(:tagged, 3), (:tagged, 4)]),
    4 => Set(Any[5, :symbol, (:tagged, 6)]),
    5 => Set(Any[:symbol, (:tagged, 6)])
  ))

  @test size(set) == (5, 7)
  @test exact_cover_count(set) == 1
  @test Set(find_exact_cover(set)) == Set([1, 2, 3, 4])
end

let # user-defined value types
  immutable Point
    x::Integer
    y::Integer
  end

  set = CoverMatrix(Dict(
    1 => Set(Any[Point(1, 2), Point(2, 3)]),
    2 => Set(Any[Point(2, 3)]),
    3 => Set(Any[(1, 2)])
  ))

  @test size(set) == (3, 3)
  @test exact_cover_count(set) == 1
  @test Set(find_exact_cover(set)) == Set([1, 3])
end
