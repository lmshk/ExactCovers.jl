"""
Allows finding and enumerating all exact covers for a given collection of sets.

Given some set X and a collection S of subsets of X, an exact cover of X is
a selection S* of subsets from S such that each element of X is contained
in S* exactly once. [See this.](https://en.wikipedia.org/wiki/Exact_cover)

This module defines a type `CoverMatrix` that represents such a collection of
subsets along with methods to query it for exact covers. Instances can be
created from a `Dict{K, Set{V}}`, where the values `Set{V}` represent the
subsets and the universe X is implicitly defined by their union. The keys `K`
are used to identify the subsets in the exact covers.

For example, to find a simple exact cover for the sets {{1, 2}, {1, 2, 3}, {3}}:

    subsets = CoverMatrix(Dict(
      :a => Set([1, 2]),
      :b => Set([1, 2, 3]),
      :c => Set([3])
    ))

    find_exact_cover(subsets) # gives either `Set([:b])` or `Set([:a, :c])`

Of course there might be no exact cover for the given subsets, in which case
`find_exact_cover` will return `nothing`. If there are multiple exact covers,
it may return any of them. If you need all of them, you can call
`exact_cover_producer`, which returns a `Task` that produces each of them
individually. For instance:

    for cover in exact_cover_producer(subsets)
      println(cover)
    end

But note that the `CoverMatrix` is modified when enumerating covers; it cannot
be used otherwise while the enumeration is in progress and will only be
restored to its original state once the consumer finishes.

If you are just interested in the number of covers, you can use
`exact_cover_exists` or `exact_cover_count`.

The implemented algorithm for finding the exact covers is Knuth's
["Algorithm X" with Dancing Links (DLX)]
(http://www-cs-faculty.stanford.edu/~uno/papers/dancing-color.ps.gz).

"""
module ExactCovers

export
  CoverMatrix,
  exact_cover_exists,
  exact_cover_count,
  exact_cover_producer,
  find_exact_cover,
  cover_subset!,
  uncover_subset!


"""
The node type for the (sparse) subset matrix.

It has three specializations: the root (`CoverMatrix`), the column/element
headers (`ElementNode`) and the actual cells representing the entries in the
incidence matrix (`Cell`), where each has a different "payload".

All nodes are members of two cyclic lists, one horizontally and one vertically.
The `ElementNode`s and `CoverMatrix` act as list sentinels and are only linked
to themselves on creation.

There are some general functions on `Node`s to simplify code further down. These
are:
  * accessors for their neighbors,
  * functions to temporarily detach and reattach them from their containing
    lists, and
  * functions to initially insert them into the lists at specified positions.
"""
type Node{T}
  payload::T
  left::Node
  right::Node
  up::Node
  down::Node

  function Node(data::T)
    self = new(data)
    self.left = self.right = self.up = self.down = self
    self
  end
end

left(node::Node) = node.left
right(node::Node) = node.right
up(node::Node) = node.up
down(node::Node) = node.down

function detach_horizontally!(node::Node)
  node.left.right = node.right
  node.right.left = node.left
end

function reattach_horizontally!(node::Node)
  node.left.right = node
  node.right.left = node
end

function detach_vertically!(node::Node)
  node.up.down = node.down
  node.down.up = node.up
end

function reattach_vertically!(node::Node)
  node.up.down = node
  node.down.up = node
end

function append_down!(position::Node, node::Node)
  node.up = position
  node.down = position.down
  reattach_vertically!(node)

  node
end

function append_right!(position::Node, node::Node)
  node.left = position
  node.right = position.right
  reattach_horizontally!(node)

  node
end

type ElementHeader
  element
  count::Int
  isprimary::Bool
end

typealias ElementNode Node{ElementHeader}

type CellData
  element_node::ElementNode
  subset_key
end

typealias Cell Node{CellData}

type CoverMatrixHeader
  elements::Dict{Any, ElementNode}
  subsets::Dict{Any, Cell}
  visible_primary_element_count::Int

  CoverMatrixHeader() = new(Dict{Any, ElementNode}(), Dict{Any, Cell}(), 0)
end

typealias CoverMatrix Node{CoverMatrixHeader}

"Constructs a `CoverMatrix` from the given `Dict`. (See the module description)"
function CoverMatrix{K,V}(data::Dict{K, Set{V}}; optionals::Set{V} = Set{V}())
  result = CoverMatrix(CoverMatrixHeader())

  # Constructs the `CoverMatrix` row-by-row.
  for (key, subset) in data
    rest = copy(subset)
    # For each row (subset), the `ElementNode`s (colums) that are already
    # present in the matrix are iterated.
    for element_node in element_nodes(result)
      # If the corresponding element is contained in the current subset, a new
      # cell is inserted at the bottom of that column. If there are already
      # cells present in that row, the new cell must also be linked to them.
      # `append_cell!` takes care of this; it will always put the new cell at
      # the right end of the (cyclically linked) row list.
      element = element_node.payload.element
      if in(element, rest)
        delete!(rest, element)
        append_cell!(result, element_node, key)
      end
    end

    # For all subset elements that do not have columns in the matrix yet, we
    # need to create one (with `append_element!`). After that, it's the same as
    # above.
    for element in rest
      isprimary = !in(element, optionals)
      element_node = append_element!(result, element, isprimary)
      append_cell!(result, element_node, key)
    end
  end

  result
end

"""
Creates a new `ElementNode`, attaches it at the end of the element list, and
then returns it.

This function is a helper for the construction of the matrix. It must be called
at most once for each element.
"""
function append_element!(matrix::CoverMatrix, element, isprimary::Bool)
  matrix.payload.visible_primary_element_count += isprimary
  # Note: `matrix` is the sentinel for the (linked cyclic) element list, hence
  #   `matrix.left` is the last `ElementNode`.
  matrix.payload.elements[element] = append_right!(
    matrix.left,
    ElementNode(ElementHeader(element, 0, isprimary))
  )
end

"""
Inserts a new cell into the incidence matrix.

The cell is linked so that it is in `element_node`'s column and in the row
corresponding to the subset identified by `key`. This must be called in
left-to-right, top-down order.
"""
function append_cell!(matrix::CoverMatrix, element_node::ElementNode, key)
  cell = append_down!(element_node.up, Cell(CellData(element_node, key)))
  element_node.payload.count += 1
  if haskey(matrix.payload.subsets, key)
    first_cell = matrix.payload.subsets[key]
    append_right!(first_cell.left, cell)
  else
    matrix.payload.subsets[key] = cell
  end
end

Base.show(io::IO, matrix::CoverMatrix) =
  print(io, "CoverMatrix#$(object_id(matrix))")

Base.size(matrix::CoverMatrix) =
  (length(matrix.payload.subsets), length(matrix.payload.elements))


"""
Creates a `Task` that produces `Node`s from the (cyclic) linked lists.

It starts at some node `root` of the subset matrix and moves along neighboring
nodes until it arrives back at `root`, producing all nodes along the way. Which
direction is taken at each step is determined by `next`. `skip_first` specified
whether `root` should be produced as well.

This function is used as a primitive to simplify iteration.
"""
node_producer(root::Node, next::Function; skip_first = false) = @task let
  if !skip_first
    produce(root)
  end

  current = root |> next
  while current != root
    produce(current)
    current = current |> next
  end
end

element_nodes(node::CoverMatrix) = node_producer(node, right, skip_first = true)
cells(node::ElementNode) = node_producer(node, down, skip_first = true)
cells_reverse(node::ElementNode) = node_producer(node, up, skip_first = true)
cells_in_subset(node::Cell) = node_producer(node, right)
cells_in_subset_reverse(node::Cell) = node_producer(node.left, left)
other_cells_in_subset(node::Cell) =
  node_producer(node, right, skip_first = true)
other_cells_in_subset_reverse(node::Cell) =
  node_producer(node, left, skip_first = true)

"""
Enumerates exact covers and returns their count.

This function implements the DLX algorithm from the referenced paper, but is
slightly extended to allow aborting the search after the first hit as well as
counting the exact covers found.

If the actual covers are relevant (as opposed to simply the count),
`produce_traces` can be enabled. This will record the covered nodes in `stack`,
and cause the stack to be `produce`d when an exact cover is found. At this
point, `stack` contains all choices that led to the exact cover; it contains
the keys of all subsets covered at this point.
"""
function enumerate_exact_covers!(
  matrix::CoverMatrix;
  stack = Cell[],
  only_first = false,
  produce_traces = false
)
  if is_completely_covered(matrix)
    produce_traces && produce(stack)
    return 1
  end

  found = 0
  element_node = find_minimal_element_node(matrix)

  cover_element_node!(matrix, element_node)
  for element_cell in cells(element_node)
    produce_traces && push!(stack, element_cell)
    for cell in other_cells_in_subset(element_cell)
      cover_element_node!(matrix, cell.payload.element_node)
    end

    found += enumerate_exact_covers!(
      matrix,
      stack = stack,
      only_first = only_first,
      produce_traces = produce_traces
    )

    for cell in other_cells_in_subset_reverse(element_cell)
      uncover_element_node!(matrix, cell.payload.element_node)
    end
    produce_traces && pop!(stack)

    if only_first && found > 0
      break
    end
  end
  uncover_element_node!(matrix, element_node)

  found
end

"Determines whether at least one uncovered primary element exists."
is_completely_covered(matrix::CoverMatrix) =
  matrix.payload.visible_primary_element_count == 0

"""
Finds the uncovered primary element node that is contained in the fewest uncovered
subsets.

Precondition: At least one uncovered primary element must exist.
"""
function find_minimal_element_node(matrix::CoverMatrix)
  nodes = element_nodes(matrix)

  minimal_node = consume(nodes)
  for node in nodes
    if !minimal_node.payload.isprimary
      minimal_node = node
    elseif node.payload.isprimary && node.payload.count < minimal_node.payload.count
      minimal_node = node
    end
  end

  minimal_node
end

"Implements the cover operation from the referenced paper."
function cover_element_node!(matrix::CoverMatrix, element_node::ElementNode)
  detach_horizontally!(element_node)
  matrix.payload.visible_primary_element_count -= element_node.payload.isprimary
  for element_cell in cells(element_node)
    for cell in other_cells_in_subset(element_cell)
      detach_vertically!(cell)
      cell.payload.element_node.payload.count -= 1
    end
  end
end

"""
Looks up the `ElementNode` for `element` and then executes an cover operation
on it.
"""
function cover_element!(matrix::CoverMatrix, element)
  cover_element_node!(matrix, matrix.payload.elements[element])
end

"Implements the uncover operation from the referenced paper."
function uncover_element_node!(matrix::CoverMatrix, element_node::ElementNode)
  for element_cell in cells_reverse(element_node)
    for cell in other_cells_in_subset_reverse(element_cell)
      reattach_vertically!(cell)
      cell.payload.element_node.payload.count += 1
    end
  end
  reattach_horizontally!(element_node)
  matrix.payload.visible_primary_element_count += element_node.payload.isprimary
end

"""
Looks up the `ElementNode` for `element` and then executes an uncover operation
on it.
"""
function uncover_element!(matrix::CoverMatrix, element)
  uncover_element_node!(matrix, matrix.payload.elements[element])
end

"""
Similar to `cover_element!`, but covers all elements from the subset identified
by `key`.

This was not in the paper, and is also not used in the DLX algorithm. Rather,
it is public API that can be used to constrain the exact cover search so that
only covers which contain the given subset are considered.

This operation can be undone by `uncover_subset!`, which means that a single
`CoverMatrix` can be reused. TODO sudoku reference
"""
function cover_subset!(matrix::CoverMatrix, key)
  for cell = cells_in_subset(matrix.payload.subsets[key])
    detach_vertically!(cell)
    cover_element_node!(matrix, cell.payload.element_node)
  end
end

"""
Reverses `cover_subset!` for the subset identified by `key`.

This *must not* be called without a preceding call to `cover_subset!` on the
same `key`. `cover_subset!` calls *must* be undone *in reverse order*. Failure
to heed this advice will result in chaos and madness.
"""
function uncover_subset!(matrix::CoverMatrix, key)
  for cell = cells_in_subset_reverse(matrix.payload.subsets[key])
    uncover_element_node!(matrix, cell.payload.element_node)
    reattach_vertically!(cell)
  end
end

"Counts the possible exact covers of `matrix`."
exact_cover_count(matrix::CoverMatrix) = enumerate_exact_covers!(matrix)

"Checks whether an exact cover for `matrix` exists."
exact_cover_exists(matrix::CoverMatrix) =
  0 != enumerate_exact_covers!(matrix, only_first = true)

"""
Returns a `Task` that produces the possible exact covers of `matrix`.

The produced covers are `Set`s of keys that identify the actual subsets,
as defined when `matrix` was constructed. If `only_first` is set, this function
aborts after the first found exact cover.

During enumeration (i.e. when a value is consumed) the `matrix` is in a "dirty"
state; it should then not be used to execute another search or be modified
by `cover_subset!` or `uncover_subset!`.
"""
exact_cover_producer(matrix::CoverMatrix; only_first = false) = @task let
  covers = @task enumerate_exact_covers!(
    matrix,
    only_first = only_first,
    produce_traces = true
  )

  for cover in covers
    keys = map(cell -> cell.payload.subset_key, cover)
    set = Set(keys)
    produce(set)
  end
end

"""
Searches for the first exact cover of `matrix` and returns it.

If none is found, `nothing` is returned.
"""
function find_exact_cover(matrix::CoverMatrix)
  producer = exact_cover_producer(matrix, only_first = true)
  cover = consume(producer)
  consume(producer) # <- We need to do this because the CoverMatrix would not be
                    #    restored to its original state otherwise, as the
                    #    producing task would simply 'hang' after the cover was
                    #    found. This forces the producer to finish.
  cover
end

end
