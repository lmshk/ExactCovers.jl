# This implements a CoverSet type along with methods to find one or enumerate
# all exact covers. To use it, create an instance and then push subsets into it:
#
#     set = CoverSet()
#     push!(set, [1, 2])
#     push!(set, [2, 3], key = :second)     # Subsets may optionally be labeled.
#     push!(set, [3, 4])  # If they are not, an integer label will be generated.
#     push!(set, [4, (:abc, 2)])   # Key and elements may be of arbitrary types.
#
#     # Find first exact cover, or ():
#     cover = find_exact_cover(set)
#     # ^ cover is a collection of subset keys.
#
#     # Alternatively, iterate over all covers:
#     for cover in exact_cover_producer(set)
#       println(cover)
#     end
#
# Both operations may change the CoverSet during operation, but will restore its
# original state before returning/finishing. (But when using
# exact_cover_producer, the set may be in a modified state during iteration.)
#
# There are also corresponding functions exact_cover_exists and
# exact_cover_count that do not produce the actual covers, but only count them.
#
# The implementation is Knuth's "Algorithm X" with Dancing Links (DLX). See:
#
#     http://www-cs-faculty.stanford.edu/~uno/papers/dancing-color.ps.gz
#
# Individual elements (columns) and subsets (rows) can be covered before
# searching for the exact cover:
#
#   cover_subset!(set, :second)
#   cover = find_exact_cover(set)
#
# This means that CoverSets can be reused if all cover-operations are undone
# by calling the respective uncover_...! functions in reverse order.
#
module CoverSets

export
  CoverSet,
  exact_cover_exists,
  exact_cover_count,
  exact_cover_producer,
  find_exact_cover,
  cover_element!,
  uncover_element!,
  cover_subset!,
  uncover_subset!


# This defines the Node type, of which there are three specializations: The root
# (CoverSet), the column/element headers (ElementNode) and the actual cells
# representing the entries in the incidence matrix (Cell), where each has a
# different "payload". All nodes are members of two cyclic lists, one horizontal
# and one vertical. The ElementNodes and CoverSet act as list sentinels and are
# only linked to themselves on creation.
#
# There are some general functions on Nodes to simplify code further down. These
# are:
#   - accessors for their neighbors,
#   - functions to temporarily detach and reattach them from their containing
#     lists, and
#   - functions to initially insert them into the lists at specified positions.

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


# The following definitions set up the Node specializations.

type ElementHeader
  element
  count::Int
end

typealias ElementNode Node{ElementHeader}

type CellData
  element_node::ElementNode
  subset_key
end

typealias Cell Node{CellData}

type CoverSetHeader
  elements::Dict{Any, ElementNode}
  subsets::Dict{Any, Cell}

  CoverSetHeader() = new(Dict{Any, ElementNode}(), Dict{Any, Cell}())
end

typealias CoverSet Node{CoverSetHeader}
CoverSet() = CoverSet(CoverSetHeader())
Base.show(io::IO, x::CoverSet) = print(io, "CoverSet#$(object_id(x))")

Base.size(set::CoverSet) =
  (length(set.payload.subsets), length(set.payload.elements))


# These producers simplify iteration over the linked lists later on.

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

element_nodes(node::CoverSet) = node_producer(node, right, skip_first = true)
cells(node::ElementNode) = node_producer(node, down, skip_first = true)
cells_reverse(node::ElementNode) = node_producer(node, up, skip_first = true)
cells_in_subset(node::Cell) = node_producer(node, right)
cells_in_subset_reverse(node::Cell) = node_producer(node.left, left)
other_cells_in_subset(node::Cell) =
  node_producer(node, right, skip_first = true)
other_cells_in_subset_reverse(node::Cell) =
  node_producer(node, left, skip_first = true)


# The following functions implement the push! operation, which grows the grid
# dynamically. For this purpose, the CoverSetHeader contains two dictionaries
# that index the columns (element incidences) and contained rows (subsets) of
# the grid respectively, and which were not in the paper.

function find_free_key(dictionary::Dict)
  key = length(dictionary) + 1
  while haskey(dictionary, key)
    key += 1
  end

  key
end

function append_element!(set::CoverSet, element)
  if !haskey(set.payload.elements, element)
    element_node = append_right!(set.left, ElementNode(ElementHeader(element, 0)))
    set.payload.elements[element] = element_node
    return element_node
  end
end

function append_cell!(set::CoverSet, element_node::ElementNode, subset_key)
  cell = append_down!(element_node.up, Cell(CellData(element_node, subset_key)))
  element_node.payload.count += 1
  if haskey(set.payload.subsets, subset_key)
    first_cell = set.payload.subsets[subset_key]
    append_right!(first_cell.left, cell)
  else
    set.payload.subsets[subset_key] = cell
  end
end

function Base.push!(set::CoverSet, subset; key = nothing)
  if key == nothing
    key = find_free_key(set.payload.subsets)
  elseif haskey(set.payload.subsets, key)
    throw(KeyError("Cannot push subset with already existing key."))
  end

  rest = Set(subset)
  for element_node in element_nodes(set)
    element = element_node.payload.element
    if in(element, rest)
      delete!(rest, element)
      append_cell!(set, element_node, key)
    end
  end

  for element in rest
    element_node = append_element!(set, element)
    append_cell!(set, element_node, key)
  end
end


# The following functions implement the DLX algorithm from the referenced paper.
# walk_exact_covers! is the main routine, and is slightly extended to allow
# aborting the search after the first hit as well as counting the found exact
# covers. If the actual covers are relevant (as opposed to simply the count), it
# is run in a Task and will produce the found exact covers.
#
# cover_element! and uncover_element! implement the cover- and
# uncover-operations from the paper. cover_subset! and uncover_subset! are not
# in the paper and execute a similar operation, but for rows instead of columns.

function find_minimal_element_node(set::CoverSet)
  nodes = element_nodes(set)
  minimal_node = consume(nodes)
  for node in nodes
    if node.payload.count < minimal_node.payload.count
      minimal_node = node
    end
  end

  minimal_node
end

function cover_element!(element_node::ElementNode)
  detach_horizontally!(element_node)
  for element_cell in cells(element_node)
    for cell in other_cells_in_subset(element_cell)
      detach_vertically!(cell)
      cell.payload.element_node.payload.count -= 1
    end
  end
end

function cover_element!(set::CoverSet, element)
  cover_element!(set.payload.elements[element])
end

function uncover_element!(element_node::ElementNode)
  for element_cell in cells_reverse(element_node)
    for cell in other_cells_in_subset_reverse(element_cell)
      reattach_vertically!(cell)
      cell.payload.element_node.payload.count += 1
    end
  end
  reattach_horizontally!(element_node)
end

function uncover_element!(set::CoverSet, element)
  uncover_element!(set.payload.elements[element])
end

function cover_subset!(set::CoverSet, key)
  for cell = cells_in_subset(set.payload.subsets[key])
    detach_vertically!(cell)
    cover_element!(cell.payload.element_node)
  end
end

function uncover_subset!(set::CoverSet, key)
  for cell = cells_in_subset_reverse(set.payload.subsets[key])
    uncover_element!(cell.payload.element_node)
    reattach_vertically!(cell)
  end
end

is_completely_covered(set::CoverSet) = set.right == set

# Returns the number of exact covers.
function walk_exact_covers!(
  set::CoverSet,
  stack::Vector{Cell};
  only_first = false,
  produce_traces = false
)
  if is_completely_covered(set)
    produce_traces && produce(copy(stack))
    return 1
  end

  found = 0
  element_node = find_minimal_element_node(set)

  cover_element!(element_node)
  for element_cell in cells(element_node)
    produce_traces && push!(stack, element_cell)
    for cell in other_cells_in_subset(element_cell)
      cover_element!(cell.payload.element_node)
    end

    found += walk_exact_covers!(
      set,
      stack,
      only_first = only_first,
      produce_traces = produce_traces
    )

    for cell in other_cells_in_subset_reverse(element_cell)
      uncover_element!(cell.payload.element_node)
    end
    produce_traces && pop!(stack)

    if only_first && found > 0
      break
    end
  end
  uncover_element!(element_node)

  found
end

exact_cover_count(set::CoverSet) = walk_exact_covers!(set, Cell[])

exact_cover_exists(set::CoverSet) = walk_exact_covers!(
  set,
  Cell[],
  only_first = true
) != 0

exact_cover_producer(set::CoverSet; only_first = false) = @task let
  covers = @task walk_exact_covers!(
    set,
    Cell[],
    only_first = only_first,
    produce_traces = true
  )

  for cover in covers
    produce(map(cell -> cell.payload.subset_key, cover))
  end
end

function find_exact_cover(set::CoverSet)
  producer = exact_cover_producer(set, only_first = true)
  cover = consume(producer)
  consume(producer) # <- We need to do this because the CoverSet would not be
                    #    restored to its original state otherwise, as the
                    #    producing task would simply 'hang' after the cover was
                    #    found. This forces the producer to finish.
  cover
end

end
