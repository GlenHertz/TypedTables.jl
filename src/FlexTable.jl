# A `Table` presents itself as an `AbstractArray` of `NamedTuples`s

"""
    FlexTable(name1 = array1, ...)

Create a column-storage-based `FlexTable` with column names `name1`, etc, from arrays `array1`,
etc. The input arrays `array1`, etc, must share the same dimensionality and indices.

`FlexTable` itself is an `AbstractArray` whose elements are `NamedTuple`s of the form
`(name1 = first(array1), ...)`, etc. Rows of the table are obtained via standard array
indexing `table[i]`, and columns via `table.name`.

`FlexTable` differs from `Table` in that the columns are mutable - you may add, remove,
rename and replace entire columns of a `FlexTable`, but not a `Table`. However, `Table` can
access and iterate rows in local scope with fast, fully type-inferred code while `FlexTable`
will be more efficient with a higher-order interface.
"""
mutable struct FlexTable{N} <: AbstractArray{NamedTuple, N}
    data::NamedTuple{<:Any, <:Tuple{Vararg{AbstractArray{<:Any, N}}}}

    # Inner constructor, to compare axes?
end

FlexTable(;kwargs...) = FlexTable(kwargs.data)
FlexTable(nt::NamedTuple) = FlexTable{_ndims(nt)}(nt)

function FlexTable(x)
    cols = columns(x)
    if cols isa NamedTuple{<:Any, <:Tuple{Vararg{AbstractArray{N}} where N}}
        return FlexTable(cols)
    else
        return FlexTable(columntable(cols))
    end
end
FlexTable(t::Table) = FlexTable(columns(t))
Table(t::FlexTable) = Table(columns(t))

FlexTable{N}(t::Table{<:Any, N}) where {N} = FlexTable{N}(columns(t))

Tables.AccessStyle(::FlexTable) = Tables.ColumnAccess()
Tables.schema(t::FlexTable) = _eltypes(columns(t))

"""
    columns(dataframe::FlexTable)

Convert a `FlexTable` into a `NamedTuple` of it's columns.
"""
@inline columns(t::FlexTable) = getfield(t, :data)

@inline rows(t::FlexTable) = Table(columns(t))

# Simple column access via `table.columnname`
@inline Base.getproperty(t::FlexTable, name::Symbol) = getproperty(columns(t), name)

# Private fields are never exposed since they can conflict with column names
Base.propertynames(t::FlexTable, private::Bool=false) = columnnames(t)

function Base.setproperty!(t::FlexTable, name::Symbol, a::AbstractArray)
    setfield!(t, :data, merge(columns(t), NamedTuple{(name,)}((a,))))
    return t
end

"""
    columnnames(table)

Return a tuple of the column names of a `Table`.
"""
columnnames(t::FlexTable) = keys(columns(t))


# Basic AbstractArray interface

@inline Base.size(t::FlexTable{N}) where {N} = size(first(columns(t)))::NTuple{N, Integer}
@inline Base.axes(t::FlexTable{N}) where {N} = axes(first(columns(t)))::NTuple{N, Any}
@inline Base.IndexStyle(t::FlexTable) = IndexStyle(first(columns(t)))

function Base.checkbounds(::Type{Bool}, t::FlexTable, i...)
    # Make sure we are in bounds for *every* column. Only safe to do
    # here because each column might mutate size independently of the others!
    all(col -> checkbounds(Bool, col, i...), columns(t))
end

@inline function Base.getindex(t::FlexTable, i::Int)
    @boundscheck checkbounds(t, i)
    map(col -> @inbounds(getindex(col, i)), columns(t))::NamedTuple
end

@inline function Base.getindex(t::FlexTable, i::Int...)
    @boundscheck checkbounds(t, i...)
    map(col -> @inbounds(getindex(col, i...)), columns(t))::NamedTuple
end

@inline function Base.setindex!(t::FlexTable, v::NamedTuple, i::Int)
    @boundscheck begin
        checkbounds(t, i)
        @assert keys(v) === keys(columns(t))
    end
    map((val, col) -> @inbounds(setindex!(col, val, i)), v, columns(t))
    return t
end

@inline function Base.setindex!(t::FlexTable, v::NamedTuple, i::Int...)
    @boundscheck begin
        checkbounds(t, i)
        @assert keys(v) === keys(columns(t))
    end
    map((val, col) -> @inbounds(setindex!(col, val, i...)), v, columns(t))
    return t
end

# similar
@inline Base.similar(t::FlexTable{N}) where {N} = FlexTable{N}(similar(Table(t)))::FlexTable{N}
@inline Base.similar(t::FlexTable{N}, ::Type{NamedTuple}) where {N} = FlexTable{N}(similar(Table(t)))
@inline Base.similar(t::FlexTable{N}, ::Type{NamedTuple{names,T}}) where {N, names, T} = FlexTable{N}(similar(Table(t, NamedTuple{names, T})))
@inline Base.similar(t::FlexTable, ::Type{NamedTuple}, dims) = FlexTable{__ndims(dims)}(similar(Table(t), _eltypes(columns(t)), dims))
@inline Base.similar(t::FlexTable, ::Type{NamedTuple{names,T}}, dims) where {names, T} = FlexTable{__ndims(dims)}(similar(Table(t), NamedTuple{names, T}, dims))

# Ambiguities...
@inline Base.similar(t::FlexTable, ::Type{NamedTuple}, dims::Union{Integer, AbstractUnitRange}...) = FlexTable{__ndims(dims)}(similar(Table(t), _eltypes(columns(t)), dims))
@inline Base.similar(t::FlexTable, ::Type{NamedTuple}, dims::Tuple{Vararg{Int64,N}}) where {N} = FlexTable{__ndims(dims)}(similar(Table(t), _eltypes(columns(t)), dims))
@inline Base.similar(t::FlexTable, ::Type{NamedTuple}, dims::Tuple{Union{Integer, OneTo},Vararg{Union{Integer, OneTo}}}) = FlexTable{__ndims(dims)}(similar(Table(t), _eltypes(columns(t)), dims))

@inline Base.similar(t::FlexTable, ::Type{NamedTuple{names, T}}, dims::Union{Integer, AbstractUnitRange}...) where {names, T} = FlexTable{__ndims(dims)}(similar(Table(t), NamedTuple{names, T}, dims))
@inline Base.similar(t::FlexTable, ::Type{NamedTuple{names, T}}, dims::Tuple{Vararg{Int64,N}}) where {N, names, T} = FlexTable{__ndims(dims)}(similar(Table(t), NamedTuple{names, T}, dims))
@inline Base.similar(t::FlexTable, ::Type{NamedTuple{names, T}}, dims::Tuple{Union{Integer, OneTo},Vararg{Union{Integer, OneTo}}}) where {names, T} = FlexTable{__ndims(dims)}(similar(Table(t), NamedTuple{names, T}, dims))

@inline __ndims(::Integer) = 1
@inline __ndims(::AbstractUnitRange) = 1
@inline __ndims(::NTuple{N, Any}) where {N} = N

# empty
Base.empty(t::FlexTable) = empty(t, _eltypes(columns(t)))
function Base.empty(t::FlexTable, ::Type{NamedTuple{names, T}}) where {names, T <: Tuple}
    FlexTable(empty(Table(t)))
end

# Support Vector / deque interface (mutable-length vectors)

function Base.empty!(t::FlexTable)
    map(empty!, columns(t))
    return t
end

function Base.pop!(t::FlexTable)
    return map(pop!, columns(t))::NamedTuple
end

function Base.push!(t::FlexTable, v::NamedTuple)
    map(push!, columns(t), v)
    return t
end

function Base.append!(t::Union{FlexTable, Table}, t2::Union{FlexTable, Table})
    map(append!, columns(t), columns(t2))
    return t
end

function Base.popfirst!(t::FlexTable)
    return map(popfirst!, columns(t))::NamedTuple
end

function Base.pushfirst!(t::FlexTable, v::NamedTuple)
    map(pushfirst!, columns(t), v)
    return t
end

function Base.prepend!(t::Union{FlexTable, Table}, t2::Union{FlexTable, Table})
    map(prepend!, columns(t), columns(t2))
    return t
end

function Base.deleteat!(t::FlexTable, i)
    map(col -> deleteat!(col, i), columns(t))
    return t
end

function Base.insert!(t::FlexTable, i::Integer, v::NamedTuple)
    map((col, val) -> insert!(col, i, val), columns(t), v)
    return t
end

function Base.splice!(t::FlexTable, inds::Integer)
    return map(col -> splice!(col, inds), columns(t))::NamedTuple
end

function Base.splice!(t::FlexTable, inds::AbstractVector)
    return FlexTable{1}(map(col -> splice!(col, inds), columns(t)))
end

function Base.splice!(t::FlexTable, inds::Integer, ins::NamedTuple)
    return map((col, vals) -> splice!(col, inds, vals), columns(t), ins)::NamedTuple
end

function Base.splice!(t::FlexTable, inds::AbstractVector, ins::NamedTuple)
    return FlexTable{1}(map((col, vals) -> splice!(col, inds, vals), columns(t), ins))
end

function Base.splice!(t::Union{FlexTable, Table}, inds::Integer, ins::Union{FlexTable, Table})
    return map((col, vals) -> splice!(col, inds, vals), columns(t), columns(ins))::NamedTuple
end

function Base.splice!(t::Union{FlexTable, Table}, inds::AbstractVector, ins::Union{FlexTable, Table})
    return FlexTable{1}(map((col, vals) -> splice!(col, inds, vals), columns(t), columns(ins)))
end

# TODO splicing in an `AbstractArray{<:NamedTuple}` should be possible...

# Speedups for column-based storage

function Base.getindex(t::FlexTable, inds::Union{AbstractArray, Colon}...)
    return FlexTable{_getindex_dims(inds)}(map(col -> getindex(col, inds...), columns(t)))
end

function Base.view(t::FlexTable, inds::Union{AbstractArray, Colon}...)
    return FlexTable{_getindex_dims(inds)}(map(col -> view(col, inds...), columns(t)))
end

@inline _getindex_dims(inds) = __getindex_dims(0, inds...)
@inline __getindex_dims(n::Int) = n
@inline __getindex_dims(n::Int, ::Int, inds...) = __getindex_dims(n, inds...)
@inline __getindex_dims(n::Int, ::AbstractArray{<:Any, m}, inds...) where {m} = __getindex_dims(n + m, inds...)
@inline __getindex_dims(n::Int, ::Colon, inds...) = __getindex_dims(n + 1, inds...)

# Deprecated for .= syntax (via Base.Broadcast.materialize!)
# It seems `Ref` might be the new cool here. Could also consider `AbstractArray{<:NamedTuple, 0}`?
#function Base.setindex!(t::Table{<:NamedTuple{names}}, v::NamedTuple{names}, inds::Union{AbstractArray, Colon}...) where {names}
#    map((col, val) -> setindex!(col, val, inds...), columns(t), v)
#    return t
#end

function Base.setindex!(t::FlexTable, t2::Union{FlexTable, Table}, inds::Union{AbstractArray, Colon}...)
    map((col, col2) -> setindex!(col, col2, inds...), columns(t), columns(t2))
    return t
end

function Base.vcat(t::Union{FlexTable, Table}, t2::Union{FlexTable, Table})
    return FlexTable{_vcat_ndims(ndims(t), ndims(t2))}(map(vcat, columns(t), columns(t2)))
end

function Base.hcat(t::Union{FlexTable, Table}, t2::Union{FlexTable, Table})
    return FlexTable{_hcat_ndims(ndims(t), ndims(t2))}(map(hcat, columns(t), columns(t2)))
end

function Base.hvcat(rows::Tuple{Vararg{Int}}, ts::Union{FlexTable, Table}...)
    return FlexTable(map((cols...,) -> hvcat(rows, cols...), map(columns, ts)...))
end

@pure function _vcat_ndims(i::Int, j::Int)
    max(i, j, 1)
end

@pure function _hcat_ndims(i::Int, j::Int)
    max(i, j, 2)
end

function Base.resize!(t::FlexTable, len::Int)
    for c in columns(t)
        resize!(c, len)
    end
    t
end
