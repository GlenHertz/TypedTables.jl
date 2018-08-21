function Base.show(io::IO, ::MIME"text/plain", t::T) where {T <: Union{Table,FlexTable}}
    showtable(io, t)
end
function Base.show(io::IO, t::T) where {T <: Union{Table,FlexTable}}
    showtable(io, t)
end

showtable(t; kwargs...) = showtable(stdout, t; kwargs...)

function showtable(io::IO, t::T;
                   allcols::Bool=false,
                   rowlabel::Symbol=:Row,
                   compact::Bool=true,
                   displaysummary::Bool=true) where {T<:Union{Table,FlexTable}}
    if compact
        io = IOContext(io, :compact=>compact)
    end
    nrows = size(t)[1]
    disp_rows, disp_cols = displaysize(io)
    disp_max_rows = disp_rows - 5
    if nrows <= disp_max_rows
        rowindices1 = 1:nrows
        rowindices2 = 1:0
    else
        half_max_rows = disp_max_rows ÷ 2
        bound = min(half_max_rows - 1, nrows)
        rowindices1 = 1:bound
        rowindices2 = max(bound + 1, nrows - half_max_rows + 1):nrows
    end
    maxwidths = getmaxwidths(io, t, rowindices1, rowindices2, rowlabel)
    showrows(io,
             t,
             rowindices1,
             rowindices2,
             maxwidths,
             !allcols,
             allcols,
             rowlabel,
             displaysummary)
    return
end

function showrows(io::IO,
                  t::T,
                  rowindices1::AbstractVector{Int},
                  rowindices2::AbstractVector{Int},
                  maxwidths::AbstractVector{Int},
                  splitchunks::Bool = false,
                  allcols::Bool = true,
                  rowlabel::Symbol = :Row,
		  displaysummary::Bool = true) where {T <: Union{Table, FlexTable}}

    if isempty(rowindices1)
        if displaysummary
            println(io, summary(t))
        end
        return
    end

    ncols = length(columnnames(t))
    labelmaxwidth = maxwidths[end]
    chunkbounds = getchunkbounds(maxwidths, splitchunks, displaysize(io)[2])
    nchunks = allcols ? length(chunkbounds) - 1 : min(length(chunkbounds) - 1, 1)

    # Print summary:
    if displaysummary
        println(io, summary(t))
    end

    for chunkindex in 1:nchunks
        leftcol = chunkbounds[chunkindex] + 1
        rightcol = chunkbounds[chunkindex + 1]

        # Print column names
        print(io, "│ ", rowlabel)
        padding = labelmaxwidth - ourstrwidth(rowlabel)
        print(io, " " ^ padding)
        print(io, " │")

        for c in leftcol:rightcol
            s = columnnames(t)[c]
            print(io, " ", s)
            padding = maxwidths[c] - ourstrwidth(s)
            print(io, " " ^ padding)
            if c == rightcol
                print(io, " │\n")
            else
                print(io, " │")
            end
        end

        # Print table bounding line
        print(io, '├')
        for itr in 1:(labelmaxwidth + 2)
            print(io, '─')
        end
        print(io, '┼')
        for c in leftcol:rightcol
            for itr in 1:(maxwidths[c] + 2)
                print(io, '─')
            end
            if c < rightcol
                print(io, '┼')
            else
                print(io, '┤')
            end
        end
        print(io, '\n')

        # Print main table body, potentially in two abbreviated sections
        showrowindices(io,
                       t,
                       rowindices1,
                       maxwidths,
                       leftcol,
                       rightcol)

        if !isempty(rowindices2)
            print(io, "\n⋮\n")
            showrowindices(io,
                           t,
                           rowindices2,
                           maxwidths,
                           leftcol,
                           rightcol)
        end

        # Print newlines to separate chunks
        if chunkindex < nchunks
            print(io, "\n\n")
        end
    end

    if !allcols && length(chunkbounds) > 2
        C = chunkbounds[end] - chunkbounds[2]
        s = C == 1 ? "" : "s"
        print(io, "\nNote: Omitted printing $C column$s: ")
        join(io, columnnames(t)[end-C+1:end], ", ")
        println(io)
    end


    return
end

function showrowindices(io::IO,
                        t::T,
                        rowindices::AbstractVector{Int},
                        maxwidths::Vector{Int},
                        leftcol::Int,
			rightcol::Int) where {T <: Union{Table, FlexTable}}
    labelmaxwidth = maxwidths[end]

    rows_t = rows(t)
    for r in rowindices
        # Print row ID
        print(io, "│ ", r)
        padding = labelmaxwidth - ndigits(r)
        print(io, " " ^ padding)
        print(io, " │ ")

        # Print table row
        for c in leftcol:rightcol
            strlen = 0
            s = rows_t[r][c]
            str = ourstr(s)
            strlen = ourstrwidth(str)
            if ismissing(s)
                printstyled(io, s, color=:light_black)
            elseif s === nothing
                strlen = 0
            else
                print(io, str)
            end
            padding = maxwidths[c] - strlen
            print(io, " " ^ padding)
            if c == rightcol
                if r == rowindices[end]
                    print(io, " │")  # is this right?
                else
                    print(io, " │\n")
                end
            else
                print(io, " │ ")
            end
        end
    end
    return
end

function summary(t::Table)
    nrow = size(t)[1]
    ncol = length(columnnames(t))
    string(nrow, "x", ncol, " Table")
end

function summary(t::FlexTable)
    nrow = size(t)[1]
    ncol = length(columnnames(t))
    string(nrow, "x", ncol, " FlexTable")
end

function getmaxwidths(io::IO, t::T, rowindices1::AbstractVector{Int}, rowindices2::AbstractVector{Int}, rowlabel::Symbol)::AbstractVector{Int} where {T <: Union{Table, FlexTable}}
    ncol = length(columnnames(t))
    maxwidths = fill(0, ncol + 1)

    # columns from table:
    for (c, name) in enumerate(columnnames(t))
        # 1) consider length of column name
        maxwidth = ourstrwidth(name)
        # 2) consider length of longest entry in column
        col = getproperty(t, name)
        for indices in (rowindices1, rowindices2), idx in indices
            maxwidth = max(maxwidth, ourstrwidth(col[idx]))
        end
        maxwidths[c] = maxwidth
    end

    # Calc row index width (stored as extra entry in maxwidths)
    labelwidth = ourstrwidth(rowlabel)
    rowmaxwidth1 = isempty(rowindices1) ? 0 : ndigits(last(rowindices1))
    rowmaxwidth2 = isempty(rowindices2) ? 0 : ndigits(last(rowindices2))
    maxwidths[end] = max(labelwidth, rowmaxwidth1, rowmaxwidth2)

    return maxwidths
end

#' @description
#'
#' When rendering an Table to a REPL window in chunks, each of
#' which will fit within the width of the REPL window, this function will
#' return the indices of the columns that should be included in each chunk.
#'
#' NOTE: The resulting bounds should be interpreted as follows: the
#'       i-th chunk bound is the index MINUS 1 of the first column in the
#'       i-th chunk. The (i + 1)-th chunk bound is the EXACT index of the
#'       last column in the i-th chunk. For example, the bounds [0, 3, 5]
#'       imply that the first chunk contains columns 1-3 and the second chunk
#'       contains columns 4-5.
#'
#' @param maxwidths::Vector{Int} The maximum width needed to render each
#'        column of an Table.
#' @param splitchunks::Bool Should the output be split into chunks at all or
#'        should only one chunk be constructed for the entire
#'        Table?
#' @param availablewidth::Int The available width in the REPL.
#'
#' @returns chunkbounds::Vector{Int} The bounds of each chunk of columns.
#'
#' @examples
#'
#' t = Table(A = 1:3, B = ["x", "yy", "z"])
#' maxwidths = getmaxwidths(t, 1:1, 3:3, "Row")
#' chunkbounds = getchunkbounds(maxwidths, true)
function getchunkbounds(maxwidths::Vector{Int},
                        splitchunks::Bool,
                        availablewidth::Int=displaysize()[2]) # -> Vector{Int}
    ncols = length(maxwidths) - 1
    labelmaxwidth = maxwidths[end]
    if splitchunks
        chunkbounds = [0]
        # Include 2 spaces + 2 | characters for row/col label
        totalwidth = labelmaxwidth + 4
        for c in 1:ncols
            # Include 2 spaces + | character in per-column character count
            totalwidth += maxwidths[c] + 3
            if totalwidth > availablewidth
                push!(chunkbounds, c - 1)
                totalwidth = labelmaxwidth + 4 + maxwidths[c] + 3
            end
        end
        push!(chunkbounds, ncols)
    else
        chunkbounds = [0, ncols]
    end
    return chunkbounds
end

let
    local io = IOBuffer(Vector{UInt8}(undef, 300), read=true, write=true)
    global ourstr
    function ourstr(x::Any)
        truncate(io, 0)
        ourshowcompact(io, x)
        String(take!(io))
    end
end

ourstrwidth(x::Any) = textwidth(ourstr(x))

ourshowcompact(io::IO, x::Any) = show(IOContext(io, :compact=>true), x)
ourshowcompact(io::IO, x::AbstractString) = escape_string(io, x, "")
ourshowcompact(io::IO, x::Symbol) = ourshowcompact(io, string(x))
ourshowcompact(io::IO, x::Nothing) = ""

