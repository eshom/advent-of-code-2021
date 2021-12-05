## The output is a list of vectors of length 4, each elements corresponds
## to x1, y1, x2, y2 respectively
read_input <- function(filepath = c("input.txt", "example.txt"), eg = TRUE) {
        if (eg) {
                filepath <- filepath[2]
        } else {
                filepath <- filepath[1]
        }

        out <- readLines(filepath)

        gregexpr(r"{\d+}", out) |> regmatches(x = out) |> lapply(as.integer)
}

## Puzzle one

is_hv <- function(inp) {
        inp[1] == inp[3] || inp[2] == inp[4]
}

## Generate a data frames with only horizontal/vertical lines
get_hv_lines <- function(input) {

        input <- Filter(is_hv, input)

        ## We also add one to all points, because R indexing starts with 1
        f <- function(inp) {
                data.frame(x = seq(inp[1] + 1, inp[3] + 1), y = seq(inp[2] + 1, inp[4] + 1))
        }

        lapply(input, f)
}

## Returns an environment with the diagram in it, for reference semantics later
gen_diagram <- function(lines) {
        e <- new.env()

        do.call(rbind, lines) |> apply(2, max) -> dims

        e$mat <- matrix(0, nrow = dims["x"], ncol = dims["y"])
        e
}

## This function updates the matrix in the diagram environment
update_diagram <- function(diag_env, lines) {
        update <- function(l) {
                if (exists("diagon", l) && all(l$diagon)) {
                        diag(diag_env$mat[l$x, l$y]) <- diag(diag_env$mat[l$x, l$y] + 1)
                } else {
                        diag_env$mat[l$x, l$y] <- diag_env$mat[l$x, l$y] + 1
                }
        }

        lapply(lines, update)

        invisible(NULL) # Nothing should be returned
}

read_input(eg = FALSE) |> get_hv_lines() -> lines

e_diagram <- gen_diagram(lines)

update_diagram(e_diagram, lines)

sum(e_diagram$mat >= 2)

## puzzle two
get_all_lines <- function(input) {

        diag_lines <- !sapply(input, is_hv)

        f <- function(inp, diagon) {
                x1 <- inp[1] + 1
                x2 <- inp[3] + 1
                y1 <- inp[2] + 1
                y2 <- inp[4] + 1
                data.frame(x = seq(x1, x2), y = seq(y1, y2), diagon = diagon)
        }

        Map(f, input, diag_lines)
}

read_input(eg = FALSE) |> get_all_lines() -> lines

e_diagram <- gen_diagram(lines)

update_diagram(e_diagram, lines)

sum(e_diagram$mat >= 2)

## Heatmap
##par(bg = "black")
##image(e_diagram$mat, col = viridis::magma(12))
