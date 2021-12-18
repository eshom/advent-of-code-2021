read_input <- function(filepath) {
        readLines(filepath) |> strsplit("") |> do.call(what = rbind) -> out
        storage.mode(out) <- "integer"
        out
}


## Puzzle one

## New rows/columns are 10 by default, which is above the maximum value.
## This is so nothing can be above new rows/columns
shift_up <- function(mat) {
        rbind(mat[-1, ], 10)
}

shift_down <- function(mat) {
        rbind(10, mat[-nrow(mat), ])
}

shift_left <- function(mat) {
        cbind(mat[, -1], 10)
}

shift_right <- function(mat) {
        cbind(10, mat[, -ncol(mat)])
}


mat <- read_input("input-eg.txt")

mat_d <- shift_up(mat)
mat_u <- shift_down(mat)
mat_l <- shift_right(mat)
mat_r <- shift_left(mat)

mat_lowest <- mat < mat_d &
        mat < mat_u &
        mat < mat_l &
        mat < mat_r

sum(mat[mat_lowest] + 1)

## Puzzle two
