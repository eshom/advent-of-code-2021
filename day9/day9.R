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


mat <- read_input("input.txt")

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
low_points <- which(mat_lowest, arr.ind = TRUE)



## `points` must be a matrix of one row with indice (row, col).
find_basin_size <- function(mat, points) {
        nrows <- nrow(mat)
        ncols <- ncol(mat)

        basin_size <- function(start, n = 0) {
                ## Not part of the basin
                if (start[1] > nrows || start[2] > ncols || start[1] <= 0 || start[2] <= 0 || mat[start] >= 9)
                        return(n)

                n <- n + 1
                mat[start] <<- 10 # mark counted

                start_l <- start
                start_r <- start
                start_u <- start
                start_d <- start

                start_l[2] <- start[2] - 1
                start_r[2] <- start[2] + 1
                start_u[1] <- start[1] - 1
                start_d[1] <- start[1] + 1

                n <- Recall(start_l, n)
                n <- Recall(start_r, n)
                n <- Recall(start_u, n)
                n <- Recall(start_d, n)

                n
        }

        basin_size(points)
}

sizes <- apply(low_points, 1, \(x) find_basin_size(mat, matrix(x, ncol = 2)))

prod(sort(sizes, decreasing = TRUE)[1:3])
