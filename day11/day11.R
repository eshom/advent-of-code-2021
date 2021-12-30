read_input <- function(example = TRUE) {
        if (example) {
                out <- scan(text = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526", quiet = TRUE) |> as.character() |> strsplit("") |>
        lapply(as.integer) |> do.call(what = rbind)
        } else {
                out <- scan(text = "5212166716
1567322581
2268461548
3481561744
6248342248
6526667368
5627335775
8124511754
4614137683
4724561156", quiet = TRUE) |> as.character() |> strsplit("") |>
        lapply(as.integer) |> do.call(what = rbind)
        }

        out
}

mini_example <- function() {
        scan(text = "11111
19991
19191
19991
11111", quiet = TRUE) |> as.character() |> strsplit("") |>
        lapply(as.integer) |> do.call(what = rbind)
}

## Puzzle one
step_sub1 <- function(octo) {
        octo + 1
}

step_sub2 <- function(octo) {
        octo_flash <- octo >= 10
        flashed <- octo_flash & FALSE

        increase_adj <- function(rowcol) {
                rowcol <- matrix(rowcol, ncol = 2)
                u <- rowcol + c(-1, 0)
                d <- rowcol + c(1, 0)
                l <- rowcol + c(0, -1)
                r <- rowcol + c(0, 1)
                ul <- rowcol + c(-1, -1)
                ur <- rowcol + c(-1, 1)
                dl <- rowcol + c(1, -1)
                dr <- rowcol + c(1, 1)

                l <- list(u, d, l, r, ul, ur, dl, dr, rowcol)
                l <- Filter(\(x) !(any(x == 0) || x[1] > nrow(octo) ||
                                 x[2] > ncol(octo)), l)

                lapply(l, \(x) octo[x] <<- octo[x] + 1)
        }

        while(any(octo_flash)) {
                flash_pos <- which(octo_flash, arr.ind = TRUE)
                apply(flash_pos, 1, increase_adj)
                flashed <- flashed | octo_flash
                octo_flash <- (octo >= 10) & !flashed ## Can only flash once
        }

        attr(octo, "tot_flash") <- attr(octo, "tot_flash") + sum(flashed)
        octo
}

step_sub3 <- function(octo) {
        octo[octo >= 10] <- 0
        octo
}

step <- function(octo) {
        octo |> step_sub1() |> step_sub2() |> step_sub3()
}

step_n <- function(octo, n) {
        for (i in 1:n) {
                octo <- step(octo)
        }
        octo
}

set_flash_attr <- function(octo) {
        attr(octo, "tot_flash") <- 0
        octo
}

read_input(example = FALSE) |> set_flash_attr() |> step_n(100)

## Puzzle two

step_sync <- function(octo) {
        i <- 0
        while(!all(!octo)) {
                octo <- step(octo)
                i <- i + 1
        }
        i
}

read_input(example = FALSE) |> step_sync()
