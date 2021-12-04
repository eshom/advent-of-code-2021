## Processing input

read_draws <- function(filepath = c("input.txt", "example.txt"), example = TRUE) {
        if (example)
                filepath <- filepath[2]

        filepath <- filepath[1]

        scan(filepath, sep = ",", nlines = 2, quiet = TRUE)
}

read_boards <- function(filepath = c("input.txt", "example.txt"), example = TRUE) {
        if (example)
                filepath <- filepath[2]

        filepath <- filepath[1]

        out <- scan(filepath, skip = 2, sep = "\n", what = list("", "", "", "", ""),
                    multi.line = TRUE, quiet = TRUE)

        ## Well, it works
        out |> as.data.frame() |> t() |> unname() |> as.data.frame() |>
                lapply(\(x) scan(text = paste(x, collapse = " "), quiet = TRUE)) |>
                unname() |> lapply(matrix, nrow = 5, ncol = 5, byrow = TRUE)
}

markable_boards <- function(board_list) {
        lapply(board_list, \(x) matrix(0, nrow = nrow(x), ncol = ncol(x)))
}

gen_bingo_game <- function(example = TRUE) {
        out <- list(draws = read_draws(example = example), boards = read_boards(example = example))
        out$marks <- markable_boards(out$boards)
        out
}

## Puzzle one
bingo_match_draws <- function(game) {
        Map(match, list(game$draws), game$boards, nomatch = 0)
}

bingo_mark <- function(game) {
        matches <- bingo_match_draws(game)

        do_mark <- function(board, m) {
                turn <- seq_along(m)
                no_match <- which(m == 0)
                board[m[-no_match]] <- turn[-no_match]
                board
        }

        Map(do_mark, game$marks, matches)
}

bingo_turn_state <- function(game, turn) {
        lapply(bingo_mark(game), \(x) (x - turn) <= 0)
}

## Returns the number of the board that one, otherwise NULL
bingo_winner <- function(game, turn) {
        state <- bingo_turn_state(game, turn)
        is_winner <- sapply(state, \(x) {
                any(rowSums(x) == 5 | colSums(x) == 5)
        })

        if (any(is_winner))
                return(which(is_winner))

        NULL
}

bingo_winning_number <- function(game, winner_num, turn) {
        board <- game$boards[[winner_num]]
        matches <- bingo_match_draws(game)[[winner_num]]

        board[matches[turn]]
}

bingo_unmarked_sum <- function(game, winner_num, turn) {
        board <- game$boards[[winner_num]]
        marked <- bingo_turn_state(game, turn)[[winner_num]]

        sum(board[!marked])
}

bingo_unmarked_sum(bingo, 3, 12)

bingo_winning_number(bingo, 3, 12)
