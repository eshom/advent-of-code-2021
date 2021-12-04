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

## Returns the indice of the boards that won. `numeric(0)` = zero won
bingo_winner <- function(game, turn) {
        state <- bingo_turn_state(game, turn)
        is_winner <- sapply(state, \(x) {
                any(rowSums(x) == 5 | colSums(x) == 5)
        })

        if (any(is_winner))
                return(which(is_winner))

        numeric(0)
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

## Vectorised version of `bingo_winner()`
bingo_winners <- function(game) {
        lapply(seq_along(game$draws), bingo_winner, game = game)
}

bingo_winners_length <- function(game) {
        sapply(bingo_winners(game), length)
}

first_winner_turn <- function(game) {
        winners <- bingo_winners_length(bingo)
        which(winners == 1) |> min()
}

bingo <- gen_bingo_game(example = FALSE)

win_turn <- first_winner_turn(bingo)
win_board <- bingo_winner(bingo, win_turn)

bingo_unmarked_sum(bingo, win_board, win_turn) * bingo_winning_number(bingo, win_board, win_turn)

## Puzzle two
last_winner_turn_board <- function(game) {
        winners <- bingo_winners(bingo)
        winners_len <- sapply(winners, length)

        which(winners_len == length(game$boards)) |> min() -> win_turn
        setdiff(winners[[win_turn]], winners[[win_turn - 1]]) -> win_board

        c(turn = win_turn, board = win_board)
}

last_win <- last_winner_turn_board(bingo)

bingo_unmarked_sum(bingo, last_win["board"], last_win["turn"]) * bingo_winning_number(bingo, last_win["board"], last_win["turn"])
