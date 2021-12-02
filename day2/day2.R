read_input <- function(filepath = "input.txt", example = TRUE, cols = c("command", "arg")) {
        if (example)
                return(read.table(text = "forward 5
down 5
forward 8
up 3
down 8
forward 2", col.names = cols))

        read.table(filepath, col.names = cols)
}

## Puzzle one

## position object: a vector of length 2 with horizontal / depth elements
position <- c(horizontal = 0, depth = 0)

forward <- function(arg, pos) {
        pos + c(arg, 0)
}

down <- function(arg, pos) {
        pos + c(0, arg)
}

up <- function(arg, pos) {
        pos + c(0, -arg)
}

## command object, a data.frame/list with 1 row and 2 columns: command / arg elements
run_command <- function(command, pos) {
        comm <- command$command
        arg <- command$arg
        switch(comm,
               forward = forward(arg, pos),
               down = down(arg, pos),
               up = up(arg, pos))
}

start_course <- function(all_commands) {
        ncommands <- nrow(all_commands)

        pos_out <- matrix(0, ncol = 2, nrow = ncommands)

        pos_out[1, ] <- run_command(all_commands[1, ], c(0, 0))
        for (i in seq_len(ncommands)[-1]) {
                pos_out[i, ] <- run_command(all_commands[i, ], pos_out[i - 1, ])
        }

        pos_out
}

read_input() |> start_course() |> tail(1) |> prod()
read_input(example = FALSE) |> start_course() |> tail(1) |> prod()
