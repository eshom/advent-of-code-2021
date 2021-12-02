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

forward <- function(arg, pos, with_aim = FALSE) {
        if (!with_aim)
                return(pos + c(arg, 0))

        pos + c(arg, pos[3] * arg, 0)
}

down <- function(arg, pos, with_aim = FALSE) {
        if (!with_aim)
                return(pos + c(0, arg))

        pos + c(0, 0, arg)
}

up <- function(arg, pos, with_aim = FALSE) {
        if (!with_aim)
                return(pos + c(0, -arg))

        pos + c(0, 0, -arg)
}

## Works for both versions of `pos` without branching!
run_command <- function(command, pos, with_aim = FALSE) {
        comm <- command$command
        arg <- command$arg

        switch(comm,
               forward = forward(arg, pos, with_aim),
               down = down(arg, pos, with_aim),
               up = up(arg, pos, with_aim))
}

## Also no branching here
start_course <- function(all_commands, with_aim = FALSE) {
        ncommands <- nrow(all_commands)

        pos_n_elements <- with_aim + 2 # results in 2 if FALSE or 3 if TRUE

        pos_out <- matrix(0, ncol = pos_n_elements, nrow = ncommands)

        pos_out[1, ] <- run_command(all_commands[1, ], vector("numeric", length = pos_n_elements), with_aim)
        for (i in seq_len(ncommands)[-1]) {
                pos_out[i, ] <- run_command(all_commands[i, ], pos_out[i - 1, ], with_aim)
        }

        pos_out
}

read_input() |> start_course() |> tail(1) |> prod()
read_input(example = FALSE) |> start_course() |> tail(1) |> prod()

## Puzzle two
read_input() |> start_course(with_aim = TRUE) |> tail(1) -> result
prod(result[-3])

read_input(example = FALSE) |> start_course(with_aim = TRUE) |> tail(1) -> result
prod(result[-3])
