read_input <- function(filepath) {
        readLines(filepath)
}

## Puzzle one

to_line_list <- function(raw_inp) {
        strsplit(raw_inp, "")
}

encode_brackets <- function(inp) {
        collapse::recode_char(inp, "(" = 1, "[" = 2, "{" = 3, "<" = 4,
                              ")" = -1, "]" = -2, "}" = -3, ">" = -4) |>
                lapply(as.integer)
}

close_brackets <- function(line) {
        ## Keep closing until done or problem closing
        while(TRUE) {
                if (all(line > 0))
                        return(0) # incomplete but okay

                ic <- which(line < 0)[1]
                io <- ic - 1

                if (line[ic] + line[io] != 0)
                        return(line[ic]) # return illegal character

                line <- line[c(-io, -ic)]
        }
}

illegal_points <- function(char_vec) {
        collapse::recode_num(char_vec, `-1` = 3, `-2` = 57, `-3` = 1197,
                             `-4` = 25137)
}

read_input("input.txt") |> to_line_list() |> encode_brackets() |>
        sapply(close_brackets) |> illegal_points() |> sum()

## Puzzle two
legal_line <- function(line) {
        while (TRUE) {
                if (all(line > 0))
                        return(TRUE)

                ic <- which(line < 0)[1]
                io <- ic - 1

                if (line[ic] + line[io] != 0)
                        return(FALSE)

                line <- line[c(-io, -ic)]
        }
}

legal_close <- function(line) {
        while (TRUE) {
                if (all(line > 0))
                        break

                ic <- which(line < 0)[1]
                io <- ic - 1

                line <- line[c(-io, -ic)]
        }
        rev(line)
}

read_input("input.txt") |> to_line_list() |> encode_brackets() |>
        Filter(f = legal_line) |> lapply(legal_close) |>
        sapply(\(x) Reduce(\(a, b) a * 5 + b, x, 0)) |> median()
