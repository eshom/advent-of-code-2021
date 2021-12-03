read_input <- function(filepath = "input.txt", example = TRUE) {
        if (example) {
                tmp <- tempfile()
                on.exit(file.remove(tmp)) # clean up
                cat("00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
", file = tmp)

                readLines(tmp) |> strsplit("") |> simplify2array() |> t() -> out
                storage.mode(out) <- "integer"
                storage.mode(out) <- "logical"
                return(out)
        }

        readLines(filepath) |> strsplit("") |> simplify2array() |> t() -> out
        storage.mode(out) <- "integer"
        storage.mode(out) <- "logical"
        out
}

## Puzzle one
gamma_rate <- function(bit_mat) {
        colSums(bit_mat) > (nrow(bit_mat) / 2)
}

epsilon_rate <- function(bit_mat) {
        colSums(bit_mat) < (nrow(bit_mat) / 2)
}

binvec_to_dec <- function(binvec) {
        binvec |> as.integer() |> paste(collapse = "") |> strtoi(base = 2)
}

diag_report <- read_input(example = FALSE)

diag_report |> gamma_rate() |> binvec_to_dec() -> gamma_r
diag_report |> epsilon_rate() |> binvec_to_dec() -> epsilon_r

gamma_r * epsilon_r

## Puzzle two
oxygen_generator_cond <- function(bit_mat) {
        colSums(bit_mat) >= (nrow(bit_mat) / 2)
}

co2_scrubber_cond <- epsilon_rate # epsilon rate == our condition

rating <- function(bit_mat, type = c("oxygen_generator", "co2_scrubber")) {
        condf <- paste0(type[1], "_cond") # Will select appropriately named function

        for (pos in seq_len(ncol(bit_mat))) {
                bit_mat <- bit_mat[get(condf)(bit_mat)[pos] == bit_mat[, pos], ]

                ## When the result becomes a vector, we arrived to our output
                if (is.vector(bit_mat))
                        break
        }

        bit_mat
}

diag_report |> rating("oxygen_generator") |> binvec_to_dec() -> oxygen_r
diag_report |> rating("co2_scrubber") |> binvec_to_dec() -> co2_r

oxygen_r * co2_r
