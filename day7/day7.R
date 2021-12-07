read_input <- function(eg = TRUE) {
        if (eg)
                return(c(16,1,2,0,4,2,7,1,2,14))

        scan("input.txt", quiet = FALSE, sep = ",")
}

likely_crabs <- function(crabs, keep = 2) {
        crabs |> table() |> sort(decreasing = TRUE) -> possible

        possible[possible >= keep] |> names() |> as.integer()
}

fuel_cost <- function(crabs, chosen_pos) {
        abs(chosen_pos - crabs) |> sum()
}

## Puzzle one
crabs <- read_input(eg = FALSE)

likely <- likely_crabs(crabs, 2)
sapply(likely, fuel_cost, crabs = crabs) |> min()

## Puzzle two
fuel_cost2 <- function(crabs, pos) {
        f <- function(x) sum(x:0)
        abs(crabs - pos) |> sapply(f) |> sum()
}

seq(min(crabs), max(crabs)) |> sapply(fuel_cost2, crabs = crabs) |> min()
