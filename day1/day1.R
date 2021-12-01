read_input <- function(filename = "input.txt", example = TRUE) {
        if (!example)
                return(scan(filename))

        scan(text = "
199
200
208
210
200
207
240
269
260
263")

}

## Puzzle one

depth_increasing <- function(depths) {
        c(NA, diff(depths) > 0)
}

read_input() |> depth_increasing() |> sum(na.rm = TRUE)
read_input(example = FALSE) |> depth_increasing() |> sum(na.rm = TRUE)

## Puzzle two
read_input() |> zoo::rollapply(3, sum) |> depth_increasing() |> sum(na.rm = TRUE)
read_input(example = FALSE) |> zoo::rollapply(3, sum) |> depth_increasing() |> sum(na.rm = TRUE)
