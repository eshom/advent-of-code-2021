read_input <- function(filepath = "input-eg.txt") {
        readLines(filepath) |> strsplit("|", fixed = TRUE) -> out

        lapply(out, trimws) |> lapply(strsplit, split = "\\s") |>
                lapply(setNames, c("pattern", "output"))
}

## Puzzle one
count_segments <- function(inp) {
        rapply(inp, nchar, how = "replace")
}

g_unique_seg_count = c("1" = 2, "4" = 4, "7" = 3, "8" = 7)

library(collapse) # For easy list processing

read_input("input.txt") |> count_segments() |> get_elem("output") |>
        sapply(\(x) sum(x %in% g_unique_seg_count)) |> sum()

## Puzzle two
get_pattern_count <- function(entry) {
        patterns <- get_elem(entry, "pattern")
        count <- nchar(patterns)

        list(patterns = patterns, count = count)
}

seg_difference <- function(more_segs, less_segs) {
        setdiff(strsplit(more_segs, "")[[1]], strsplit(less_segs, "")[[1]]) |> paste(collapse = "")
}

seg_split <- function(segs) {
        strsplit(segs, "")
}

seg_common <- function(lhs, rhs) {
        lhs <- seg_split(lhs)
        rhs <- seg_split(rhs)

        Reduce(intersect, append(lhs, rhs))
}

top_seg <- function(entry) {
        l <- get_pattern_count(entry)

        d1 <- l$patterns[l$count == 2]
        d7 <- l$patterns[l$count == 3]

        seg_difference(d7, d1)
}

bottom_seg <- function(entry) {
        l <- get_pattern_count(entry)

        d8 <- l$patterns[l$count == 7]
        d4 <- l$patterns[l$count == 4]
        d069 <- l$patterns[l$count == 6]

        segs <- seg_difference(d8, d4)

        common <- seg_common(segs, d069)

        common[!(common %in% top_seg(entry))]
}

bottomleft_seg <- function(entry) {
        l <- get_pattern_count(entry)

        d8 <- l$patterns[l$count == 7]
        d4 <- l$patterns[l$count == 4]
        top <- top_seg(entry)
        bottom <- bottom_seg(entry)

        segs <- seg_difference(d8, d4)
        segs <- seg_difference(segs, top)
        seg_difference(segs, bottom)
}

middle_seg <- function(entry) {
        l <- get_pattern_count(entry)

        d8 <- l$patterns[l$count == 7]
        d235 <- l$patterns[l$count == 5]
        top <- top_seg(entry)
        bottom <- bottom_seg(entry)

        segs <- seg_common(d8, d235) |> paste(collapse = "")
        segs <- seg_difference(segs, top)
        seg_difference(segs, bottom)
}

topleft_seg <- function(entry) {
        l <- get_pattern_count(entry)

        d8 <- l$patterns[l$count == 7]
        d1 <- l$patterns[l$count == 2]
        d069 <- l$patterns[l$count == 6]

        segs <- seg_common(d8, d069) |> paste(collapse = "")
        segs <- seg_difference(segs, d1)
        segs <- seg_difference(segs, top_seg(entry))
        seg_difference(segs, bottom_seg(entry))
}

bottomright_seg <- function(entry) {
        l <- get_pattern_count(entry)

        d8 <- l$patterns[l$count == 7]
        d069 <- l$patterns[l$count == 6]

        segs <- seg_common(d8, d069) |> paste(collapse = "")
        segs <- seg_difference(segs, top_seg(entry))
        segs <- seg_difference(segs, bottom_seg(entry))
        seg_difference(segs, topleft_seg(entry))

}

topright_seg <- function(entry) {
        l <- get_pattern_count(entry)

        d1 <- l$patterns[l$count == 2]

        seg_difference(d1, bottomright_seg(entry))
}

top <- function(inp) lapply(inp, top_seg)
bottom <- function(inp) lapply(inp, bottom_seg)
bleft <- function(inp) lapply(inp, bottomleft_seg)
mid <- function(inp) lapply(inp, middle_seg)
tleft <- function(inp) lapply(inp, topleft_seg)
bright <- function(inp) lapply(inp, bottomright_seg)
tright <- function(inp )lapply(inp, topright_seg)

construct_digit <- function(...) {
        Map(c, ...)
}

inp <- read_input("input.txt")

zero <- construct_digit(top(inp), tleft(inp), tright(inp), bleft(inp), bright(inp), bottom(inp))
one <- construct_digit(tright(inp), bright(inp))
two <- construct_digit(top(inp), tright(inp), mid(inp), bleft(inp), bottom(inp))
three <- construct_digit(top(inp), tright(inp), mid(inp), bright(inp), bottom(inp))
four <- construct_digit(tleft(inp), tright(inp), mid(inp), bright(inp))
five <- construct_digit(top(inp), tleft(inp), mid(inp), bright(inp), bottom(inp))
six <- construct_digit(top(inp), tleft(inp), mid(inp), bleft(inp), bright(inp), bottom(inp))
seven <- construct_digit(top(inp), tright(inp), bright(inp))
eight <- construct_digit(top(inp), tleft(inp), tright(inp), mid(inp), bleft(inp), bright(inp), bottom(inp))
nine <- construct_digit(top(inp), tleft(inp), tright(inp), mid(inp), bright(inp), bottom(inp))

find_digit <- function(digit, output_entry) {
        sapply(output_entry, \(x) identical(sort(x), sort(digit)))
}

output_digits <- lapply(get_elem(inp, "output"), seg_split)

find_digit_matrix <- function(digit, num, output) {
        mapply(find_digit, digit, output) |> t() |> ifelse(num, NA) |> as.integer() |> matrix(ncol = 4)
}

m0 <- find_digit_matrix(zero, 0, output_digits)
m1 <- find_digit_matrix(one, 1, output_digits)
m2 <- find_digit_matrix(two, 2, output_digits)
m3 <- find_digit_matrix(three, 3, output_digits)
m4 <- find_digit_matrix(four, 4, output_digits)
m5 <- find_digit_matrix(five, 5, output_digits)
m6 <- find_digit_matrix(six, 6, output_digits)
m7 <- find_digit_matrix(seven, 7, output_digits)
m8 <- find_digit_matrix(eight, 8, output_digits)
m9 <- find_digit_matrix(nine, 9, output_digits)

result <- data.table::fcoalesce(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9)

apply(result, 1, \(x) paste(x, collapse = "") |> as.integer()) |> sum()
