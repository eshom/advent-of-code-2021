read_input <- function(filepath = "input.txt", eg = TRUE) {
        if (!eg)
                return(scan("input.txt", sep = ",", quiet = TRUE))

        c(3, 4, 3, 1, 2)
}

## Puzzle one

spawn_table <- data.frame(day = 0:8, fish = 0)

fish_tracker <- function(init) {
        fish <- rep(0, 9)
        fish <- setNames(fish, paste0("day", 0:8))

        tab <- table(init)

        fish[as.integer(names(tab)) + 1] <- tab

        fish
}

advance_day <- function(fish) {
        labs <- names(fish)

        ## Spawn fish
        day6 <- fish["day7"] + fish["day0"]
        day8 <- fish["day0"]

        ## Next day
        fish <- append(fish[-1], 0)

        ## Fix day labels
        fish <- setNames(fish, labs)

        ## Add new fish
        fish["day6"] <- day6
        fish["day8"] <- day8

        fish
}

sim <- function(init, days) {
        fish <- fish_tracker(init)

        for(i in seq_len(days)) {
                fish <- advance_day(fish)
        }

        sum(fish)
}

read_input(eg = FALSE) |> sim(80)

## Puzzle two
read_input(eg = FALSE) |> sim(256)
