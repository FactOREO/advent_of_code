shapes <- list(
  "X" = 1L, # X == Rock
  "Y" = 2L, # Y == Paper
  "Z" = 3L  # Z == Scissors
)

results <- list(
  "W" = 6L,
  "D" = 3L,
  "L" = 0L,
  # Add encoding for part2
  "X" = 0L,
  "Y" = 3L,
  "Z" = 6L
)

get_match_result <- function(opponent, associate) {
  switch(
    opponent,
    "A" = switch(associate, "X" = "D", "Y" = "W", "L"),
    "B" = switch(associate, "X" = "L", "Y" = "D", "W"),
    "C" = switch(associate, "X" = "W", "Y" = "L", "D")
  )
}

get_correct_shape <- function(opponent, desired_result) {
  switch(
    desired_result,
    "X" = switch(opponent, "A" = "Z", "B" = "X", "Y"),
    "Y" = switch(opponent, "A" = "X", "B" = "Y", "Z"),
    "Z" = switch(opponent, "A" = "Y", "B" = "Z", "X"),
  )
}

input <- readLines("./input.txt")

part1 <- input |>
  sapply(\(x) {
    choices <- strsplit(x, " ")[[1L]]
    result <- get_match_result(choices[[1L]], choices[[2L]])
    shapes[[choices[[2L]]]] + results[[result]]
  }) |>
  sum()

part2 <- input |>
  sapply(\(x) {
    choices <- strsplit(x, " ")[[1L]]
    shape <- get_correct_shape(choices[[1L]], choices[[2L]])
    results[[choices[[2L]]]] + shapes[[shape]]
  }) |>
  sum()

print(sprintf("Part 1: %d | Part 2: %d", part1, part2))
