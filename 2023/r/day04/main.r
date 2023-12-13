input <- readLines("./input.txt")

winning <- stringr::str_extract(input, "\\|\\s*(\\d*\\s?)*") |>
  stringr::str_remove_all("(\\|\\s*|\\s*$)") |>
  stringr::str_split("\\s+") |>
  lapply(as.integer)

cards <- stringr::str_extract(input, ":\\s*(\\d*\\s)*") |>
  stringr::str_remove_all("(^\\:\\s*|\\s$)") |>
  stringr::str_split("\\s+") |>
  lapply(as.integer)

# Part 1: Matching numbers double points
result <- 0L
for (i in seq_along(cards)) {
  points <- sum(cards[[i]] %in% winning[[i]])
  if (points > 0) result <- result + 2 ^(points - 1)
}
sprintf("PART 1: %d", result)

# Part 2: Append the pool of cards for every match with the next n cards where n is the number of matches
n_cards <- rep(1, length(input))
for (i in seq_along(n_cards)) {
  if (i == 0) next
  n <- n_cards[[i]]
  wins <- sum(winning[[i]] %in% cards[[i]])
  n_cards[seq_len(wins) + i] <- n_cards[seq_len(wins) + i] + n
}
sprintf("PART 2: %d", sum(n_cards))
