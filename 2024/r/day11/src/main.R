source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 11, session_cookie)
input <- readLines("../input/input.txt")
arrangement <- strsplit(input, " ")[[1L]]

expand_stones <- function(initial_stones, iterations = 25L) {
  stone_counter <- vector("list")
  for (i in seq_along(initial_stones)) {
    if (initial_stones[[i]] %in% names(stone_counter)) stone_counter[[initial_stones[[i]]]] <- stone_counter[[initial_stones[[i]]]] + 1L else stone_counter[[initial_stones[[i]]]] <- 1L
  }

  i <- 0L
  while (i < iterations) {
    i <- i + 1L
    stones <- names(stone_counter)
    new_counter <- vector("list")
    # Check each stone, since each stone is counter often present, increase the resulting stone counts by count
    for (j in seq_along(stones)) {
      stone <- stones[[j]]
      count <- stone_counter[[stone]]
      if (stone == "0") {
        new_counter[["1"]] <- new_counter[["1"]] %||% 0 + count
      } else if (nchar(stone) %% 2 == 0) {
        stone_digits <- strsplit(stone, "")[[1]]
        stone_length <- nchar(stone)
        stone_left   <- as.integer(paste(stone_digits[1:(stone_length / 2)], collapse = "")) |> as.character()
        stone_right  <- as.integer(paste(stone_digits[(stone_length / 2 + 1):stone_length], collapse = "")) |> as.character()
        new_counter[[stone_left]] <- new_counter[[stone_left]] %||% 0 + count
        new_counter[[stone_right]] <- new_counter[[stone_right]] %||% 0 + count
      } else {
        new_stone <- as.character(as.integer(stone) * 2024)
        new_counter[[new_stone]] <- new_counter[[new_stone]] %||% 0 + count
      }
    }
    stone_counter <- new_counter
  }

  sum(unlist(stone_counter))
}

tictoc::tic()
p1 <- expand_stones(arrangement, 25)
tictoc::toc()
print_result(2024, 11, p1)

tictoc::tic()
p2 <- expand_stones(arrangement, 75)
tictoc::toc()
print_result(2024, 11, p2)

# 0.063 sec elapsed
# The result for day 11 of AOC 2024 is: 233875

# 10.178 sec elapsed
# The result for day 11 of AOC 2024 is: 277444936413293
