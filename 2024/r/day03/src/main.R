source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 3, session_cookie)
input <- readLines("../input/input.txt")

pattern <- "mul\\([0-9]*,[0-9]*\\)"
tictoc::tic()
p1 <- stringr::str_extract_all(input, pattern) |>
  unlist() |>
  stringr::str_replace_all("mul|\\(|\\)", "") |>
  sapply(\(i) strsplit(i, ",") |> lapply(as.integer) |> unlist() |> prod()) |>
  sum()
tictoc::toc()

print_result(2024, 3, p1)
# 0.009 sec elapsed
# The result for day 3 of AOC 2024 is: 179834255

pattern <- "(don't\\(\\)|do\\(\\)|mul\\([0-9]*,[0-9]*\\))"
instructions <- stringr::str_extract_all(input, pattern) |>
  unlist() |>
  stringr::str_replace_all("mul|\\(|\\)", "")

p2 <- numeric(length=1)
do <- TRUE
tictoc::tic()
for (i in seq_along(instructions)) {
  if (instructions[[i]] == "don't") {
    do <- FALSE
    next
  } else if (instructions[[i]] == "do") {
    do <- TRUE
    next
  }
  if (do)
    p2 = p2 + strsplit(instructions[[i]], ",") |> lapply(as.integer) |> unlist() |> prod()
}
tictoc::toc()

print_result(2024, 3, p2)
# 0.011 sec elapsed
# The result for day 3 of AOC 2024 is: 80570939
