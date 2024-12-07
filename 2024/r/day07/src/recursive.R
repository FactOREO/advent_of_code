source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 7, session_cookie)
input <- readLines("../input/input.txt")

## Recursive attempt to reduce runtime
solve_recursive <- function(target, numbers, num_len, current, progression, operands = "+*") {
  # Base Cases
  if (progression == num_len + 1L) return(current == target)
  operand <- numbers[[progression]]
  if (operands == "+*") {
    return(
      solve_recursive(target, numbers, num_len, current + operand, progression + 1L) |
      solve_recursive(target, numbers, num_len, current * operand, progression + 1L)
    )
  } else if (operands == "+*||") {
    return(
      solve_recursive(target, numbers, num_len, current + operand, progression + 1L) |
      solve_recursive(target, numbers, num_len, current * operand, progression + 1L) |
      solve_recursive(target, numbers, num_len, current * 10  ^ (as.integer(log10(operand)) + 1) + operand, progression + 1L)
    )
  }
}

tictoc::tic()
p1 <- sapply(input, \(i) {
  values <- strsplit(i, ": ", fixed = TRUE)[[1L]]
  target <- values[[1L]] |> as.numeric()
  numbers <- strsplit(values[[2L]], " ", fixed = TRUE)[[1L]] |> as.numeric()
  if (solve_recursive(target, numbers, length(numbers), numbers[[1L]], 2L)) target else 0
}) |> sum()
tictoc::toc()

print_result(2024, 7, p1)

tictoc::tic()
p2 <- sapply(input, \(i) {
  values <- strsplit(i, ": ", fixed = TRUE)[[1L]]
  target <- values[[1L]] |> as.numeric()
  numbers <- strsplit(values[[2L]], " ", fixed = TRUE)[[1L]] |> as.numeric()
  if (solve_recursive(target, numbers, length(numbers), numbers[[1L]], 2L, "+*||")) target else 0
}) |> sum()
tictoc::toc()

print_result(2024, 7, p2)

# $ Rscript recursive.R
# Download input for day 7 of AOC 2024 from https://adventofcode.com/2024/day/7/input

# 0.636 sec elapsed
# The result for day 7 of AOC 2024 is: 1260333054159

# 0.928 sec elapsed
# The result for day 7 of AOC 2024 is: 2561281386403
