source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 7, session_cookie)
input <- readLines("../input/input.txt")

#' Generate Operators
#'
#' Get all possible combinations for an arbitrary number of operators and combinations
#' of those.
#'
#' @param len - How many places for the operators are requested
#' @param operators - Character vectors of possible operators
#'
#' @return character matrix of operator combinations
generate_operators <- function(len, operators = c("*", "+", "||")) {
  n_com <- length(operators) ^ (len)
  len_operators <- length(operators)
  out <- matrix(NA_character_, ncol = len, nrow = n_com)
  for (index in seq.int(1L, n_com)) {
      for (i in seq.int(0L, (len - 1L))) {
        op_index <- (index - 1L) %/% len_operators ^ i %% len_operators
        out[index, i + 1L] <- operators[op_index + 1L]
      }
  }
  out
}

#' Calculate result
#'
#' Operators are evaluated left to right, not according to the precendence rules.
#'
#' @param operators The operators to use
#' @param numbers The numbers to apply operation onto
#' @param target_value The target value to reach as boundary
#'
#' @return logical TRUE / FALSE if the result is equal to the target_value
target_value_is_reachable_from_operators <- function(operators, numbers, target_value) {
  summations      <- length(which(operators == "+"))
  multiplications <- length(which(operators == "*"))
  concatenations  <- length(which(operators == "||"))
  if (summations == length(operators) && sum(numbers) == target_value) {
    return(TRUE)
  } else if (multiplications == length(operators) && prod(numbers) == target_value) {
    return(TRUE)
  } else if (concatenations == length(operators) && as.numeric(paste0(numbers, collapse = "")) == target_value) {
    return(TRUE)
  } else if (multiplications == length(operators) || concatenations == length(operators) || summations == length(operators)) {
    return(FALSE)
  }
  res <- numbers[[1L]]
  for (i in seq_along(operators)) {
    if (operators[[i]] == "*") {
      res <- res * numbers[[i + 1L]]
    } else if (operators[[i]] == "+") {
      res <- res + numbers[[i + 1L]]
    } else {
      res <- as.numeric(paste0(res, numbers[[i + 1L]], collapse = ""))
    }
    # Catch to large values and return FALSE
    if (res > target_value) return(FALSE)
  }
  if (res == target_value) return(TRUE)
  FALSE
}

#' Check Operator Combinations
#'
#' Go through all operator combinations and stop if the target value was achieved once -> don't compute all result values
#'
#' @param operator_combinations Matrix of operator combinations (character values)
#' @param numbers The numbers to operate on
#' @param target_value The target value to achieve via operations on the numbers
#'
#' @param Returns the target_value if achievable, else 0
check_operator_combinations <- function(operator_combinations, numbers, target_value) {
  for (i in seq.int(1L, nrow(operator_combinations))) {
    if (target_value_is_reachable_from_operators(operator_combinations[i, ], numbers, target_value)) return(target_value)
  }
  0L
}

tictoc::tic()
p1 <- lapply(input, \(i) {
  values <- strsplit(i, ": ", fixed = TRUE)[[1L]]
  result <- values[[1L]] |> as.numeric()
  numbers <- strsplit(values[[2L]], " ", fixed = TRUE)[[1L]] |> as.numeric()
  operator_combinations <- generate_operators(length(numbers) - 1L, operators = c("*", "+"))
  check_operator_combinations(operator_combinations, numbers, result)
}) |> unlist() |> sum()
tictoc::toc()

print_result(2024, 7, p1)
# 4.033 sec elapsed
# The result for day 7 of AOC 2024 is: 1260333054159

# This is a brutforce solution which takes a long long time (~ 10 minutes)
# Might be a possibility to check for the first N multiplications / concatenations and check if the resulting
# number is greater than target already
tictoc::tic()
p2 <- mapply(\(i, idx) {
  values <- strsplit(i, ": ", fixed = TRUE)[[1L]]
  result <- values[[1L]] |> as.numeric()
  numbers <- strsplit(values[[2L]], " ", fixed = TRUE)[[1L]] |> as.numeric()
  # cat(sprintf("Check if %s can be combined to %.0f\n", paste(numbers, collapse = ", "), result))
  operator_combinations <- generate_operators(length(numbers) - 1L, operators = c("*", "+", "||"))
  results <- apply(operator_combinations, calculate_result, MARGIN = 1, numbers = numbers, target_value = result)
  if (idx %% 10 == 0) cat(sprintf("Finished test case %d\n", idx))
  if (any(results == result)) result
}, i = input, idx = seq_along(input)) |> unlist() |> sum()
tictoc::toc()

print_result(2024, 7, p2)
# 562.236 sec elapsed
# The result for day 7 of AOC 2024 is: 162042343638683
