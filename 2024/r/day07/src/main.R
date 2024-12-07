source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
input <- get_aoc(2024, 7, session_cookie)
# input <- readLines("../input/test_input.txt")

#' Generate Operators
#'
#' Count of combinations: length(operators) ^ (length(numbers) - 1)
#'
#' @param len - How many places for the operators are requested
#' @param operators - Character vectors of possible operators
#'
#' @return character matrix of operator combinations
generate_operators <- function(len, operators = c("*", "+", "||")) {
  n_com <- length(operators) ^ (len)
  len_operators <- length(operators)
  out <- matrix(NA_character_, ncol = len, nrow = n_com)
  for (index in 1:n_com) {
      for (i in 0:(len - 1)) {
        op_index <- (index - 1) %/% len_operators^i %% len_operators
        out[index, i + 1] <- operators[op_index + 1]
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
#' @return int Result of the operations
calculate_result <- function(operators, numbers, target_value) {
  multiplications <- length(which(operators == "*"))
  concatenations <- length(which(operators == "||"))
  if (multiplications == 0L && concatenations == 0L) return(sum(numbers))
  if (multiplications == length(operators)) return(prod(numbers))
  if (concatenations == length(operators)) return(as.numeric(paste(numbers, collapse="")))
  res <- numbers[[1L]]
  for (i in seq_along(operators)) {
    if (operators[[i]] == "*") {
      res <- res * numbers[[i + 1L]]
    } else if (operators[[i]] == "+") {
      res <- res + numbers[[i + 1L]]
    } else {
      res <- as.numeric(paste0(res, numbers[[i + 1L]], collapse = ""))
    }
    # Catch to large values and return 0
    if (res > target_value) return(0)
  }
  res
}

p1 <- lapply(input, \(i) {
  values <- strsplit(i, ": ", fixed = TRUE)[[1L]]
  result <- values[[1L]] |> as.numeric()
  numbers <- strsplit(values[[2L]], " ", fixed = TRUE)[[1L]] |> as.numeric()
  # cat(sprintf("Check if %s can be combined to %.0f\n", paste(numbers, collapse = ", "), result))
  operator_combinations <- generate_operators(length(numbers) - 1L, operators = c("*", "+"))
  results <- apply(operator_combinations, calculate_result, MARGIN = 1, numbers = numbers, target_value = result)
  if (any(results == result)) result
}) |> unlist() |> sum()
print_result(2024, 7, p1)
# The result for day 7 of AOC 2024 is: 1260333054159

# This is a brutforce solution which takes a long long time (~ 10 minutes)
# Might be a possibility to check for the first N multiplications / concatenations and check if the resulting
# number is greater than target already
p2 <- mapply(\(i, idx) {
  values <- strsplit(i, ": ", fixed = TRUE)[[1L]]
  result <- values[[1L]] |> as.numeric()
  numbers <- strsplit(values[[2L]], " ", fixed = TRUE)[[1L]] |> as.numeric()
  # cat(sprintf("Check if %s can be combined to %.0f\n", paste(numbers, collapse = ", "), result))
  operator_combinations <- generate_operators(length(numbers) - 1L, operators = c("*", "+", "||"))
  results <- apply(operator_combinations, calculate_result, MARGIN = 1, numbers = numbers, target_value = result)
  if (idx %% 10 == 0) cat(sprintf("Finished iteration %d\n", idx))
  if (any(results == result)) result
}, i = input, idx = seq_along(input)) |> unlist() |> sum()
print_result(2024, 7, p2)
