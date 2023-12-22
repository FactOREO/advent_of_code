input <- readLines("./input.txt")

# Goal: Tilt all the rocks north
# -> tilt column by column
rolling_stones <- function(vec, reverse = FALSE) {
  # print("Process vector:")
  # print(vec)
  if (reverse) vec <- rev(vec)
  while (TRUE) {
    cant_move <- 0L
    # Sequentally check if a round stone can move upwards or not
    for (i in seq_along(vec)) {
      # First row cannot move
      if (i == 1L) {
        cant_move <- cant_move + 1L
      } else if (vec[[i]] == "O" && vec[[i - 1]] == ".") {
        vec[[i - 1L]] <- "O"
        vec[[i]] <- "."
      } else {
        cant_move <- cant_move + 1L
      }
    }
    if (cant_move == length(vec)) break
  }
  if (reverse) vec <- rev(vec)
  # print("Finished vector:")
  # print(vec)
  vec
}

reflector <- matrix(unlist(strsplit(input, "")), byrow = TRUE, ncol = length(input))
reflector <- apply(reflector, 2, rolling_stones)
# Add the load
# -> count round stones per row and multiply by weight, which is equal to the row number + max_rows - 1
weight_of_stones <- function(vec) {
  length(vec[which(vec == "O")])
}
number_of_stones <- apply(reflector, 1, weight_of_stones)
print(sprintf("Part 1: %d", sum(number_of_stones * seq.int(nrow(reflector), 1))))

### PART 2: There might be a cycle present, so the rolls repeat after a certain amount of cycles
# -> function to circle ones
# -> check if a result matrix happened before
# -> if yes, get the cycle length, than the last state w.r.t. to the total cycles to spin and get the
#    result state from there
spin_stones <- function(reflector) {
  result <- reflector
  # north
  result <- apply(result, 2, rolling_stones, reverse = FALSE)
  # west
  result <- do.call(rbind, apply(result, 1, rolling_stones, reverse = FALSE, simplify = FALSE))
  # south
  result <- apply(result, 2, rolling_stones, reverse = TRUE)
  # east
  result <- do.call(rbind, apply(result, 1, rolling_stones, reverse = TRUE, simplify = FALSE))
  result
}
reflector <- matrix(unlist(strsplit(input, "")), byrow = TRUE, ncol = length(input))
# Only check the first 1k rotations to see if a pattern can be detected
reflector_states <- vector("list", length = 1e03)
for (i in seq_along(reflector_states)) {
  reflector_state <- spin_stones(reflector)
  if (any(unlist(lapply(reflector_states, function(x) identical(x, reflector_state))))) {
    first_seen <- which(unlist(lapply(reflector_states, function(x) identical(x, reflector_state))))
    cycle_length <- i - first_seen
    print(sprintf("Detected cycle length of: %d", cycle_length))
    break
  }
  reflector_states[[i]] <- reflector_state
  reflector <- reflector_states[[i]]
}
# With the cycle length, calculate the remainder to find the correct position
# In the example: Spin 3 == Spin 10
remainder <- (1e09 - first_seen) %% 42L
rotation <- first_seen + remainder
print(sprintf("Final rotation: %d", rotation))
number_of_stones <- apply(reflector_states[[rotation]], 1, weight_of_stones)
print(sprintf("Part 2: %d", sum(number_of_stones * seq.int(nrow(reflector_states[[rotation]]), 1))))
