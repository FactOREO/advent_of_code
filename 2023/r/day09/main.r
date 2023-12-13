library("data.table")
input <- fread("./input.txt", header = FALSE, sep = "|", colClasses = "character")

# Function which takes a string of numbers and calculates the next number in the sequence
calculate_number <- Vectorize(function(num_string, extrapolate_forward = TRUE) {
  nums <- strsplit(num_string, " ")[[1L]] |> as.integer()
  num_list <- vector("list", length = length(nums))
  num_list[[1L]] <- nums
  num_sum <- sum(num_list[[1L]])
  cntr <- 2L
  # Get the pyramid down
  while (!all(num_list[[cntr - 1L]] == 0L)) {
    num_list[[cntr]] <- diff(num_list[[cntr - 1L]])
    cntr <- cntr + 1
  }
  # Remove NULL list elements
  num_list <- num_list[!lapply(num_list, is.null) |> unlist()]
  if (extrapolate_forward) {
    # Move up to caculate the next number
    for (i in seq.int(length(num_list), 2L)) {
      num_list[[i - 1L]] <- tail(num_list[[i]], 1L) + tail(num_list[[i - 1L]], 1L)
    }
    tail(num_list[[1L]], 1L)
  } else {
    # Move up to caculate the previous number
    for (i in seq.int(length(num_list), 2L)) {
      num_list[[i - 1L]] <- head(num_list[[i - 1L]], 1L) - head(num_list[[i]], 1L)
    }
    head(num_list[[1L]], 1L)
  }
}, vectorize.args = c("num_string"))

input[, c("next_number", "previous_number") := .(calculate_number(V1), calculate_number(V1, FALSE))]
print(sprintf("Part 1: %s",
              input[, .(result = sum(next_number))]$result
))
print(sprintf("Part 2: %s",
              input[, .(result = sum(previous_number))]$result
))
