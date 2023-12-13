# Read the input
input <- readLines("./input_test.txt")
# Get the seeds
seeds <- sub("^.*:", "", input[[1]]) |> strsplit(" ") |> sapply(as.numeric)
seeds <- seeds[!is.na(seeds)]
seeds <- matrix(seeds, ncol = 2, byrow = TRUE)
seeds[, 2] <- seeds[, 1] + seeds[, 2]
# Parse all numbers to get the maps
ctr <- 0L
maps <- vector("list", length = 7)
for (line in input[grepl("^\\d|^$", input)]) {
  if (line == "") {
    ctr <- ctr + 1L
  } else {
    nums <- strsplit(line, " ") |> sapply(as.numeric)
    maps[[ctr]] <- rbind(maps[[ctr]], nums[!is.na(nums)])
  }
}
rm(list = c("ctr", "input"))

# Target: Function which takes the maps and a single seed range and returns the minimum location values
get_locations <- function(seed_range, maps, ctr = 1L) {
  # Find intersection
  # Pass the found intersection into a remapping function
  # Pass the remapped result together with a map counter into the location function again
  # All non intersecting parts should be put into the next map section
  # If there was no intersection until the last map part, pass the ranges to the locations function with map counter
  # If map counter is greater than maps length, return the minimum value
  if (ctr > length(maps)) return(seed_range[[1]])
  n_parts <- nrow(maps[[ctr]])
  src_start <- maps[[ctr]][[2]]
  src_end <- maps[[ctr]][[2]] + maps[[ctr]][[3]]
  for (i in seq.default(1, n_parts)) {
    # There is an intersection
    if (seed_range[[2]] >= src_start && src_end >= seed_range[[1]]) {
      out <- get_locations(c(max(seed_range[[1]], src_start), min(seed_range[[2]], src_end)), maps, ctr + 1L)
      print(out)
    }
    if (i == n_parts) {
      out <- get_locations(c(max(seed_range[[1]], src_start), min(seed_range[[2]], src_end)), maps, ctr + 1L)
      print(out)
    }
  }
  out
}
