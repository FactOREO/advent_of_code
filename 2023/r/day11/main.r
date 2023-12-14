args <- commandArgs(trailingOnly = TRUE)
# PART 1: Duplicate
# PART 2: Replicate one million times -> there is a perfect linear dependency between the number of replications and the sum of minimal distances
get_sum_of_minimal_distances <- function(data, reps) {
  # Expand all empty rows / columns
  universe <- do.call(rbind, lapply(data, strsplit, split = "") |> lapply(`[[`, 1))
  universe <- t(do.call(rbind, apply(universe, 1, function(x) if (all(x == ".")) t(replicate(reps, x)) else x, simplify = TRUE)))
  universe <- t(do.call(rbind, apply(universe, 1, function(x) if (all(x == ".")) t(replicate(reps, x)) else x, simplify = TRUE)))
  # Get all universe by position
  galaxies <- vector("list")
  for (i in seq.int(1, nrow(universe))) {
    for (j in seq.int(1, ncol(universe))) {
      if (universe[[i, j]] == "#") galaxies <- c(galaxies, list(paste0(i, "|", j)))
    }
  }
  # Create pairs
  galaxies <- do.call(rbind, galaxies)
  galaxies <- combn(galaxies, 2) |> t() |> as.data.frame()
  # Refactor the pasted coordina tes to actual integers
  galaxies[c("x1", "y1")] <- do.call(rbind, strsplit(galaxies[, 1], split = "\\|"))
  galaxies[c("x2", "y2")] <- do.call(rbind, strsplit(galaxies[, 2], split = "\\|"))
  galaxies <- galaxies[which(galaxies[, 1] != galaxies[, 2]), ]
  galaxies[c("x1", "x2", "y1", "y2")] <- lapply(galaxies[, c("x1", "x2", "y1", "y2")], as.integer)
  # Calculate the manhatten distance for every pair
  galaxies[["distance"]] <- abs(galaxies[["x1"]] - galaxies[["x2"]]) + abs(galaxies[["y1"]] - galaxies[["y2"]])
  print(sprintf("The sum of minimal distances is equal to %d", sum(galaxies[["distance"]])))
  c(reps, sum(galaxies[["distance"]]))
}

data <- scan("./input.txt", what = "list", sep = "\n")
if (args[[1]] == "part1") {
  get_sum_of_minimal_distances("./input.txt", 2)
} else if (args[[1]] == "part2") {
  # Find the linear dependency
  reps <- vector("integer", 0L)
  dists <- vector("integer", 0L)
  for (n in c(2, 5, 10)) {
    res <- get_sum_of_minimal_distances(data, n)
    reps <- c(reps, res[[1]])
    dists <- c(dists, res[[2]])
  }
  reg <- lm(dists ~ reps)[["coefficients"]]
  print(sprintf("The sum of minimal distances with 1,000,000 replications is equal to %s", reg[[1]] + 1e6 * reg[[2]]))
}
