source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 8, session_cookie)
input <- readLines("../input/input.txt")

map <- do.call(rbind, strsplit(input, ""))

# Find all pairwise equal antennas and calculate the distances
# -> regular distance if on the same column / row
# -> triangular distance (e.g. keep row and col movement) if not
unique_symbols <- unique(as.vector(map))[!unique(as.vector(map)) == "."]

get_antinodes <- function(i1, i2, antennas, nrow_map, ncol_map, include_all = FALSE) {
  a1 <- antennas[i1, ]
  a2 <- antennas[i2, ]
  row_dist <- abs(a1[[1L]] - a2[[1L]])
  col_dist <- abs(a1[[2L]] - a2[[2L]])
  if (include_all) {
    if ((a1[[1L]] <= a2[[1L]] && a1[[2L]] <= a2[[2L]]) || (a2[[1L]] <= a1[[1L]] && a2[[2L]] <= a1[[2L]])) {
      # One of the antennas is to the upper left of the other
      upper_antinode_rows <- seq.int(min(a1[[1L]], a2[[1L]]), 1L, -row_dist)
      upper_antinode_cols <- seq.int(min(a1[[2L]], a2[[2L]]), 1L, -col_dist)
      lower_antinode_rows <- seq.int(max(a1[[1L]], a2[[1L]]), nrow_map, row_dist)
      lower_antinode_cols <- seq.int(max(a1[[2L]], a2[[2L]]), ncol_map, col_dist)
    } else {
      upper_antinode_rows <- seq.int(min(a1[[1L]], a2[[1L]]), 1L, -row_dist)
      upper_antinode_cols <- seq.int(max(a1[[2L]], a2[[2L]]), ncol_map, col_dist)
      lower_antinode_rows <- seq.int(max(a1[[1L]], a2[[1L]]), nrow_map, row_dist)
      lower_antinode_cols <- seq.int(min(a1[[2L]], a2[[2L]]), 1L, -col_dist)
    }
    # How many antinodes are there at max per direction?
    upper_antinode_count <- min(length(upper_antinode_rows), length(upper_antinode_cols))
    lower_antinode_count <- min(length(lower_antinode_rows), length(lower_antinode_cols))
    # Sometimes there are no feasible antinodes in upper or lower direction, we need to account for that
    matrix(
      c(
        upper_antinode_rows[seq.int(1L, upper_antinode_count, 1L)],
        lower_antinode_rows[seq.int(1L, lower_antinode_count, 1L)],
        upper_antinode_cols[seq.int(1L, upper_antinode_count, 1L)],
        lower_antinode_cols[seq.int(1L, lower_antinode_count, 1L)]
      ),
      ncol = 2
    )
  } else {
    if ((a1[[1L]] <= a2[[1L]] && a1[[2L]] <= a2[[2L]]) || (a2[[1L]] <= a1[[1L]] && a2[[2L]] <= a1[[2L]])) {
      # One of the antennas is to the upper left of the other
      upper_antinode <- c(min(a1[[1L]], a2[[1L]]) - row_dist, min(a1[[2L]], a2[[2L]]) - col_dist)
      lower_antinode <- c(max(a1[[1L]], a2[[1L]]) + row_dist, max(a1[[2L]], a2[[2L]]) + col_dist)
    } else {
      # One of the antennas is to the upper right of the other
      upper_antinode <- c(min(a1[[1L]], a2[[1L]]) - row_dist, max(a1[[2L]], a2[[2L]]) + col_dist)
      lower_antinode <- c(max(a1[[1L]], a2[[1L]]) + row_dist, min(a1[[2L]], a2[[2L]]) - col_dist)
    }
    matrix(c(lower_antinode, upper_antinode), ncol = 2, byrow = TRUE)
  }
}

tictoc::tic()
antinodes <- do.call(rbind, sapply(unique_symbols, \(s) {
  antennas <- which(map == s, arr.ind=TRUE)
  combinations <- combn(seq.int(1L, nrow(antennas)), 2) |> t()
  # Find the antinodes for every non-redundant combination
  do.call(rbind, mapply(
    get_antinodes,
    i1 = combinations[, 1], i2 = combinations[, 2],
    MoreArgs = list(
      antennas = antennas,
      nrow_map = nrow(map),
      ncol_map = ncol(map),
      include_all = FALSE
      ),
    SIMPLIFY = FALSE
  ))
}))

p1 <- nrow(antinodes[!(antinodes[, 1] < 1L | antinodes[, 1] > nrow(map) | antinodes[, 2] < 1 | antinodes[, 2] > ncol(map)), ] |> unique())
tictoc::toc()

print_result(2024, 8, p1)

tictoc::tic()
antinodes <- do.call(rbind, sapply(unique_symbols, \(s) {
  antennas <- which(map == s, arr.ind=TRUE)
  combinations <- combn(seq.int(1L, nrow(antennas)), 2) |> t()
  # Find the antinodes for every non-redundant combination
  do.call(rbind, mapply(
    get_antinodes,
    i1 = combinations[, 1], i2 = combinations[, 2],
    MoreArgs = list(
      antennas = antennas,
      nrow_map = nrow(map),
      ncol_map = ncol(map),
      include_all = TRUE
      ),
    SIMPLIFY = FALSE
  ))
}))

p2 <- nrow(antinodes[!(antinodes[, 1] < 1L | antinodes[, 1] > nrow(map) | antinodes[, 2] < 1 | antinodes[, 2] > ncol(map)), ] |> unique())
tictoc::toc()

print_result(2024, 8, p2)

# 0.047 sec elapsed
# The result for day 8 of AOC 2024 is: 244

# 0.016 sec elapsed
# The result for day 8 of AOC 2024 is: 912
