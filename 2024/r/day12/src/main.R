source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
# get_aoc(2024, 12, session_cookie)
input <- readLines("../input/test_input.txt")

garden <- do.call(rbind, strsplit(input, ""))
plants <- unique(as.vector(garden))

# Given a garden and a plant, identify all spots belonging together
# mark every belonging plant with an approriate indice, get the
# neighbours count for all the plants and finaly calculate the price
# for the fences needed per spot with area * perimeter
calc_neighbour_counts <- function(plant_spots) {
  # Get all possible neighbours
  neighbour_spots <- do.call(
    rbind,
    apply(
      plant_spots, 1L,
      \(ps) {
        matrix(c(ps[[1L]], ps[[2L]]) + c(-1L, 0L, 1L, 0L, 0L, -1L, 0L, 1L), ncol = 2, byrow = TRUE)
      },
      simplify = FALSE
    )
  )
  # Check all possible neighbour spots for intersections with the plant spots
  neighbours <- apply(plant_spots, 1L, \(plant, neighbour_spots) {
      match_row <- which(neighbour_spots[, 1L] == plant[[1L]])
      match_row[match_row %in% which(neighbour_spots[, 2L] == plant[[2L]])]
  }, neighbour_spots = neighbour_spots, simplify = FALSE)
}


solve_part1 <- function(plant, garden) {
  plant_spots <- which(garden == plant, arr.ind = TRUE)
  # Divide by the number of distinct spots
  # distinct_spots <- length(neighbour_count == 0L)
  # The necessary fences for every plant are 4 - neighbour_count
  # (4 * nrow(plant_spots) ^ 2 - sum(neighbour_count) * nrow(plant_spots)) / distinct_spots
}

sapply(plants, solve_part1, garden = garden)
