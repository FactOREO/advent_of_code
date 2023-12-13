input <- readLines("./input_test.txt")
input <- input[input != ""]

sections <- c("Seeds", "Seed-to-Soil", "Soil-to-Fertilizer", "Fertilizer-to-Water",
              "Water-to-Light", "Light-to-Temperature", "Temperatur-to-Humidity",
              "Humidity-to-Location")

pos <- 1L
ctr <- 1L
section <- sections[[pos]]
maps <- list()
for (i in seq_along(input)) {
  if (grepl(":$", input[[i]])) {
    pos <- pos + 1L
    ctr <- 1L
    section <- sections[[pos]]
  } else {
    nums <- stringr::str_extract_all(input[[i]], "\\d*") |> lapply(as.numeric) |> unlist()
    maps[[section]] <- rbind(maps[[section]], nums[!is.na(nums)])
    ctr <- ctr + 1L
  }
  if (pos > length(sections)) {
    break
  }
}

min_location <- Inf
locations <- c()
seeds <- maps[["Seeds"]]
maps[["Seeds"]] <- NULL

# Part 1: Find the smallst location value necessary with the given seeds
for (i in seq_along(seeds)) {
  location <- seeds[[i]]
  for (map in maps) {
    for (i in seq.default(1, nrow(map))) {
      dest_val <- map[[i, 1]]
      source_val <- map[[i, 2]]
      range_val <- map[[i, 3]]
      if (location >= source_val && location < source_val + range_val) {
        location <- location + (dest_val - source_val)
        match <- 1L
      } else {
        location <- location
        match <- 0L
      }
      if (match == 1L) break
    }
  }
  locations <- c(locations, location)
}

print(sprintf("Part 1: %f", min(locations)))

# Part 2: Smallest location for a range of seeds, not individual ones
# Idea: Take two values (seed start and range length), find all matching seeds in this range and map them.
# Here we only need the star and end point of the intersection between seedrange and map, since everything between will
# be within the range of those (remapped) values.
# All non-matching seeds need to remain at their location.
# Continue for all maps, than take the minima of all minima.


# Algorithm:
# 1) Given a matrix of seed range(s) and n map parts, find the intersection between the range(s) and the i-th map part in the current map
seed_matrix <- matrix(seeds, ncol = 2, byrow = TRUE)
seed_matrix[, 2] <- seed_matrix[, 1] + seed_matrix[, 2]

has_intersection <- function(range, map_part) {
  if (range[[2]] < map_part[[2]] || range[[1]] > map_part[[2]] + map_part[[3]]) return(FALSE)
  TRUE
}

find_intersection <- function(range, map_part) {
  intersection_start <- max(range[[1]], map_part[[2]])
  intersection_end   <- min(range[[2]], map_part[[2]] + map_part[[3]])
  if (intersection_end < intersection_start) return(range)
  c(intersection_start, intersection_end)
}

split_range <- function(range, map_part) {
  # Return: list with first entry = intersection or NULL
  #                   second entry = non-matching parts or NULL
  out <- vector("list", length = 2L)
  if (has_intersection(range, map_part)) {
    out[[1]] <- find_intersection(range, map_part)
    # Find the remainders and append them
    if (out[[1]][[1]] > range[[1]]) {
      out[[2]] <- rbind(out[[2]], c(range[[1]], intersection[[1]] - 1))
    }
    if (out[[1]][[2]] < range[[2]]) {
      out[[2]] <- rbind(out[[2]], c(intersection[[2]] + 1, range[[2]]))
    }
    return(out)
  }
  out[[2]] <- matrix(range, ncol = 2, byrow = TRUE)
  out
}

# 2) Calculate the new start and end points of the seed range subset which matches the map
get_range_location <- function(range, map_part) {
  return(c(
    range[[1]] + (map_part[[1]] - map_part[[2]]),
    range[[2]] + (map_part[[1]] - map_part[[2]])
  ))
}

process_range <- function(range, map_part) {
  # Input: single range
  # Output: Processed range(s), e.g. every matching subrange is matched and non-matching subranges returned
  ranges <- split_range(range, map_part)
  if (!is.null(ranges[[1]])) {
    # Case: There was a detected intersection in the given range
    ranges[[1]] <- get_range_location(ranges[[1]], map_part)
  }
  matrix(unlist(ranges), ncol = 2, byrow = TRUE)
}

# 3) Repeat until we reach the n-th map part of the current map
for (i in seq.default(1, nrow(seed_matrix))) {
  print(sprintf("Process seed range: %s", paste(seed_matrix[i, ], collapse = " - ")))
  out <- list()
  recheck <- seed_matrix
  for (j in seq.default(1, nrow(maps[[1]]))) {
    print(sprintf("Check map part %s", paste(maps[[1]][j, ], collapse = "|")))
    if (has_intersection(recheck[i, ], maps[[1]][j, ])) {
      print("Found an intersection")
      res <- process_range(recheck[i, ], maps[[1]][j, ])
      print(res)
      out[[i]] <- matrix(res[[1]], ncol = 2, byrow = TRUE)
      recheck <- matrix(res[[2]], ncol = 2, byrow = TRUE)
    }
  }
  print(out)
}



map_range <- function(range, map_part) {
  # map_part: numeric -> c(dest, source, range)
  #    range: numeric -> c(start, end)
  print(sprintf("Got map part: %s", paste(map_part, collapse = "|")))
  print(sprintf("Got range: %s", paste(range, collapse = " - ")))
  seed_map_intersection <- find_intersection(range, map_part)
  if (!is.null(seed_map_intersection)) {
    print(sprintf("Found intersection: %s", paste(seed_map_intersection, collapse = " - ")))
    return(
      get_range_location(seed_map_intersection[[1]], seed_map_intersection[[2]], map_part[[1]], map_part[[2]])
    )
  }
  print("Found no intersection")
  range
}

process_map <- function(location_range, map) {
  # Input
  # location_range: Matrix of ranges (one range per row)
  # map: A matrix of maps
  # Returns: A matrix of mapped ranges
  for (i in seq.default(1, nrow(map))) {
    # Take the ranges and calculate the mapped positions
    print(sprintf("Process map part %d", i))
    # Find the intersecting parts of each provided range and pass them to map_range
    intersections <- matrix(apply(location_range, 1, find_intersection, map[i, ]), ncol = 2, byrow = TRUE)
    # All non-matching ranges have to be saved to be passed to the next iteration
    # mapped_ranges <- apply(location_range, 1, map_range, map[i, ])
    # Save all changed ranges and do not continue fot them
    # Pass unchanged ranges to the next map part
    print(intersections)
  }
  # mapped_range
}
# 4) Pass all remaining seeds from the original range
# 5) Return a list of all (new) ranges
# 6) Repeat 1-5) until there are no maps left
# 7) Return the smallest value in all seed ranges
