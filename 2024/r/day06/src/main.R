source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
get_aoc(2024, 6, session_cookie)
input <- readLines("../input/input.txt")

map <- strsplit(input, "", fixed=TRUE) |> do.call(rbind, args = _)

get_guard_path <- function(map) {
  guard_position <- which(map == "^", arr.ind = TRUE)
  guard_direction <- "^"
  seen_tiles <- vector("list", nrow(map)) |> lapply(\(i) vector("list", ncol(map)))
  exit <- FALSE
  while (!exit) {
    switch(guard_direction,
      "^" = {
        obstacle <- suppressWarnings(max(which(map[seq.int(1L, guard_position[[1L]]), guard_position[[2L]]] == "#")))
        if (obstacle == -Inf) {
          exit <- TRUE
          for (r in seq.int(1L, guard_position[[1L]])) {
            seen_tiles[[r]][[guard_position[[2L]]]] <- TRUE
          }
        } else {
          for (r in seq.int(guard_position[[1L]], obstacle + 1L)) {
            seen_tiles[[r]][[guard_position[[2L]]]] <- TRUE
          }
          guard_position[[1L]] <- obstacle + 1L
          guard_direction <- ">"
        }
      },
      ">" = {
        obstacle <- min(which(map[guard_position[[1L]], seq.int(guard_position[[2L]], ncol(map))] == "#"))
        if (obstacle == Inf) {
          exit <- TRUE
          for (c in seq.int(guard_position[[2L]], ncol(map))) {
            seen_tiles[[r]][[guard_position[[2L]]]] <- TRUE
          }
        } else {
          offset <- guard_position[[2L]] - 1L
          for (c in seq.int(guard_position[[2L]], offset + obstacle - 1L)) {
            seen_tiles[[guard_position[[1L]]]][[c]] <- TRUE
          }
          guard_position[[2L]] <- offset + obstacle - 1L
          guard_direction <- "v"
        }
      },
      "v" = {
        obstacle <- min(which(map[seq.int(guard_position[[1L]], nrow(map)), guard_position[[2L]]] == "#"))
        if (obstacle == Inf) {
          exit <- TRUE
          for (r in seq.int(guard_position[[1L]], nrow(map))) {
            seen_tiles[[r]][[guard_position[[2L]]]] <- TRUE
          }
        } else {
          offset <- guard_position[[1L]] - 1L
          for (r in seq.int(guard_position[[1L]], offset + obstacle - 1L)) {
            seen_tiles[[r]][[guard_position[[2L]]]] <- TRUE
          }
          guard_position[[1L]] <- offset + obstacle - 1L
          guard_direction <- "<"
        }
      },
      "<" = {
        obstacle  <- suppressWarnings(max(which(map[guard_position[[1L]], seq.int(1L, guard_position[[2L]] - 1L)] == "#")))
        if (obstacle == -Inf) {
          exit <- TRUE
          for (c in seq.int(1L, guard_position[[2L]])) {
            seen_tiles[[guard_position[[1L]]]][[c]] <- TRUE
          }
        } else {
          for (c in seq.int(obstacle + 1L, guard_position[[2L]])) {
            seen_tiles[[guard_position[[1L]]]][[c]] <- TRUE
          }
          guard_position[[2L]] <- obstacle + 1L
          guard_direction <- "^"
        }
      }
    )
  }
  seen_tiles
}

guard_tiles <- get_guard_path(map)
p1 <- sum(unlist(guard_tiles))
print_result(2024, 6, p1)

# Create loops - place a new obstacle on the map and see, if we can reach an already seen obstacle from the same
# direction. Only check new obstacles on tiles from part 1, since the guard will not reach other tiles anyway
check_for_loop <- function(map) {
  guard_position <- which(map == "^", arr.ind = TRUE)
  if (length(guard_position) == 0L) return(FALSE)
  guard_direction <- "^"
  obstacles <- vector("list", length=nrow(map)) |> lapply(\(i) vector("list", ncol(map)))
  exit <- FALSE
  found_loop <- FALSE
  while (! (exit || found_loop)) {
    switch(guard_direction,
      "^" = {
        obstacle <- max(which(map[seq.int(1L, guard_position[[1L]]), guard_position[[2L]]] == "#"))
        if (obstacle == -Inf) {
          exit <- TRUE
        } else {
          if (guard_direction %in% obstacles[[obstacle]][[guard_position[[2L]]]]) {
            found_loop <- TRUE
          } else {
            # print(paste(obstacle, guard_position[[2L]]))
            obstacles[[obstacle]][[guard_position[[2L]]]] <-
              c(obstacles[[obstacle]][[guard_position[[2L]]]], guard_direction)
            guard_position[[1L]] <- obstacle + 1L
            guard_direction <- ">"
          }
        }
      },
      ">" = {
        obstacle <- min(which(map[guard_position[[1L]], seq.int(guard_position[[2L]], ncol(map))] == "#"))
        if (obstacle == Inf) {
          exit <- TRUE
        } else {
          offset <- guard_position[[2L]] - 1L
          if (guard_direction %in% obstacles[[guard_position[[1L]]]][[offset + obstacle]]) {
            found_loop <- TRUE
          } else {
            # print(paste(guard_position[[1L]], offset + obstacle))
            obstacles[[guard_position[[1L]]]][[offset + obstacle]] <-
              c(obstacles[[guard_position[[1L]]]][[offset + obstacle]], guard_direction)
            guard_position[[2L]] <- offset + obstacle - 1L
            guard_direction <- "v"
          }
        }
      },
      "v" = {
        obstacle <- min(which(map[seq.int(guard_position[[1L]], nrow(map)), guard_position[[2L]]] == "#"))
        if (obstacle == Inf) {
          exit <- TRUE
        } else {
          offset <- guard_position[[1L]] - 1L
          if (guard_direction %in% obstacles[[offset + obstacle]][[guard_position[[2L]]]]) {
            found_loop <- TRUE
          } else {
            # print(paste(offset + obstacle, guard_position[[2L]]))
            obstacles[[offset + obstacle]][[guard_position[[2L]]]] <-
              c(obstacles[[offset + obstacle]][[guard_position[[2L]]]], guard_direction)
            guard_position[[1L]] <- offset + obstacle - 1L
            guard_direction <- "<"
          }
        }
      },
      "<" = {
        obstacle  <- max(which(map[guard_position[[1L]], seq.int(1L, guard_position[[2L]] - 1L)] == "#"))
        if (obstacle == -Inf) {
          exit <- TRUE
        } else {
          if (guard_direction %in% obstacles[[guard_position[[1L]]]][[obstacle]]) {
            found_loop <- TRUE
          } else {
            # print(paste(guard_position[[1L]], obstacle))
            obstacles[[guard_position[[1L]]]][[obstacle]] <-
              c(obstacles[[guard_position[[1L]]]][[obstacle]], guard_direction)
            guard_position[[2L]] <- obstacle + 1L
            guard_direction <- "^"
          }
        }
      }
    )
  }
  if (found_loop) {
    TRUE
  } else {
    FALSE
  }
}

# Indices to check for a loop if a new obstacle is placed
tiles_to_check <-  lapply(guard_tiles, \(l) { sapply(l, isTRUE) } ) |>
  unlist() |>
  matrix(ncol = ncol(map), byrow=TRUE) |>
  which(arr.ind=TRUE)

# Apply the check_for_loop function to all those positions
loops <- mapply(function(r, c, map) {
  clone_map <- map
  clone_map[[r, c]] <- "#"
  check_for_loop(clone_map)
}, r = tiles_to_check[, 1], c = tiles_to_check[, 2], MoreArgs = list(map = map))

detected_loops <- tiles_to_check[which(loops), ]

p2 <- sum(loops)
print_result(2024, 6, p2)
