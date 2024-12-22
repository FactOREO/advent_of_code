#' Get next position
get_next_position <- function(x, y, direction) {
  switch(direction,
    ">" = c(x, y + 1),
    "<" = c(x, y - 1),
    "^" = c(x - 1, y),
    "v" = c(x + 1, y)
  )
}

#' Get tile value
get_tile_value <- function(x, y, warehouse) {
  warehouse[[x, y]]
}

#' Get movable chests
#'
#' Get a list of movable chests (only left positions) for vertial movements
get_movable_chests <- function(cp1, cp2, direction, warehouse, chests = NULL) {
  # Add current chest position 1 to the chests
  # and get unique subset
  chests <- rbind(chests, cp1)
  # Next position of chest part 1 / 2
  cp1_np <- get_next_position(cp1[[1]], cp1[[2]], direction)
  cp2_np <- get_next_position(cp2[[1]], cp2[[2]], direction)
  # Next tile of chest part 1 / 2
  cp1_nt <- get_tile_value(cp1_np[[1]], cp1_np[[2]], warehouse)
  cp2_nt <- get_tile_value(cp2_np[[1]], cp2_np[[2]], warehouse)
  if (cp1_nt == "." && cp2_nt == ".") {
    # print(chests)
    chests
  } else if (cp1_nt == "[" && cp2_nt == "]") {
    # One chest exatly
    get_movable_chests(cp1_np, cp2_np, direction, warehouse, chests)
  } else if (cp1_nt == "]" && cp2_nt == "[") {
    # Two chests next
    rbind(
      get_movable_chests(c(cp1_np[[1]], cp1_np[[2]] - 1), cp1_np, direction, warehouse, chests),
      get_movable_chests(cp2_np, c(cp2_np[[1]], cp2_np[[2]] + 1), direction, warehouse, chests)
    )
  } else if (cp1_nt == "]") {
    # One chest left part
    get_movable_chests(c(cp1_np[[1]], cp1_np[[2]] - 1), cp1_np, direction, warehouse, chests)
  } else {
    # One chest right part
    get_movable_chests(cp2_np, c(cp2_np[[1]], cp2_np[[2]] + 1), direction, warehouse, chests)
  }
}

#' Get first free space in direction
get_first_target_tile <- function(x, y, tile, direction, warehouse) {
  switch(direction,
    ">" = which(warehouse[x, (y + 1):ncol(warehouse)] == tile)[[1]],
    "<" = which(warehouse[x, (y - 1):1] == tile)[[1]],
    "^" = which(warehouse[(x - 1):1, y] == tile)[[1]],
    "v" = which(warehouse[(x + 1):nrow(warehouse), y] == tile)[[1]]
  )
}

#' Get entries till wall in direction
get_entries_till_wall <- function(x, y, direction, warehouse) {
  switch(direction,
    ">" = warehouse[x, (y + 1):ncol(warehouse)],
    "<" = warehouse[x, 1:(y - 1)],
    "^" = warehouse[1:(x - 1), y],
    "v" = warehouse[(x + 1):nrow(warehouse), y]
  )
}

#' Check if a chest is movable in vertical direction
#'
#'  Input Entries always have a space or more chests above the chest before the first wall
#'  Now check second part of the chest => (4, 5)
#'  - if wall is above, no movement possible
#'  - if space is above, check the second tile according to the same rules as second tile
#'    from the first chest
#'  - if another chest is above, check the second tile according to the same rules as
#'    second tile from the first check + do the same checks as with the first chest part
chest_is_vertically_movable <- function(cp1, cp2, direction, warehouse) {
  # cat(sprintf(
  #   "Check chest position %s - %s\n",
  #   paste(cp1, collapse = ", "), paste(cp2, collapse = ", ")
  # ))
  # Next position of chest part 1 / 2
  cp1_np <- get_next_position(cp1[[1]], cp1[[2]], direction)
  cp2_np <- get_next_position(cp2[[1]], cp2[[2]], direction)
  # Next tile of chest part 1 / 2
  cp1_nt <- get_tile_value(cp1_np[[1]], cp1_np[[2]], warehouse)
  cp2_nt <- get_tile_value(cp2_np[[1]], cp2_np[[2]], warehouse)
  # cat(sprintf(
  #   "Found %s at (%s) and %s at (%s) in direction %s\n",
  #   cp1_nt, paste(cp1_np, collapse = ", "), cp2_nt, paste(cp2_np, collapse = ", "), direction
  # ))
  # If all are space -> TRUE | If any is wall -> FALSE | Else invoke again with chest positions
  if (cp1_nt == "." && cp2_nt == ".") {
    # print("movable")
    TRUE
  } else if (cp1_nt == "#" || cp2_nt == "#") {
    # print("not movable")
    FALSE
  } else if (cp1_nt == "[" && cp2_nt == "]") {
    # One chest exactly above the other
    # print("Found one chest exactly")
    chest_is_vertically_movable(cp1_np, cp2_np, direction, warehouse)
  } else if (cp1_nt == "]" && cp2_nt == "[") {
    # Two chests above the one
    # print("Found two chests next")
    all(
      chest_is_vertically_movable(c(cp1_np[[1]], cp1_np[[2]] - 1), cp1_np, direction, warehouse),
      chest_is_vertically_movable(cp2_np, c(cp2_np[[1]], cp2_np[[2]] + 1), direction, warehouse)
    )
  } else if (cp1_nt == "]") {
    # One chest above left part
    # print("Found one chest at left")
    chest_is_vertically_movable(c(cp1_np[[1]], cp1_np[[2]] - 1), cp1_np, direction, warehouse)
  } else {
    # One chest above right part
    # print("Found one chest at right")
    chest_is_vertically_movable(cp2_np, c(cp2_np[[1]], cp2_np[[2]] + 1), direction, warehouse)
  }
}

#' Check if position is feasible
is_position_feasible <- function(position, warehouse) {
  ! any(position == 1 | position[[1]] == nrow(warehouse) | position[[2]] == ncol(warehouse)) #nolint
}

#' Reposition entries in warehouse
reposition_warehouse <- function(x, y, direction, warehouse, free_space_idx, entries) {
  warehouse <- switch(direction,
    ">" = {
      for (i in seq.int(free_space_idx, 2)) warehouse[[x, y + i]] <- entries[[i - 1]]
      warehouse
    },
    "<" = {
      for (i in seq.int(free_space_idx, 2)) warehouse[[x, y - i]] <- entries[y - i + 1]
      warehouse
    },
    "^" = {
      for (i in seq.int(free_space_idx, 2)) warehouse[[x - i, y]] <- rev(entries)[[i - 1]]
      warehouse
    },
    "v" = {
      for (i in seq.int(free_space_idx, 2)) warehouse[[x + i, y]] <- entries[[i - 1]]
      warehouse
    }
  )
  # Actually move the robot
  next_position <- get_next_position(x, y, direction)
  warehouse[[next_position[[1]], next_position[[2]]]] <- "@"
  warehouse[[x, y]] <- "."
  warehouse
}

move_robot_in_warehouse <- function(robot, direction, warehofirst_space) {
  next_position <-  get_next_position(robot[[1]], robot[[2]], direction)
  if (!(is_position_feasible(next_position, warehouse))) {
    return(warehouse)
  }
  next_tile <- get_tile_value(next_position[[1]], next_position[[2]], warehouse)
  if (next_tile == "#") {
    return(warehouse)
  }
  if (next_tile == ".") {
    warehouse[[robot[[1]], robot[[2]]]] <- "."
    warehouse[[next_position[[1]], next_position[[2]]]] <- "@"
    return(warehouse)
  }
  entries <- get_entries_till_wall(robot[[1]], robot[[2]], direction, warehouse)
  if (! any(entries == "."))
    return(warehouse)

  first_space <- get_first_target_tile(robot[[1]], robot[[2]], ".", direction, warehouse)
  first_wall  <- get_first_target_tile(robot[[1]], robot[[2]], "#", direction, warehouse)
  if (first_wall < first_space) return(warehouse)

  # Move everything up to the next wall one into direction
  reposition_warehouse(
    robot[[1]], robot[[2]], direction, warehouse, first_space, entries
  )
}


#' Reposition entries in larger warehouse
reposition_large_warehouse <- function(x, y, direction, warehouse, free_space_idx, entries) {
  warehouse <- switch(direction,
    ">" = {
      for (i in seq.int(free_space_idx, 2)) warehouse[[x, y + i]] <- entries[[i - 1]]
      warehouse
    },
    "<" = {
      for (i in seq.int(free_space_idx, 2)) warehouse[[x, y - i]] <- entries[y - i + 1]
      warehouse
    },
    "^" = {
      # Check if all chests above could be moved upwards and if so, repeat the replacements
      # for every vector of entries where a chest is located
      # If the chest part is "[", we have to check to the right, else to the left
      cp1 <- get_next_position(x, y, direction)
      if (warehouse[[cp1[[1]], cp1[[2]]]] == "[") {
        cp2 <- get_next_position(cp1[[1]], cp1[[2]], ">")
      } else {
        # cp1 should be the left edge, so here we must swap the chest positions
        cp2_tmp <- get_next_position(cp1[[1]], cp1[[2]], "<")
        cp2 <- cp1
        cp1 <- cp2_tmp
      }
      if (! chest_is_vertically_movable(cp1, cp2, direction, warehouse)) {
        return(warehouse)
      }
      # Get all chests effected and move from top to bottom
      movable_chests <- get_movable_chests(cp1, cp2, direction, warehouse)
      if (nrow(movable_chests) > 1)
        movable_chests <- unique(movable_chests[order(movable_chests[, 1], decreasing = TRUE), ])
      # print(movable_chests)
      for (i in seq.int(1, nrow(movable_chests))) {
        idx <- nrow(movable_chests) + 1 - i
        # Move left part one up
        warehouse[[movable_chests[[idx, 1]] - 1, movable_chests[[idx, 2]]]] <- "["
        warehouse[[movable_chests[[idx, 1]], movable_chests[[idx, 2]]]] <- "."
        # Move right part one up
        warehouse[[movable_chests[[idx, 1]] - 1, movable_chests[[idx, 2]] + 1]] <- "]"
        warehouse[[movable_chests[[idx, 1]], movable_chests[[idx, 2]] + 1]] <- "."
      }
      warehouse
    },
    "v" = {
      cp1 <- get_next_position(x, y, direction)
      if (warehouse[[cp1[[1]], cp1[[2]]]] == "[") {
        cp2 <- get_next_position(cp1[[1]], cp1[[2]], ">")
      } else {
        # cp1 should be the left edge, so here we must swap the chest positions
        cp2_tmp <- get_next_position(cp1[[1]], cp1[[2]], "<")
        cp2 <- cp1
        cp1 <- cp2_tmp
      }
      if (! chest_is_vertically_movable(cp1, cp2, direction, warehouse)) {
        return(warehouse)
      }
      # Get all chests effected and move from top to bottom
      movable_chests <- get_movable_chests(cp1, cp2, direction, warehouse)
      if (nrow(movable_chests) > 1)
        movable_chests <- unique(movable_chests[order(movable_chests[, 1], decreasing = FALSE), ])
      for (i in seq.int(1, nrow(movable_chests))) {
        idx <- nrow(movable_chests) + 1 - i
        # cat(sprintf("Move chest at %s\n", paste(movable_chests[idx, ], collapse = ", ")))
        # Move left part one down
        # cat(sprintf(
        #   "Move left part one down - Replace %s at %s with [\n",
        #   warehouse[movable_chests[[idx, 1]] + 1, movable_chests[[idx, 2]]],
        #   paste(movable_chests[[idx, 1]] + 1, movable_chests[[idx, 2]], collapse = ", ")
        # ))
        warehouse[[movable_chests[[idx, 1]] + 1, movable_chests[[idx, 2]]]] <- "["
        warehouse[[movable_chests[[idx, 1]], movable_chests[[idx, 2]]]] <- "."
        # Move right part one down
        # cat(sprintf(
        #   "Move right part one down - Replace %s at %s with ]\n",
        #   warehouse[[movable_chests[[idx, 1]] + 1, movable_chests[[idx, 2]] + 1]],
        #   paste(movable_chests[[idx, 1]] + 1, movable_chests[[idx, 2]] + 1, collapse = ", ")
        # ))
        warehouse[[movable_chests[[idx, 1]] + 1, movable_chests[[idx, 2]] + 1]] <- "]"
        warehouse[[movable_chests[[idx, 1]], movable_chests[[idx, 2]] + 1]] <- "."
      }
      warehouse
    }
  )
  # Actually move the robot
  next_position <- get_next_position(x, y, direction)
  warehouse[[next_position[[1]], next_position[[2]]]] <- "@"
  warehouse[[x, y]] <- "."
  warehouse
}


# Extend the logic from move_robot_in_warehouse to handle a larger chest
move_robot_in_large_warehouse <- function(robot, direction, warehouse) {
  next_position <-  get_next_position(robot[[1]], robot[[2]], direction)
  if (!(is_position_feasible(next_position, warehouse))) {
    return(warehouse)
  }
  next_tile <- get_tile_value(next_position[[1]], next_position[[2]], warehouse)
  if (next_tile == "#") {
    return(warehouse)
  }
  if (next_tile == ".") {
    warehouse[[robot[[1]], robot[[2]]]] <- "."
    warehouse[[next_position[[1]], next_position[[2]]]] <- "@"
    return(warehouse)
  }
  entries <- get_entries_till_wall(robot[[1]], robot[[2]], direction, warehouse)
  if (! any(entries == "."))
    return(warehouse)

  first_space <- get_first_target_tile(robot[[1]], robot[[2]], ".", direction, warehouse)
  first_wall  <- get_first_target_tile(robot[[1]], robot[[2]], "#", direction, warehouse)
  if (first_wall < first_space) return(warehouse)

  # Move everything up to the next wall one into direction
  reposition_large_warehouse(
    robot[[1]], robot[[2]], direction, warehouse, first_space, entries
  )
}

