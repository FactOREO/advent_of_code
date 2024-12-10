source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
# get_aoc(2024, 9, session_cookie)
input <- readLines("../input/input.txt")

disk <- strsplit(input, "")[[1L]] |> as.integer()

unwind_compact_disk_storage <- function(disk) {
  res <- vector("numeric", sum(disk) - 1L)
  cp <- 0
  for (i in seq_along(disk) - 1L) {
    type <- if (i %% 2 == 0) "storage" else "space"
    block_size <- disk[[(i + 1L)]]
    if (block_size == 0L) next
    if (type == "storage") {
      res[(cp + 1):(cp + block_size)] <- i / 2
    } else {
      res[(cp + 1):(cp + block_size)] <- -1
    }
    cp <- cp + block_size
  }
  res
}

resort_unwinded_disk_storage <- function(disk) {
  while(any(disk == -1)) {
    repl_pos <- which(disk == -1)[[1L]]
    disk[[repl_pos]] <- disk[[length(disk)]]
    disk <- disk[-length(disk)]
  }
  disk
}

get_checksum <- function(disk) {
  idx <- seq_along(disk) - 1L
  sum(mapply(\(i, j) if (j == -1) 0 else i * j, i = idx, j = disk))
}

move_whole_files_to_front <- function(disk) {
  # From back to forth
  file_ids <- sort(setdiff(unique(disk), -1), decreasing=TRUE)
  for (i in seq_along(file_ids)) {
    file_id <- file_ids[[i]]
    file_length <- sum(disk == file_id)
    file_start <- min(which(disk == file_id))
    file_stop <- max(which(disk == file_id))
    # Check for all free spaces and it's length
    # gather it in a data.frame and repartition it
    block_spaces <- rle(disk)
    stop_idxs <- cumsum(block_spaces[["lengths"]])
    start_idxs <- c(1, head(stop_idxs + 1, -1))
    partitions <- data.frame(
      value = block_spaces[["values"]],
      start = start_idxs,
      length = block_spaces[["lengths"]]
    )
    target <- partitions[
      partitions$length >= file_length & # enough ...
      partitions$value == -1 &           # free space ...
      partitions$start < file_start      # before file
    ,]
    if (nrow(target) >= 1L) {
      disk[target$start[[1L]]:(target$start[[1L]] + file_length - 1L)] <- file_id
      disk[file_start:file_stop] <- -1L
    }
  }
  disk
}

tictoc::tic()
unwind_compact_disk_storage(disk) |>
  resort_unwinded_disk_storage() |>
  get_checksum() |>
  print_result(year = 2024, day = 9, solution = _)
tictoc::toc()

tictoc::tic()
unwind_compact_disk_storage(disk) |>
  move_whole_files_to_front() |>
  get_checksum() |>
  print_result(year = 2024, day = 9, solution = _)
tictoc::toc()

# The result for day 9 of AOC 2024 is: 6337921897505

# 27.825 sec elapsed
# The result for day 9 of AOC 2024 is: 6362722604045

# 30.01 sec elapsed
