crates <- list()
crates_cntr <- 1L
cmds <- vector("integer")

for (line in readLines("./test_input.txt")) {
  if (grepl("[", line, fixed = TRUE)) {
    crates[[crates_cntr]] <- gsub("[[:space:]]{3}", "[_]", line) |>
      (\(x) strsplit(x, "]", fixed = TRUE)[[1L]])() |>
      strsplit("[", fixed = TRUE) |>
      sapply(\(x) x[!grepl("^$|^[[:space:]]*$", x)])
    crates_cntr <- crates_cntr + 1L
  } else if (grepl("move", line, fixed = TRUE)) {
    steps <- gsub("[^[0-9]", "", line) |> strsplit("")
    cmds <- c(cmds, as.integer(steps[[1L]]))
  } else if (grepl("^$", line)) {
    next
  } else {
    strsplit(line, " ")[[1L]] |> max()
  }
}

# Matrix which stores the operations
cmd_matrix <- matrix(cmds, ncol = 3, byrow = TRUE)

# Now make a list with one vector per stack (instead of one vector per row)
n_max <- which.max(sapply(crates, length))
tmp_crates <- lapply(seq.default(1, length(crates)), \(x) rep(NA_character_, n_max))

for (i in seq_along(tmp_crates)) {
  for (j in seq.default(1, n_max)) {
    print(sprintf("Try to assign %d position in element %d", j, i))
    tmp_crates[[i]][[j]] <- if (crates[[j]][[i]] == "_" || length(crates[[j]]) < i) NA_character_ else crates[[j]][[i]]
  }
}

crates <- tmp_crates
print(crates)
rm(tmp_crates)

# Loop through the commands
for (i in seq.default(1, nrow(cmd_matrix))) {
  amount <- cmd_matrix[[i, 1]]
  origin <- cmd_matrix[[i, 2]]
  dest <- cmd_matrix[[i, 3]]
  print(sprintf("Move %d from %d to %d", amount, origin, dest))
  for (j in seq.default(1, amount)) {
    print(sprintf("step %d", j))
    
  }
}
