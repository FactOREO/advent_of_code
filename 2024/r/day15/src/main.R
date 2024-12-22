source("../../utils.R")
source("./functions.R")
PRINT_IMAGES <- FALSE

session_cookie <- readLines("../../../../.aoc_session")
# get_aoc(2024, 15, session_cookie)

input <- readLines("../input/input.txt")

warehouse <- do.call(rbind, strsplit(input[1:(which(input == "") - 1)], ""))
directions <- strsplit(paste(input[(which(input == "") + 1):length(input)], sep = "", collapse = ""), "")[[1]]

tictoc::tic()
robot <- which(warehouse == "@", arr.ind = TRUE)
for (i in seq_along(directions)) {
  warehouse <- move_robot_in_warehouse(robot, directions[[i]], warehouse)
  robot <- which(warehouse == "@", arr.ind = TRUE)
}
chests <- which(warehouse == "O", arr.ind = TRUE)
p1 <- sum((chests[, 1] - 1) * 100 + chests[, 2] -  1)
tictoc::toc()
print_result(2024, 15, p1)

# 1.386 sec elapsed
# The result for day 15 of AOC 2024 is: 1509863

# Expand the map to be twice as big
warehouse <- do.call(rbind, strsplit(input[1:(which(input == "") - 1)], ""))
directions <- strsplit(paste(input[(which(input == "") + 1):length(input)], sep = "", collapse = ""), "")[[1]]
warehouse <- do.call(rbind, apply(warehouse, 1, \(r) {
  out <- vector("character", length = 2 * length(r))
  for (i in seq_along(r)) {
    out[(2 * i - 1):(2 * i)] <- {
      if (r[[i]] == "#") {
        rep(r[[i]], 2)
      } else if (r[[i]] == "O") {
        c("[", "]")
      } else if (r[[i]] == "@") {
        c("@", ".")
      } else {
        rep(".", 2)
      }
    }
  }
  out
}, simplify = FALSE))
robot <- which(warehouse == "@", arr.ind = TRUE)

# Printing attributes for the GIF
char_to_num <- c("#" = 1, "." = 2, "@" = 3, "[" = 4, "]" = 5)
color_palette <- c("black", "white", "green", "brown", "brown")

tictoc::tic()
for (i in seq_along(directions)) {
  if (PRINT_IMAGES) {
    subdir <- sprintf("%02d", (i - 1) %/% 1000 + 1)
    if (! dir.exists(sprintf("../output/%s", subdir))) dir.create(sprintf("../output/%s", subdir))
    img_matrix <- matrix(char_to_num[warehouse], nrow = nrow(warehouse))
    png(filename = sprintf("../output/%s/frame_%03d.png", subdir, i), width = 800, height = 800)
    image(t(apply(img_matrix, 2, rev)), col = color_palette, axes = FALSE, main = paste("Move Nr", i))
    dev.off()
  }
  warehouse <- move_robot_in_large_warehouse(robot, directions[[i]], warehouse)
  robot <- which(warehouse == "@", arr.ind = TRUE)
}
chests <- which(warehouse == "[", arr.ind = TRUE) - 1
p2 <- sum(100 * chests[, 1] + chests[, 2])
tictoc::toc()
print_result(2024, 15, p2)

# 2.304 sec elapsed
# The result for day 15 of AOC 2024 is: 1548815
