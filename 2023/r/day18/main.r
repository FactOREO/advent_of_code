# Adjusted solution from plannapus
# https://github.com/plannapus/Advent_of_Code/blob/master/2023/day18.R
input <- read.table("./input.txt", sep = " ", colClasses = c("character", "numeric", "character"), comment.char = "")

### PART 1
path <- matrix(c(0L, 0L), ncol = 2L)
for (i in seq_len(nrow(input))) {
  add <- switch(input[[i, 1L]],
                "R" = c(0L, 1L),
                "D" = c(1L, 0L),
                "L" = c(0L, -1L),
                "U" = c(-1L, 0L))
  for (j in seq_len(input[[i, 2L]])) {
    path <- rbind(path, path[nrow(path), ] + add)
  }
}
(abs(sum(path[-nrow(path), 1] * path[-1, 2]) - sum(path[-nrow(path), 2] * path[-1, 1])) + nrow(path) - 1) / 2 + 1

### PART 2
g <- gsub("[)(#)]", "", input[, 3L])
l <- strtoi(substr(g, 1, 5), 16)
d <- c("R", "D", "L", "U")[as.integer(substr(g, 6, 6)) + 1]
path <- matrix(c(0, 0), ncol = 2)
for (i in seq_len(nrow(input))) {
  add <- switch(d[[i]],
                "R" = c(0L, 1L),
                "D" = c(1L, 0L),
                "L" = c(0L, -1L),
                "U" = c(-1L, 0L))
  path <- rbind(path, path[nrow(path), ] + add * l[[i]])
}
options(digits=22)
area <- abs(sum(path[-nrow(path),1]*path[-1,2])-sum(path[-nrow(path),2]*path[-1,1]))/2
perim <- sum(abs(diff(path[,1])+diff(path[,2])))
area+perim/2+1
