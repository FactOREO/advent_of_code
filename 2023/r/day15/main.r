input <- strsplit(readLines("./input.txt"), ",")[[1]]

char_to_ascii <- function(char) {
  strtoi(charToRaw(char), 16L)
}

hash_init <- function(string) {
  chars <- strsplit(string, "")[[1]]
  val <- 0L
  for (i in seq_along(chars)) {
    val <- ((val + char_to_ascii(chars[[i]])) * 17) %% 256
  }
  val
}

print(sprintf("Part 1: %s", sum(sapply(input, hash_init))))

### Part 2
# String to Box:
# rn=1
# hash_init(rn) -> 0
# '=' -> put the lens inside box 0
# '1' -> focal length is 1
# Box-Lens to value:
# (box-nr + 1) * lens_position * focal_length
# Values to Box-Result:
# sum(values)

remove_lense <- function(vec, pos) {
  # Given a vector, remove the element at the nth position and move elements afterwards one forward
  if (pos == 1L) return(tail(vec, -1))
  if (pos == length(vec)) return(head(vec, -1))
  c(vec[seq.int(1, (pos - 1L))], vec[seq.int(pos + 1L, length(vec))])
}

boxes <- vector("list", length = 256)
for (i in seq_along(input)) {
  lense_split <- strsplit(input[[i]], "-|=")[[1L]]
  box_nr <- hash_init(lense_split[[1L]]) + 1L
  lense <- lense_split[[1L]]
  if (length(lense_split) == 2L) {
    focal_length <- as.numeric(lense_split[[2L]])
    names(focal_length) <- lense
  }
  # Check, which operation to perform
  if (grepl("=", input[[i]])) {
    # Add the lens to the box, if present replace the existing lense and don't touch the position of others
    if (lense %in% names(boxes[[box_nr]])) {
      # Find the position the current lense is present and replace afterwards
      pos <- which(lense == names(boxes[[box_nr]]))
      # print(sprintf("Replace lense %s with new focal length %d in box %d at position %d from input %s",
      #               lense, focal_length, box_nr, pos, input[[i]]))
      boxes[[box_nr]][[pos]] <- focal_length
    } else {
      # Otherwise add at the end
      # print(sprintf("Add lense %s with focal length %d to box %d at position %d from input %s",
      #               lense, focal_length, box_nr, length(boxes[[box_nr]]) + 1L, input[[i]]))
      boxes[[box_nr]] <- c(boxes[[box_nr]], focal_length)
    }
    # print(sprintf("Box %d: [%s]", box_nr, paste(boxes[[box_nr]], collapse = ", ")))
  } else if (grepl("-", input[[i]])) {
    # Remove the lense and move all the others one forward
    if (lense %in% names(boxes[[box_nr]])) {
      # print(sprintf("Remove lense %s from box %d from input %s",
      #               lense, box_nr, input[[i]]))
      # Find the position the current lense is present and remove
      pos <- which(lense == names(boxes[[box_nr]]))
      boxes[[box_nr]] <- remove_lense(boxes[[box_nr]], pos)
      # print(sprintf("Box %d: [%s]", box_nr, paste(boxes[[box_nr]], collapse = ", ")))
    }
  }
}

# Calculate the final result
get_box_value <- function(vec, pos) {
  sum(pos * vec * seq_along(vec))
}

print(sprintf("Part 2: %d", sum(unlist(purrr::map2(boxes, seq_along(boxes), get_box_value)))))
