source("../../utils.R")
session_cookie <- readLines("../../../../.aoc_session")
input <- get_aoc(2024, 5, session_cookie)
# input <- readLines("../input/test_input.txt")

# Find the empty section divider
divider <- which(grepl("^$", input))
page_orderings <- strsplit(input[seq.int(1L, (divider - 1L))], "|", fixed = TRUE)
update_instructions <- input[(divider + 1L):length(input)]

# Create a mapping with the structure
# "page_nr":  c(pages after this page if update)
page_mapping <- vector(mode = "list", length = 0L)
for (i in seq_along(page_orderings)) {
  pages <- page_orderings[[i]]
  if (pages[[1L]] %in% names(page_mapping)) {
    page_mapping[[pages[[1L]]]] <- c(page_mapping[[pages[[1L]]]], pages[[2L]])
  } else {
    page_mapping[[pages[[1L]]]] <- pages[[2L]]
  }
}

get_list_of_vecs_from_vec <- function(vec) {
  res <- sapply(seq_along(vec)[-length(vec)], function(i) vec[(i + 1):length(vec)], simplify = FALSE)
  names(res) <- vec[-length(vec)]
  res
}

check_instructions_from_page <- function(page, instructions, page_mapping) {
  all(instructions %in% page_mapping[[page]])
}

is_update_in_correct_order <- function(instruction, page_mapping) {
  # Split the update order into a vector of pages
  instructions <- strsplit(instruction, ",")[[1L]]
  # The Mapping defines all pages which have to be printed after the selected page
  # hence we need to check, if every subset of `instructions` (e.g. the elements after the
  # current position) are within the entry of the mapping for the given key
  list_of_instructions <- get_list_of_vecs_from_vec(instructions)
  in_order <- mapply(
    check_instructions_from_page,
    page = names(list_of_instructions),
    instructions = list_of_instructions,
    MoreArgs=list(page_mapping = page_mapping)
  )
  if (any(!in_order)) FALSE else TRUE
}

get_middle_value_as_integer <- function(vec) {
  as.integer(vec[[(ceiling(length(vec) / 2))]])
}

correct_instructions <- sapply(lapply(
  update_instructions, is_update_in_correct_order, page_mapping = page_mapping), isTRUE
)

p1 <- mapply(function(vec, in_order) {
  if (in_order) get_middle_value_as_integer(strsplit(vec, ",")[[1L]]) else 0L
}, vec = update_instructions, in_order = correct_instructions) |> sum()

print_result(2024, 5, p1)

### Part 2
# Get all wrong instructions and re-order them to get the correct instruction order
# -> Go from the first to the last wrong page in the instruction vector and find the furthest page apart in the
#    instruction chain, which lists the wrong page as a subsequent page as well as the closest page in the instruction
#    chain, which is part of the pages the wrong page should be before. Than put the wrong page in the middle of the
#    two subsequent pages in the chain. This has to be done iteratively, I think...
instruction_is_wrong <- sapply(lapply(
  update_instructions, is_update_in_correct_order, page_mapping = page_mapping), isFALSE
)
wrong_instructions <- update_instructions[which(instruction_is_wrong)]

rearrange_wrong_instruction <- function(instruction, page_mapping) {
  instructions <- strsplit(instruction, ",")[[1L]]
  # Initialize the list of instructions
  list_of_instructions <- get_list_of_vecs_from_vec(instructions)
  # Actual wrong pages
  in_order <- mapply(
    check_instructions_from_page,
    page = names(list_of_instructions),
    instructions = list_of_instructions,
    MoreArgs=list(page_mapping = page_mapping)
  )

  while (!all(in_order)) {
    wrong_pages <- which(sapply(in_order, isFALSE))

    # Only one page at a time
    wrong_page <- names(wrong_pages)[[1L]]
    page_idx <- wrong_pages[[1L]]
    pages_to_check <- page_mapping[setdiff(instructions, wrong_page)]
    # non existent pages in the mapping will cause this to fail
    pages_to_check <- pages_to_check[!is.na(names(pages_to_check))]
    should_follow <- lapply(
      pages_to_check,
      \(pages) {
        wrong_page %in% pages
      })

    # Remove the page from the instructions
    instructions <- instructions[-page_idx]

    if (all(unlist(should_follow)) && length(unlist(should_follow)) == length(instructions)) {
      # Put the page to the last position since it should come before no other page
      instructions <- c(instructions, wrong_page)
    } else if (! any(unlist(should_follow))) {
      # Put the page to the first position
      instructions <- c(wrong_page, instructions)
    } else {
      # Put the page after the last `should_follow` page
      page_before <- names(should_follow)[[max(which(unlist(should_follow)))]]
      new_idx <- which(names(should_follow) == page_before) + 1L
      # If this is the last page, apply first logic
      if (new_idx > length(instructions)) {
        instructions <- c(instructions, wrong_page)
      } else {
        instructions <- c(
          instructions[seq.int(1L, (new_idx - 1L))],
          wrong_page,
          instructions[seq.int((new_idx), length(instructions))]
        )
      }
    }
    # Update the list of instructions and get new wrong pages
    list_of_instructions <- get_list_of_vecs_from_vec(instructions)
    in_order <- mapply(
      check_instructions_from_page,
      page = names(list_of_instructions),
      instructions = list_of_instructions,
      MoreArgs=list(page_mapping = page_mapping)
    )
  }
  instructions
}

new_instructions <- mapply(rearrange_wrong_instruction, instruction=wrong_instructions, MoreArgs=list(page_mapping=page_mapping))

p2 <- sapply(new_instructions, get_middle_value_as_integer) |> sum()

print_result(2024, 5, p2)
