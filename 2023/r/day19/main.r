main <- function(file) {
  input <- readLines(file)

  # Get the parts and the workflows from input
  workflows <- head(input[-which(grepl("^\\{", input))], -1)
  parts <- input[which(grepl("^\\{", input))]

  # Transform the workflows and create them as global variables
  lapply(workflows, get_workflow) |> unlist() |> as.list() |> list2env(env = .GlobalEnv)

  # Define the final states A and R in .GlobalEnv
  A <<- "Accepted"
  R <<- "Rejected"

  # Evaluate the parts and select accepted parts
  evaluated_parts <- lapply(lapply(parts, parse_part), is_part_accepted, workflow = `in`) |> unlist()
  accepted_parts <- parts[evaluated_parts]

  # Calculate the result of part1 as the sum of all components of all accepted parts
  sum_of_values <- lapply(accepted_parts, parse_part) |> unlist() |> sum()
  print(sprintf("Part 1: %d", sum_of_values))

  # List of ranges for every combination
  x <- m <- a <- s <- c(1L, 4000L)
  ranges <- list(x, m, a, s)
  names(ranges) <- c("x", "m", "a", "s")
  # matrix with workflow name and all instructions
  wf_mat <- do.call(rbind, strsplit(workflows, "[}{]"))
  # named list of workflows with splitted instructions
  workflows <- strsplit(wf_mat[, 2L], ",")
  names(workflows) <- wf_mat[, 1L]
  combinations <- evaluate_workflows("in", ranges, 1L, workflows = workflows)
  print(sprintf("Part 2: %s", format(combinations, scientific = FALSE)))
}

# Function to transform a given workflow as string into a `data.table::fcase(...)` statement as string
get_workflow <- function(workflow_string) {
  # Split input string into
  #    left-side: workflow name | right-side: instructions
  workflow_string <- strsplit(workflow_string, "{", fixed = TRUE)[[1L]] |> sub(pattern = "}", replacement = "", x = _, fixed = TRUE)
  # Construct control flow from instructions
  workflow_name <- workflow_string[[1L]]
  instructions <- strsplit(workflow_string[[2L]], split = ",")[[1L]]
  control_flow <- paste0(
    "data.table::fcase(",
    paste(
          lapply(strsplit(instructions, ":"),
                 function(s) {
                   if (length(s) == 1L) {
                     paste0(c("default = ", s), collapse = "")
                   } else {
                     paste(s, collapse = ", ")
                   }
                 }) |> unlist(),
          collapse = ", "),
    ")"
  )
  # Return named control flow string
  workflow <- control_flow
  names(workflow) <- workflow_string[[1L]]
  workflow
}

# Function to split a part string into its components
parse_part <- function(part) {
  strsplit(part, ",")[[1L]] |>
    sub("\\{|\\}", "", x = _) |>
    strsplit("=") |>
    lapply(function(x) {
      v <- x[[2L]] |> as.integer()
      names(v) <- x[[1L]]
      v
    }) |>
    unlist()
}

# Recursive function to follow the workflow path of a given part
is_part_accepted <- function(workflow, part) {
  list2env(as.list(part), env = environment())
  if (workflow == "Accepted") return(TRUE)
  if (workflow == "Rejected") return(FALSE)
  workflow <- eval(parse(text = workflow))
  is_part_accepted(workflow, part)
}

evaluate_workflows <- function(workflow, ranges, position = 1L, workflows = workflows) {
  # Base cases A and R
  if (workflow == "A") {
    return(prod((diff(ranges[["x"]]) + 1),
                (diff(ranges[["m"]]) + 1),
                (diff(ranges[["a"]]) + 1),
                (diff(ranges[["s"]]) + 1))
    )
  }
  if (workflow == "R") {
    return(0)
  }

  # Recursive Cases
  condition <- workflows[[workflow]][[position]]
  if (grepl(":", condition)) {
    component <- substr(condition, 1L, 1L)
    rest <- strsplit(substr(condition, 3L, nchar(condition)), ":")[[1L]]
    value <- as.integer(rest[[1L]])
    target <- rest[[2L]]
    if (grepl(">", condition)) {
      if (ranges[[component]][[1L]] <= value && ranges[[component]][[2L]] > value) {
        ranges_1 <- ranges_2 <- ranges
        ranges_1[[component]][[2L]] <- value
        ranges_2[[component]][[1L]] <- value + 1L
        total <- evaluate_workflows(workflow, ranges_1, position + 1L, workflows = workflows) +
          evaluate_workflows(target, ranges_2, 1L, workflows = workflows)
      } else if (ranges[[component]][[1L]] > value) {
        total <- evaluate_workflows(target, ranges, 1L, workflows = workflows)
      } else {
        total <- evaluate_workflows(workflow, ranges, position + 1L, workflows = workflows)
      }
    } else if (grepl("<", condition)) {
      if (ranges[[component]][[1L]] < value && ranges[[component]][[2L]] >= value) {
        ranges_1 <- ranges_2 <- ranges
        ranges_1[[component]][[2L]] <- value - 1L
        ranges_2[[component]][[1L]] <- value
        total <- evaluate_workflows(workflow, ranges_2, position + 1L, workflows = workflows) +
          evaluate_workflows(target, ranges_1, 1L, workflows = workflows)
      } else if (ranges[[component]][[1L]] > value) {
        total <- evaluate_workflows(workflow, ranges, position + 1L, workflows = workflows)
      } else {
        total <- evaluate_workflows(target, ranges, 1L, workflows = workflows)
      }
    }
  } else {
    total <- evaluate_workflows(condition, ranges, 1L, workflows = workflows)
  }
  total
}

main("./input.txt")
