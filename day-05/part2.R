path <- "day-05/input.txt"

rules <- readr::read_lines(path) |> 
  purrr::keep(~grepl("\\|", .)) |> 
  purrr::map(~strsplit(.x, "\\|")[[1]]) |> 
  purrr::map(~as.numeric(.x))

manuals <- readr::read_lines(path) |> 
  purrr::discard(~grepl("\\||^$", .)) |> 
  purrr::map(~strsplit(.x, ",")[[1]]) |> 
  purrr::map(~as.numeric(.x))

is_out_of_order <- function(manual, rules) {
  # grab only the rules we need
  relevent_rules <- rules |> 
    purrr::keep(~all(.x %in% manual))

  # grab only the ones that are out of order this time
  !all(
    relevent_rules |> 
      purrr::map_lgl(
        # check manuals index order
        ~which(manual == .x[1]) < which(manual == .x[2])
      )
  )
}

out_of_order_manuals <- manuals |> 
  purrr::keep(~is_out_of_order(.x, rules))

re_order_manuals <- function(manual, rules) {
  # grab only the rules we need
  relevent_rules <- rules |> 
    purrr::keep(~all(.x %in% manual))
  
  sorted_manual <- manual
  changed <- TRUE
  
  # make R bubble sort
  while (changed) {
    changed <- FALSE
    for (rule in relevent_rules) {
      # get indexes of the two elements in the rule
      pos1 <- which(sorted_manual == rule[1])
      pos2 <- which(sorted_manual == rule[2])
      if (pos1 > pos2) {
        # swap them if needed
        sorted_manual[c(pos1, pos2)] <- sorted_manual[c(pos2, pos1)]
        changed <- TRUE
      }
    }
  }

  sorted_manual
}

reordered_manuals <- out_of_order_manuals |>
  purrr::map(~re_order_manuals(.x, rules))

results <- reordered_manuals |> 
  purrr::map(~list(.x[ceiling(length(.x) / 2)])) |>
  unlist() |>
  sum()

glue::glue("answer: {results}")
