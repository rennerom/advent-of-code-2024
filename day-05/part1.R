path <- "day-05/input.txt"

rules <- readr::read_lines(path) |> 
  purrr::keep(~grepl("\\|", .)) |> 
  purrr::map(~strsplit(.x, "\\|")[[1]]) |> 
  purrr::map(~as.numeric(.x)) |> 
  purrr::list_flatten()

manuals <- readr::read_lines(path) |> 
  purrr::discard(~grepl("\\||^$", .)) |> 
  purrr::map(~strsplit(.x, ",")[[1]]) |> 
  purrr::map(~as.numeric(.x))

is_valid_order <- function(manual, rules) {
  # grab only the rules we need
  relevent_rules <- rules |> 
    purrr::keep(~all(.x %in% manual))

  # make sure all the rules are met
  all(
    relevent_rules |> 
      purrr::map_lgl(
        # check manuals index order
        ~which(manual == .x[1]) < which(manual == .x[2])
      )
  )
}

results <- manuals |> 
  purrr::keep(~is_valid_order(.x, rules)) |> 
  purrr::map(~list(.x[ceiling(length(.x) / 2)])) |>
  unlist() |>
  sum()

glue::glue("answer: {results}")
