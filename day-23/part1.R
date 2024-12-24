box::use(R/aoc [fetch_raw_input_data])

input <- fetch_raw_input_data(2024, 23) |> 
  stringr::str_split("\n") |>
  unlist() |> 
  purrr::discard(~ .x == "") |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(value, names = c("parent", "child"), delim = "-")

input <- readr::read_lines("day-23/sample.txt") |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(value, names = c("parent", "child"), delim = "-")

g <- igraph::graph_from_data_frame(input, directed = FALSE)

cliques <- 
  igraph::cliques(g, min=3, max= 3) |> 
  purrr::map(~ igraph::V(g)$name[.x]) |> 
  purrr::keep(~ any(grepl("^t", .x)))

result <- length(cliques)

glue::glue("answer: {result}")
