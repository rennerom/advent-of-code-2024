box::use(R/aoc [fetch_raw_input_data])

input <- fetch_raw_input_data(2024, 23) |> 
  stringr::str_split("\n") |>
  unlist() |> 
  purrr::discard(~ .x == "") |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(value, names = c("parent", "child"), delim = "-")

g <- igraph::graph_from_data_frame(input, directed = FALSE)

clique <- igraph::largest_cliques(g)

result <- clique |> 
  purrr::map(~ igraph::V(g)$name[.x]) |> 
  unlist() |> 
  sort() |> 
  paste(collapse = ",")

glue::glue("answer: {result}")
