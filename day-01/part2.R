library(dplyr)
library(readr)

path <- "day-01/input.txt"

input <- read_fwf(path, fwf_empty(path, col_name = c("one", "two")))

one <- input$one
two <- input$two

result_vector <- numeric()

for (i in seq_along(one)) {
  ith_count <- two[two == one[i]] |> length()

  result_vector <- c(result_vector, ith_count * one[i])
}

result <- sum(result_vector)

glue("answer: {result}")