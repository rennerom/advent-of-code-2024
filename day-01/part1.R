library(dplyr)
library(readr)
library(glue)

path <- "day-01/input.txt"

input <- read_fwf(path, fwf_empty(path, col_name = c("one", "two")))

one <- input$one |> sort()
two <- input$two |> sort()

three <- abs(one - two)

result <- sum(three)

glue("answer: {result}")