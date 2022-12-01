library(adventofcode22)
x <- readLines("./inst/input01.txt")

p1 <- f01a(x)
p2 <- f01b(x)

stopifnot(p1 == aoc_solutions$day01a)
stopifnot(p2 == aoc_solutions$day01b)

input <- c(
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000"
)

as.numeric(input)
c(input[1], input[2], input[3])

input == ""
split(input, f = "")



input
x <- readLines("inst/input01.txt")
readr::read_file("inst/input01.txt")



search <- function(x) {
    L <- list()
    vec <- c()
    for(i in 1:length(x)) {
        if(x[i] != "") {
            vec <- append(vec, x[i])
        } else if(x[i] == "") {
            L <- append(L, list(vec))
            vec <- c()
        }
    }
    return(L)
}

purrr::map(L, ~ sum(as.numeric(.x))) |>
    purrr::reduce(max)

solve <- function(input) {
    input |>
        search() |>
        purrr::map(as.numeric) |>
        purrr::map(sum) |>
        purrr::reduce(max)
}

solve(input)
solve(readLines("inst/input01.txt"))

out <- readLines("inst/input01.txt") |>
    search() |>
    purrr::map(as.numeric) |>
    purrr::map(sum) |>
    unlist() |>
    sort(decreasing = TRUE)

sum(out[1], out[2], out[3])

