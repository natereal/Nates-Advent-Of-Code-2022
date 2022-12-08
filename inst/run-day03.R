library(adventofcode22)
x <- readLines("./inst/input03.txt")

p1 <- f03a(x)
p2 <- f03b(x)

stopifnot(p1 == aoc_solutions$day03a)
stopifnot(p2 == aoc_solutions$day03b)

test <- c(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
)

half_string <- function(string) {
    N <- nchar(string)
    s1 <- stringr::str_sub(string, 1, N * 0.5)
    s2 <- stringr::str_sub(string, N * 0.5 + 1, N)

    return(list(s1, s2))
}

find_common <- function(list_letters) {
    x <- list_letters[[1]]
    y <- list_letters[[2]]

    return(x[x %in% y])
}

convert_to_value <- function(letter) {
    which(letter == c(letters, LETTERS))
}
#
# i <- 1
# l <- nchar(test[i])
# test[1]
# # assume l is even
# s1 <- str_sub(test[i], start = 1, end = l * 0.5)
# s2 <- str_sub(test[i], start = l * 0.5 + 1, end = l)

split_string <- function(list_strings) {
    s1 <- list_strings[[1]] |>
        stringr::str_split(pattern = "")

    s2 <- list_strings[[2]] |>
        stringr::str_split(pattern = "")

    return(c(s1, s2))
}

# x <- stringr::str_split(s1, pattern = "")
# y <- stringr::str_split(s2, pattern = "")
# x == y
# common <- x[x %in% y]
#
# alpha <- c(letters, LETTERS)
# which(common == alpha)

# test[1] |>
#     half_string() |>
#     split_string() |>
#     find_common() |>
#     convert_to_value()

parse_string <- function(string) {
    string |>
        half_string() |>
        split_string() |>
        find_common()
}

parse_list_of_strings <- function(input) {
    input |>
        purrr::map(parse_string) |>
        purrr::map(unique)

}

parse_list_of_strings(test) |>
    purrr::map(convert_to_value) |>
    unlist() |>
    sum()

solve <- function(input) {
    input |>
        parse_list_of_strings() |>
        purrr::map(convert_to_value) |>
        unlist() |>
        sum()
}

solve(test)
solve(readLines("inst/input03.txt"))

test <- readLines("inst/input03.txt")
length(test)
test |>
    map(.f = ~ str_split(.x, pattern = ""))

L <- length(test)
I <- L / 3

xx <- test |>
    map(str_split, pattern = "") |>
    purrr::flatten()

vec <- c()
for(i in 1:I) {
    ii <- (i-1)*3 + c(1:3)
    yy <- find_common(xx[c(ii[1], ii[2])])
    out <- find_common(c(list(yy), xx[ii[3]]))
    out <- convert_to_value(out)
    vec <- append(vec, out)
}

sum(vec)

