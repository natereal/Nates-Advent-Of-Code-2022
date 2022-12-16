test <- c(
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
)

test

char <- function(str, n) {
    stringr::str_sub(str, n, n)
}

# fo for i in nrows
# put each result in row of matrix
# transpose matrix, and reverse to get stack?
vec <- test[3]
N <- 3
L <- nchar(vec)
check <- 3 * N + (N-1) == L

crate <- vector(mode = "character", length = N)
for(i in 1:N) {
    #' 1:3 + (i-1)*3 + (i-1)*1
    first <- 1 + (i-1)*4
    last  <- 3 + (i-1)*4
    sub <- stringr::str_sub(vec, first, last)
    if(sub == "   ") {
        crate[i] <- ""
    } else if(str_detect(sub, "[[:alpha:]]")) {
        crate[i] <- char(sub, 2)
    }
}
crate

readLines("inst/input05.txt")
readr::read_file("inst/input05.txt")


stacks <-list(c("Z", "N"), c("M", "C", "D"), c("P"))


get_last <- function(vector, n = 1) {
    tail(vector, n)
}

pop <- function(vector, n) {
    head(vector, -1*n)
}

push <- function(vector, ob) {
    c(vector, ob)
}

get_instructions <- function(input) {
    input |>
        stringr::str_subset("move") |>
        stringr::str_remove_all("move ") |>
        stringr::str_split("from|to", n = 3) |>
        purrr::map(as.numeric)
}

move <- function(stk, instructions) {
    stopifnot(length(instructions) == 3)
    n <- instructions[1]
    from <- instructions[2]
    to <- instructions[3]

    # for(i in 1:number_of_moves) {
    #     crate <- get_last(stk[[from]])
    #     stk[[from]] <- pop(stk[[from]])
    #     stk[[to]] <- push(stk[[to]], crate)
    # }
    crate <- get_last(stk[[from]], n = n)
    stk[[from]] <- pop(stk[[from]], n = n)
    stk[[to]] <- push(stk[[to]], crate)

    return(stk)
}

do_all_moves <- function(stacks, instructions) {
    I <- length(instructions)

    for(i in 1:I) {
        stacks <- move(stacks, instructions[[i]])
    }

    return(stacks)
}

solve <- function(input, stacks) {
    ins <- input |>
        get_instructions()

    do_all_moves(stacks = stacks, ins) |>
        purrr::map(get_last) |>
        unlist() |>
        paste0(collapse = "")
}

stacks <- list(
    c("D", "H", "N", "Q", "T", "W", "V", "B"),
    c("D", "W", "B"),
    c("T", "S", "Q", "W", "J", "C"),
    c("F", "J", "R", "N", "Z", "T", "P"),
    c("G", "P", "V", "J", "M", "S", "T"),
    c("B", "W", "F", "T", "N"),
    c("B", "L", "D", "Q", "F", "H", "V", "N"),
    c("H", "P", "F", "R"),
    c("Z", "S", "M", "B", "L", "N", "P", "H")
)

readLines("inst/input05.txt") |>
    solve(stacks = stacks)


#TODO: make a function to parse the stacks from the file
#TODO: put an option to move by one or move all at once;
#      could be 'move by n'
