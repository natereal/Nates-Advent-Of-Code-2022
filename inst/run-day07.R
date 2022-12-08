library(tidyverse)
x <- readLines("./inst/input07.txt")

p1 <- f07a(x)
p2 <- f07b(x)

stopifnot(p1 == aoc_solutions$day07a)
stopifnot(p2 == aoc_solutions$day07b)

test <- c(
   "$ cd /",
   "$ ls",
   "dir a",
   "14848514 b.txt",
   "8504156 c.dat",
   "dir d",
   "$ cd a",
   "$ ls",
   "dir e",
   "29116 f",
   "2557 g",
   "62596 h.lst",
   "$ cd e",
   "$ ls",
   "584 i",
   "$ cd ..",
   "$ cd ..",
   "$ cd d",
   "$ ls",
   "4060174 j",
   "8033020 d.log",
   "5626152 d.ext",
   "7214296 k"
)

parse_input <- function(input) {
    commands <- input
    root <- list()
    # currently_listing <- FALSE
    target_list_name <- c()
    i <- 1
    while(i <= length(commands)) {

        if(commands[i] == "$ cd /") {
            # commands <- commands[-1]
            i <- i + 1
            isRoot <- TRUE
        } else if(commands[i] == "$ ls"){
            L <- list()
            # delete '$ ls'
            # commands <- commands[-1]
            i <- i + 1
            char <- str_sub(commands[i], 1, 1)
            # if(is.na(char)) char <- 0
            while(char != "$") {
                current <- commands[i]
                current <- str_split(current, pattern = " ") |>
                    unlist()
                if(current[1] == "dir") {
                    L <- append(L, list(0))
                } else {
                    val <- as.numeric(current[1])
                    L <- append(L, list(val))
                }
                names(L)[length(L)] <- current[2]
                # commands <- commands[-1]
                i <- i + 1
                char <- str_sub(commands[i], 1, 1)
                if(is.na(char)) break
            }

            if(isRoot) {
                root <- L
                isRoot <- FALSE
            } else {
                root[[target_list_name]] <- L
            }
            # when we get to this point, the top entry of the
            # commands list starts with $
        } else {
            current <- commands[i]
            current <- str_split(current, pattern = " ") |>
                unlist()
            if(current[3] != "..") {
                target_list_name <- c(target_list_name, current[3])
            } else if(current[3] == "..") {
                # deletes last entry
                target_list_name <- head(target_list_name, -1)
            }
            # commands <- commands[-1]
            i <- i + 1
        }
    }
    return(root)
}

# # manual set list 
# root <- list()
# root$a <- list()
# root$"b.txt" <- 14848514
# root$"c.dat" <- 8504156
# root$d <- list()

# root$a$e <- list()
# root$a$f <- 29116
# root$a$g <- 2557
# root$a$"h.lst" <- 62596

# root$a$e$i <- 584

# root$d$j <- 4060174
# root$d$"d.log" <- 8033020
# root$d$"d.ext" <- 5626152
# root$d$k <- 7214296
# root
# # ----------------------

isFile <- function(obj) {
    length(obj) == 1 & is.numeric(obj)
}

dir_sizes <- list()

# recursive function
get_size <- function(tree, this_list_name) {
    if(isFile(tree)) {
        return(tree)
    } else {
        size <- 0
        for(i in seq_along(1:length(tree))) {
            size <- size + get_size(tree[[i]], this_list_name = names(tree)[i])
        }
    }
    # print(this_list_name)
    # print(size)
    dir_sizes <<- append(dir_sizes, size)
    LAST <- length(dir_sizes)
    names(dir_sizes)[LAST] <<- this_list_name
    return(size)
}

get_size(root, this_list_name = "root")

dir_sizes 
values <- dir_sizes |>
    unlist() 

sum(values[values < 100000])

solve <- function(input) {
    dir_sizes <- list()
    input |>
        parse_input() |>
        get_size(this_list_name = "root")
    print(dir_sizes)
    values <- dir_sizes |>
        unlist()

    return(sum(values[values < 100000]))
}

solve(test)
parse_input(test) |>
    get_size(this_list_name = "root")

###
dir_sizes <- list()
readLines("inst/input07.txt") |>
    parse_input() |>
    get_size(this_list_name = "root")

dir_sizes 
values <- dir_sizes |>
    unlist() 

sum(values[values < 100000])

unused <- 70000000 - values["root"]
target_space <- 30000000 - unused
min(values[values > target_space])
