library(adventofcode22)
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

dir <- list(type = "dir", size = NA)

basefile <- function(size) {
    list(type = "file", size = size)
}

root = list(
    info = dir
)

root

assign("a", list(info = dir))
root$a <- list(info = dir)
root$b <- list(info = basefile(14848514))
root$c <- list(info = basefile(8504156))
root$d <- list(info = dir)
str(root)
root$a$e <- list(info = dir)
root$a$f <- list(info = basefile(29116))
root$a$g <- list(info = basefile(2557))
root$a$h <- list(info = basefile(62596))
root$a$e$i <- list(info = basefile(584))
root$d$j <- list(info = basefile(4060174))
root$d$dlog <- list(info = basefile(8033020))
root$d$dext <- list(info = basefile(5626152))
root$d$k <- list(info = basefile(7214296))

str(root)
map(root, .f = ~ {
    if(.x$info == "dir") {

    }
})

commands <- test
root <- list()
currently_listing <- FALSE
levels <- 0
target_list_name <- c()
while(!is.null(commands)) {

    if(commands[1] == "$ cd /") {
        commands <- commands[-1]
        isRoot <- TRUE
    } else if(commands[1] == "$ ls"){
        L <- list()
        # delete '$ ls'
        commands <- commands[-1]

        while(str_sub(commands[1], 1, 1) != "$") {
            current <- commands[1]
            current <- str_split(current, pattern = " ") |>
                unlist()
            if(current[1] == "dir") {
                L <- append(L, list(0))
            } else {
                val <- as.numeric(current[1])
                L <- append(L, list(val))
            }
            names(L)[length(L)] <- current[2]
            commands <- commands[-1]
        }

        if(isRoot) {
            root <- L
            isRoot <- FALSE
        } else {
            target <- tail(target_list_name, 1)
            root <- purrr::assign_in(root, target, L)
        }
        # when we get to this point, the top entry of the
        # commands list starts with $
    } else {
        current <- commands[1]
        current <- str_split(current, pattern = " ") |>
            unlist()
        if(current[3] != "..") {
            levels <- levels + 1
            target_list_name <- c(target_list_name, current[3])
            LL <- L
        } else if(current[3] == "..") {
            levels <- levels - 1
            # deletes last entry
            target_list_name <- head(target_list_name, -1)
        }
        commands <- commands[-1]
    }
}


# manual set list 
root <- list()
root$a <- list()
root$"b.txt" <- 14848514
root$"c.dat" <- 8504156
root$d <- list()

root$a$e <- list()
root$a$f <- 29116
root$a$g <- 2557
root$a$"h.lst" <- 62596

root$a$e$i <- 584

root$d$j <- 4060174
root$d$"d.log" <- 8033020
root$d$"d.ext" <- 5626152
root$d$k <- 7214296
root
# ----------------------

my_reduce <- function(lis) {
    reduce(lis, .f = ~ {
        if(is.null(.y)) {
            cat(.x)
        }

        if(is.numeric(.x)) {
            num1 <- .x
        } else {
            num1 <- my_reduce(.x)
        }
        if(is.numeric(.y)) {
            num2 <- .y
        } else {
            num2 <- my_reduce(.y)
        }
        print(num1 + num2)
        return(num1 + num2)
    })
}

my_reduce(root)

lengths(root)
# length 1 but is dir:
length(root$a$e)
is.numeric(root$a$e)
# length 1 and numeric means it's a file
length(root$a$e$i)
is.numeric(root$a$e$i)

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
    print(this_list_name)
    print(size)
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
