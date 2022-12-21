library(tidyverse)

test <- c("1", "2", "-3", "3", "-2", "0", "4")
test <- as.numeric(test)

# Mod is length - 1
#' For N,
#' 0,1,2,...,N-1
modulo <- function(n, mod = 6) {
    n %% mod
}
#' Adjusts modulo for indexes starting from 1 i.e
#' For N, it is 1,2,3,...,N
adjust <- function(n, mod) {
    ((n-1) %% mod) + 1
}

insert <- function(vec, value, pos) {
    MAX_LENGTH <- length(vec)
    stopifnot(pos < MAX_LENGTH+2)
    out <- c(head(vec, pos-1), value)
    if(pos < MAX_LENGTH+1) {
        out <- c(out, vec[pos:MAX_LENGTH])
    }
    return(out)
}

x <- test
x <- readLines("inst/input20.txt") |> as.numeric()
I <- length(x)
df <- tibble::tibble(values = x, move_order = 1:I)

decrypt <- 811589153
df$values <- df$values * decrypt
TIMES <- 10
for(T in 1:TIMES) {
    for(i in 1:I) {
        val   <- df$values[df$move_order == i]
        current_index <- which(df$move_order == i)

        # To handle moving backwards, just take modulo N-1
        movement <- modulo(val, I-1)

        # Insert 1 is equivalent to Insert N
        # So the insertion point is modulo N-1, but starting from 1,
        # (R indexes start at 1)
        # The insertion point is current index + number of movements
        target_index <- adjust(current_index + movement, mod = I-1)

        df$values <- insert(df$values[-current_index], val, target_index)
        df$move_order <- insert(df$move_order[-current_index], i, target_index)
        # print(df)
    }
}
df

zero <- which(df$values == 0)
c1 <- df$values[adjust(zero + 1000, mod = I)]
c2 <- df$values[adjust(zero + 2000, mod = I)]
c3 <- df$values[adjust(zero + 3000, mod = I)]
options(scipen = 20)
sum(c1 + c2 +c3)
