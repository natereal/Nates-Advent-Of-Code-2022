library(tidyverse)

test <- tibble::tibble(villain = c("A", "B", "C"), hero = c("Y", "X", "Z"))

#' A: ROCK
#' B: PAPER
#' C: SCISSORS
#'
#' A < B
#' B < C
#' C < A

expand_grid(villain = LETTERS[1:3], hero = LETTERS[1:3]) |>
    mutate(
        result_hero = case_when(
            villain == hero ~ 0,
            villain == "A"  ~ if_else(hero == "B", 1, -1),
            villain == "B"  ~ if_else(hero == "C", 1, -1),
            villain == "C"  ~ if_else(hero == "A", 1, -1)
        ),
        result_villain = result_hero * -1,
        outcome_hero = (result_hero + 1)* 3,
        outcome_villain = (result_villain + 1)* 3,
        bonus_villain = match(villain, LETTERS),
        bonus_hero = match(hero, LETTERS),
        hero_score = bonus_hero + outcome_hero,
        villain_score = bonus_villain + outcome_villain
    ) |>
    summarise(total_hero = sum(hero_score), total_villain = sum(villain_score))

solve <- function(data, sum = TRUE) {
    data <- data |>
        mutate(
            hero = case_when(
                hero == "X" ~ "A",
                hero == "Y" ~ "B",
                hero == "Z" ~ "C"
            ),
            result_hero = case_when(
                villain == hero ~ 0,
                villain == "A"  ~ if_else(hero == "B", 1, -1),
                villain == "B"  ~ if_else(hero == "C", 1, -1),
                villain == "C"  ~ if_else(hero == "A", 1, -1)
            ),
            result_villain = result_hero * -1,
            bonus_villain = match(villain, LETTERS),
            bonus_hero = match(hero, LETTERS),
            outcome_hero = (result_hero + 1)* 3,
            outcome_villain = (result_villain + 1)* 3,
            hero_score = bonus_hero + outcome_hero,
            villain_score = bonus_villain + outcome_villain
        )

    if(sum) {
        data <- data |>
            summarise(total_hero = sum(hero_score), total_villain = sum(villain_score))
    }

    return(data)
}

match_win <- function(opp) {
    case_when(
        opp == "A" ~ "B",
        opp == "B" ~ "C",
        opp == "C" ~ "A"
    )
}

match_loss <- function(opp) {
    case_when(
        opp == "A" ~ "C",
        opp == "B" ~ "A",
        opp == "C" ~ "B"
    )
}


match_win(c("A", "A", "B"))


solve <- function(data, sum = TRUE, treat_as_strategy = FALSE) {

    if(treat_as_strategy) {
        data <- data |>
            mutate(
                strategy = hero,
                hero = case_when(
                    strategy == "X" ~ match_loss(villain),
                    strategy == "Y" ~ villain,
                    strategy == "Z" ~ match_win(villain)
                )
            )
    } else {
        data <- data |>
            mutate(hero = case_when(
                hero == "X" ~ "A",
                hero == "Y" ~ "B",
                hero == "Z" ~ "C"
            ))
    }

    data <- data |>
        mutate(
            result_hero = case_when(
                villain == hero ~ 0,
                villain == "A"  ~ if_else(hero == "B", 1, -1),
                villain == "B"  ~ if_else(hero == "C", 1, -1),
                villain == "C"  ~ if_else(hero == "A", 1, -1)
            ),
            result_villain = result_hero * -1,
            bonus_villain = match(villain, LETTERS),
            bonus_hero = match(hero, LETTERS),
            outcome_hero = (result_hero + 1)* 3,
            outcome_villain = (result_villain + 1)* 3,
            hero_score = bonus_hero + outcome_hero,
            villain_score = bonus_villain + outcome_villain
        )

    if(sum) {
        data <- data |>
            summarise(total_hero = sum(hero_score), total_villain = sum(villain_score))
    }

    return(data)
}

readr::read_delim("inst/input02.txt", " ", col_names = c("villain", "hero")) |>
    solve()

readr::read_delim("inst/input02.txt", " ", col_names = c("villain", "hero")) |>
    solve(treat_as_strategy = TRUE)

test |>
    solve(sum = FALSE)

test |>
    solve(sum = FALSE, treat_as_strategy = TRUE)
