# install.packages("nflfastR")
library(nflfastR)
library(tidyverse)
library(vroom)

setwd("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\data_v2\\weekly\\2019")

temp <- list.files(pattern="*.csv", recursive = T)
weekly_ff <- lapply(temp, vroom)
weekly_ff <- weekly_ff %>% 
                bind_rows(.id = "column_label")
# wk1_2019 <- vroom("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\data_v2\\weekly\\2019\\week1.csv")

games_2019 <- vroom("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\nflscrapR-data\\games_data\\regular_season\\reg_games_2019.csv")

games_2019 <- games_2019 %>%
                mutate(home_team = case_when(home_team == "GB" ~ "GNB",
                                home_team == "KC" ~ "KAN",
                                home_team == "LA" ~ "LAR",
                                home_team == "NO" ~ "NOR",
                                home_team == "NE" ~ "NWE",
                                home_team == "SF" ~ "SFO",
                                home_team == "TB" ~ "TAM",
                                TRUE ~ home_team)) %>%
                mutate(away_team = case_when(away_team == "GB" ~ "GNB",
                                away_team == "KC" ~ "KAN",
                                away_team == "LA" ~ "LAR",
                                away_team == "NO" ~ "NOR",
                                away_team == "NE" ~ "NWE",
                                away_team == "SF" ~ "SFO",
                                away_team == "TB" ~ "TAM",
                                TRUE ~ away_team)) %>%
                rename(team1 = home_team,
                        team2 = away_team,
                        team1_score = home_score,
                        team2_score = away_score) %>% 
                mutate(team1 = as.factor(team1),
                        team2 = as.factor(team2)) %>% 
                select(-c(game_url, state_of_game))

games_2019_concat <- games_2019 %>% 
                        bind_rows((games_2019 %>% rename(team2 = team1,
                                team1 = team2,
                                team2_score = team1_score,
                                team1_score = team2_score)))

# wk1_2019 <- wk1_2019 %>%
#                 mutate(Tm = as.factor(Tm)) %>%
#                 rename(team1 = Tm) %>% 
#                 left_join((games_2019_concat %>% filter(week == 1)), 
#                     by = c("team1" = "team1"))

weekly_ff_2 <- weekly_ff %>%
                mutate(Tm = as.factor(Tm),
                        column_label = as.numeric(column_label)) %>%
                rename(team1 = Tm,
                    week = column_label) %>% 
                left_join(games_2019_concat, 
                    by = c("team1" = "team1", 
                    "week" = "week"))

weekly_ff_3 <- weekly_ff_2 %>% 
    group_by(Player) %>% 
    mutate(across(.cols = -c(week,
                    Pos, 
                    team1, 
                    PPRFantasyPoints, 
                    StandardFantasyPoints,
                    HalfPPRFantasyPoints,
                    type,
                    game_id,
                    team2,
                    season),
            cummean,
            .names = "{col}_cum"),
        across(.cols = -c(week,
                    Pos, 
                    team1, 
                    PPRFantasyPoints, 
                    StandardFantasyPoints,
                    HalfPPRFantasyPoints,
                    type,
                    game_id,
                    team2,
                    season,
                    ends_with("_cum")),
            lag,
            n = 1L,
            .names = "{col}_prev"),
        across(.cols = -c(week,
                    Pos, 
                    team1, 
                    PPRFantasyPoints, 
                    StandardFantasyPoints,
                    HalfPPRFantasyPoints,
                    type,
                    game_id,
                    team2,
                    season),
            replace_na,
            0)) 

vroom_write(weekly_ff_3,
            path = "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\weekly_ff_3.csv",
            delim = ",")


            
    #         %>% 
    # filter_at(vars(c(week,
    #                 Pos, 
    #                 team1, 
    #                 PPRFantasyPoints, 
    #                 StandardFantasyPoints,
    #                 HalfPPRFantasyPoints,
    #                 type,
    #                 game_id,
    #                 team2,
    #                 season)),
    #                 any_vars(is.na(.)))


# standings <- read_csv("http://www.habitatring.com/standings.csv")
# games <- read_csv("http://www.habitatring.com/games.csv")