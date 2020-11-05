## -- Data scraping for Fantasy Football Projection Project
## ORIE 4741
## Darren Chang, Zach Schaffer, Jack Vaughan
# install.packages("nflfastR")
library(nflfastR)
library(tidyverse)
library(vroom)
library(gtools)

seasons <- c(seq("2009", "2019", by = 1))

scrape_weekly_nfl <- function(seasons, path_weekly, path_games) {

        for (i in seasons){
        setwd(paste0(path_weekly, "\\",i))
        temp <- list.files(pattern="*.csv", recursive = T)
        temp <- mixedsort(sort(temp))
        weekly_ff <- lapply(temp, vroom)

        weekly_ff <- weekly_ff %>% 
                bind_rows(.id = "column_label")

        games <- vroom(paste0(path_games, "\\", paste0("reg_games_", i, ".csv")))
        games <- games %>%
                mutate(home_team = case_when(home_team == "GB" ~ "GNB",
                                home_team == "KC" ~ "KAN",
                                home_team == "JAC" ~ "JAX",
                                home_team == "LA" ~ "LAR",
                                home_team == "NO" ~ "NOR",
                                home_team == "NE" ~ "NWE",
                                home_team == "SD" ~ "SDG",
                                home_team == "SF" ~ "SFO",
                                home_team == "TB" ~ "TAM",
                                TRUE ~ home_team)) %>%
                mutate(away_team = case_when(away_team == "GB" ~ "GNB",
                                away_team == "KC" ~ "KAN",
                                away_team == "JAC" ~ "JAX",
                                away_team == "LA" ~ "LAR",
                                away_team == "NO" ~ "NOR",
                                away_team == "NE" ~ "NWE",
                                away_team == "SD" ~ "SDG",
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

        games <- games %>% 
                bind_rows((games %>% rename(team2 = team1,
                                team1 = team2,
                                team2_score = team1_score,
                                team1_score = team2_score)))
                
        weekly_ff <- weekly_ff %>%
                mutate(Tm = as.factor(Tm),
                column_label = as.numeric(column_label)) %>%
                rename(team1 = Tm,
                        week = column_label) %>% 
                left_join(games, 
                        by = c("team1" = "team1", 
                                "week" = "week")) %>%         
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
        assign(paste0("weekly_ff", i), weekly_ff,
                        envir = .GlobalEnv)
        }
}

# test <- vroom("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\nflscrapR-data\\games_data\\regular_season\\reg_games_2009.csv")
# levels(as.factor(test$home_team))

# test_wk1 <- vroom("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\data_v2\\weekly\\2009\\week1.csv")
# levels(as.factor(test_wk1$Tm))


# setwd(paste0("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\data_v2\\weekly",
#                 "\\", "2009"))
# temp <- list.files(pattern="*.csv", recursive = T)
# temp <- mixedsort(sort(temp))

# # install.packages("gtools", dependencies = TRUE, INSTALL_opts = '--no-lock')
# weekly_ff <- lapply(temp, vroom)

# weekly_ff <- weekly_ff %>% 
#         bind_rows(.id = "column_label")

scrape_weekly_nfl(seasons, 
                "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\data_v2\\weekly",
                "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\nflscrapR-data\\games_data\\regular_season")

seasons_ff <- paste0("weekly_ff", seasons)

weekly_ff <- bind_rows(mget(seasons_ff))

vroom_write(weekly_ff,
                path = "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\weekly_ff.csv",
                delim = ",")
                
# standings <- read_csv("http://www.habitatring.com/standings.csv")
# games <- read_csv("http://www.habitatring.com/games.csv")