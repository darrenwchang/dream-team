## -- add 538 predictions to data

## -- Packages
library(tidyverse)
library(vroom)

## -- Download Data
pred_538 <- vroom("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv")

pred_538_clean <- pred_538 %>% 
                filter(is.na(playoff)) %>% 
                filter(date > "1998-01-01") %>% 
                mutate(team1 = case_when(team1 == "GB" ~ "GNB",
                                team1 == "KC" ~ "KAN",
                                team1 == "JAC" ~ "JAX",
                                team1 == "LA" ~ "LAR",
                                team1 == "NO" ~ "NOR",
                                team1 == "NE" ~ "NWE",
                                team1 == "SD" ~ "SDG",
                                team1 == "SF" ~ "SFO",
                                team1 == "TB" ~ "TAM",
                                team1 == "WSH" ~ "WAS",
                                team1 == "LAC" ~ "SDG",
                                team1 == "LAR" ~ "STL",
                                TRUE ~ team1)) %>%
                mutate(team2 = case_when(team2 == "GB" ~ "GNB",
                                team2 == "KC" ~ "KAN",
                                team2 == "JAC" ~ "JAX",
                                team2 == "LA" ~ "LAR",
                                team2 == "NO" ~ "NOR",
                                team2 == "NE" ~ "NWE",
                                team2 == "SD" ~ "SDG",
                                team2 == "SF" ~ "SFO",
                                team2 == "TB" ~ "TAM",
                                team2 == "WSH" ~ "WAS",
                                team2 == "LAC" ~ "SDG",
                                team2 == "LAR" ~ "STL",
                                TRUE ~ team2)) %>% 
                mutate(team1 = as.factor(team1),
                        team2 = as.factor(team2)) %>% 
                select(season, team1, team2, score1, score2, elo1_pre, elo2_pre)

pred_538_clean <- pred_538_clean %>% 
                bind_rows((pred_538_clean %>% rename(team2 = team1,
                                                team1 = team2,
                                                score2 = score1,
                                                score1 = score2,
                                                elo2_pre = elo1_pre,
                                                elo1_pre = elo2_pre)))

## -- Add to original data\
setwd("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\data collection")
weekly_ff <- vroom("weekly_ff.csv")

weekly_ff_elo <- left_join(weekly_ff, pred_538_clean, by = c("team1" = "team1", "team2" = "team2",
                    "season" = "season", "team1_score" = "score1", "team2_score" = "score2"))

vroom_write(weekly_ff_elo,
                path = "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\data collection\\weekly_ff_elo.csv",
                delim = ",")