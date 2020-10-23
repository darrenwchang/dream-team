# library(tidyverse)
# install.packages("devtools")
# library(devtools)
# #library(ellipsis)
# # install.packages("ellipsis")
# devtools::install_github(repo = "maksimhorowitz/nflscrapR")
# detach("package:namespace", unload = T, character.only = T)

# install.packages("processx")


# unloadNamespace("namespace")


# install.packages("nflfastR")
library(nflfastR)
library(tidyverse)
library(vroom)
wk1_2019 <- vroom("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\data_v2\\weekly\\2019\\week17.csv")
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

wk1_2019 <- wk1_2019 %>%
                mutate(Tm = as.factor(Tm)) %>%
                rename(team1 = Tm) %>% 
                left_join((games_2019_concat %>% filter(week == 1)), 
                    by = c("team1" = "team1"))

View(wk1_2019)
