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
                                # team1 == "LAC" ~ "SDG",
                                # team1 == "LAR" ~ "STL",
                                TRUE ~ team1)) %>%
                mutate(team1 = case_when(team1 == "LAC" & season < 2017 ~ "SDG",
                                    team1 == "LAR" & season < 2016 ~ "STL",
                                    #team1 == "LA" && season < 2020 ~ "OAK",
                                    TRUE ~ team1)) %>% 
                mutate(team2 = case_when(team2 == "LAC" & season < 2017 ~ "SDG",
                                    team2 == "LAR" & season < 2016 ~ "STL",
                                    #team1 == "LA" && season < 2020 ~ "OAK",
                                    TRUE ~ team2)) %>% 
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
                                # team2 == "LAC" ~ "SDG",
                                # team2 == "LAR" ~ "STL",
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

pred_538 %>% filter(team1 == "LAC", season == 2009)
levels(as.factor(filter(pred_538_clean, season > 2009)$team1))


## -- Add to original data\
setwd("C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\data collection")
weekly_ff <- vroom("weekly_ff.csv")

weekly_ff_elo <- left_join(weekly_ff, pred_538_clean, by = c("team1" = "team1", "team2" = "team2",
                    "season" = "season", "team1_score" = "score1", "team2_score" = "score2"))

# # weekly_ff_elo <- weekly_ff_elo %>% 
# #                     mutate(across(.cols = c(ends_with("_cum"), ends_with("_prev")), 
# #                         ifelse(. == 0 && week == 1, NA_real_, .))) %>% 
# #                     select(week, Player, team1, team2, type, 
# #                     game_id, PPRFantasyPoints, StandardFantasyPoints, 
# #                     HalfPPRFantasyPoints, Pos, everything())

weekly_ff_elo[weekly_ff_elo$week == 1,][weekly_ff_elo[weekly_ff_elo$week == 1,] == 0] <- NA

# weekly_ff_elo <- weekly_ff_elo %>% 
#     drop_na(Pos)


weekly_ff_elo <- weekly_ff_elo %>% 
        filter(Pos %in% c("QB", "RB", "TE", "WR", "FB", "HB", "FB/RB", "K", "WR-K", "WR-R", "WR W", "WR/K", "WR/P", "WR/PR", "WR")) %>% 
        mutate(Pos = case_when(Pos %in% c("WR-K", "WR-R", "WR W", "WR/K", "WR/P", "WR/PR", "WR") ~ "WR",
                                TRUE ~ Pos),
                Pos = case_when(Pos %in% c("RB", "FB", "HB", "FB/RB") ~ "RB",
                                TRUE ~ Pos))

weekly_ff_elo <- weekly_ff_elo %>% 
                    mutate(team1_score = case_when(week == 1 ~ NA_real_, TRUE ~ team1_score),
                            team2_score = case_when(week == 1 ~ NA_real_, TRUE ~ team2_score),
                            team1_score_cum = case_when(week == 1 ~ NA_real_, TRUE ~ team1_score_cum),
                            team2_score_cum = case_when(week == 1 ~ NA_real_, TRUE ~ team2_score_cum),
                            team1_score_prev = case_when(week == 1 ~ NA_real_, TRUE ~ team1_score_prev),
                            team2_score_prev = case_when(week == 1 ~ NA_real_, TRUE ~ team2_score_prev))

vroom_write(weekly_ff_elo,
                path = "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\data collection\\weekly_ff_impute.csv",
                delim = ",")

sum(is.na(weekly_ff_elo))



# # View(weekly_ff_elo)

# # library(mice)
# # library(VIM)    
# # tempData <- mice(weekly_ff_elo,m=2,maxit=50,meth='norm', seed=0)
# # completedData <- complete(tempData,1) %>% as_tibble()

# sum(is.na(weekly_ff_elo$elo1_pre))/nrow(weekly_ff_elo)
# sum(is.na(weekly_ff_elo$elo2_pre))/nrow(weekly_ff_elo)


# ## -- imputation with h2o
library(gridExtra)
library(h2o)

## initialize h2o
h2o.init(nthreads = -1, max_mem_size = "4G")

## create original model
ff.hex <- h2o.importFile(path = "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\data collection\\weekly_ff_impute.csv",
                        destination_frame = "ff.hex")

ff.glrm <- h2o.glrm(training_frame = ff.hex, 
                    cols = 25:ncol(ff.hex), 
                    k = 5, 
                    loss = "Quadratic", 
                    # init = "SVD", 
                    # svd_method = "GramSVD",
                    regularization_x = "None", 
                    regularization_y = "None", 
                    max_iterations = 2000,
                    max_updates = 6000, 
                    min_step_size = 1e-6, 
                    seed = 0)
summary(ff.glrm)
plot(ff.glrm)

p1 <- t(ff.glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, reorder(feature, Arch1))) +
  geom_point()

p2 <- t(ff.glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, Arch2, label = feature)) +
  geom_text()

gridExtra::grid.arrange(p1, p2, nrow = 1)

## run PCA
ff_pca <- h2o.prcomp(training_frame = ff.hex[25:58], 
    pca_method = "GramSVD",
    k = ncol(ff.hex[25:58]), 
    transform = "STANDARDIZE", 
    impute_missing = TRUE,
    max_runtime_secs = 1000
)

## draw scree plot to find k = 5
data.frame(
  PC  = ff_pca@model$importance %>% seq_along,
  PVE = ff_pca@model$importance %>% .[2,] %>% unlist()
) %>%
  ggplot(aes(PC, PVE, group = 1, label = PC)) +
  geom_point() +
  geom_line() +
  geom_text(nudge_y = -.002)

## imputation with original model
ff_pred <- as_tibble(predict(ff.glrm, ff.hex))
# ff.hex[is.na(ff.hex)] <- ff.pred[is.na(ff.hex)]
ff_hex <- as_tibble(ff.hex)
names(ff_pred) <- names(ff_hex[25:58])

sum(is.na(ff_hex)) #19062

# View(ff_hex %>% 
#     mutate(team1_score = case_when(is.na(team1_score) ~ round(ff_pred$team1_score, 0),
#                                     TRUE ~ as.numeric(team1_score))))

ff.glrm_2 <- h2o.glrm(training_frame = ff.hex, 
                    cols = 25:ncol(ff.hex), 
                    k = 5, 
                    loss = "Quadratic", 
                    # init = "SVD", 
                    # svd_method = "GramSVD",
                    regularization_x = "NonNegative", 
                    regularization_y = "NonNegative", 
                    gamma_x = 0.1,
                    gamma_y = 0.1,
                    max_iterations = 6000, 
                    max_updates = 12000,
                    min_step_size = 1e-6, 
                    # transform = "STANDARDIZE",
                    seed = 0)
summary(ff.glrm_2)
plot(ff.glrm_2)

ff_pred_2 <- as_tibble(predict(ff.glrm_2, ff.hex))
# ff.hex[is.na(ff.hex)] <- ff.pred[is.na(ff.hex)]
ff_hex <- as_tibble(ff.hex)
names(ff_pred_2) <- names(ff_hex[25:58])

sum(is.na(ff_hex)) #19062

# View(ff_hex %>% 
#     mutate(team1_score = case_when(is.na(team1_score) ~ round(ff_pred_2$team1_score, 0),
#                                     TRUE ~ as.numeric(team1_score)),
#             team2_score = case_when(is.na(team2_score) ~ round(ff_pred_2$team2_score, 0),
#                 TRUE ~ as.numeric(team2_score))))

ff_hex_impute <- ff_hex %>% 
    mutate(team1_score = case_when(is.na(team1_score) ~ round(ff_pred_2$team1_score, 0),
                TRUE ~ as.numeric(team1_score)),
            team2_score = case_when(is.na(team2_score) ~ round(ff_pred_2$team2_score, 0),
                TRUE ~ as.numeric(team2_score)),
            PassingYds_cum = case_when(is.na(PassingYds_cum) ~ ff_pred_2$PassingYds_cum,
                TRUE ~ as.numeric(PassingYds_cum)),
            PassingTD_cum = case_when(is.na(PassingTD_cum) ~ round(ff_pred_2$PassingTD_cum, 0),
                TRUE ~ as.numeric(PassingTD_cum)),
            Int_cum = case_when(is.na(Int_cum) ~ round(ff_pred_2$Int_cum, 0),
                TRUE ~ as.numeric(Int_cum)),
            PassingAtt_cum = case_when(is.na(PassingAtt_cum) ~ round(ff_pred_2$PassingAtt_cum, 0),
                TRUE ~ as.numeric(PassingAtt_cum)),
            Cmp_cum = case_when(is.na(Cmp_cum) ~ round(ff_pred_2$Cmp_cum, 0),
                TRUE ~ as.numeric(Cmp_cum)),
            RushingAtt_cum = case_when(is.na(RushingAtt_cum) ~ round(ff_pred_2$RushingAtt_cum, 0),
                TRUE ~ as.numeric(RushingAtt_cum)),
            RushingYds_cum = case_when(is.na(RushingYds_cum) ~ ff_pred_2$RushingYds_cum,
                TRUE ~ as.numeric(RushingYds_cum)),
            RushingTD_cum = case_when(is.na(RushingTD_cum) ~ round(ff_pred_2$RushingTD_cum, 0),
                TRUE ~ as.numeric(RushingTD_cum)),
            Rec_cum = case_when(is.na(Rec_cum) ~ round(ff_pred_2$Rec_cum, 0),
                TRUE ~ as.numeric(Rec_cum)),
            Tgt_cum = case_when(is.na(Tgt_cum) ~ round(ff_pred_2$Tgt_cum, 0),
                TRUE ~ as.numeric(Tgt_cum)),
            ReceivingYds_cum = case_when(is.na(ReceivingYds_cum) ~ ff_pred_2$ReceivingYds_cum,
                TRUE ~ as.numeric(ReceivingYds_cum)),
            ReceivingTD_cum = case_when(is.na(ReceivingTD_cum) ~ round(ff_pred_2$ReceivingTD_cum, 0),
                TRUE ~ as.numeric(ReceivingTD_cum)),
            FL_cum = case_when(is.na(FL_cum) ~ round(ff_pred_2$FL_cum, 0),
                TRUE ~ as.numeric(FL_cum)),
            team1_score_cum = case_when(is.na(team1_score_cum) ~ round(ff_pred_2$team1_score_cum, 0),
                TRUE ~ as.numeric(team1_score_cum)),
            team2_score_cum = case_when(is.na(team2_score_cum) ~ round(ff_pred_2$team2_score_cum, 0),
                TRUE ~ as.numeric(team2_score_cum)),
            PassingYds_prev = case_when(is.na(PassingYds_prev) ~ ff_pred_2$PassingYds_prev,
                TRUE ~ as.numeric(PassingYds_prev)),
            PassingTD_prev = case_when(is.na(PassingTD_prev) ~ round(ff_pred_2$PassingTD_prev, 0),
                TRUE ~ as.numeric(PassingTD_prev)),
            Int_prev = case_when(is.na(Int_prev) ~ round(ff_pred_2$Int_prev, 0),
                TRUE ~ as.numeric(Int_prev)),
            PassingAtt_prev = case_when(is.na(PassingAtt_prev) ~ round(ff_pred_2$PassingAtt_prev, 0),
                TRUE ~ as.numeric(PassingAtt_prev)),
            Cmp_prev = case_when(is.na(Cmp_prev) ~ round(ff_pred_2$Cmp_prev, 0),
                TRUE ~ as.numeric(Cmp_prev)),
            RushingAtt_prev = case_when(is.na(RushingAtt_prev) ~ round(ff_pred_2$RushingAtt_prev, 0),
                TRUE ~ as.numeric(RushingAtt_prev)),
            RushingYds_prev = case_when(is.na(RushingYds_prev) ~ ff_pred_2$RushingYds_prev,
                TRUE ~ as.numeric(RushingYds_prev)),
            RushingTD_prev = case_when(is.na(RushingTD_prev) ~ round(ff_pred_2$RushingTD_prev, 0),
                TRUE ~ as.numeric(RushingTD_prev)),
            Rec_prev = case_when(is.na(Rec_prev) ~ round(ff_pred_2$Rec_prev, 0),
                TRUE ~ as.numeric(Rec_prev)),
            Tgt_prev = case_when(is.na(Tgt_prev) ~ round(ff_pred_2$Tgt_prev, 0),
                TRUE ~ as.numeric(Tgt_prev)),
            ReceivingYds_prev = case_when(is.na(ReceivingYds_prev) ~ ff_pred_2$ReceivingYds_prev,
                TRUE ~ as.numeric(ReceivingYds_prev)),
            ReceivingTD_prev = case_when(is.na(ReceivingTD_prev) ~ round(ff_pred_2$ReceivingTD_prev, 0),
                TRUE ~ as.numeric(ReceivingTD_prev)),
            FL_prev = case_when(is.na(FL_prev) ~ round(ff_pred_2$FL_prev, 0),
                TRUE ~ as.numeric(FL_prev)),
            team1_score_prev = case_when(is.na(team1_score_prev) ~ round(ff_pred_2$team1_score_prev, 0),
                TRUE ~ as.numeric(team1_score_prev)),
            team2_score_prev = case_when(is.na(team2_score_prev) ~ round(ff_pred_2$team2_score_prev, 0),
                TRUE ~ as.numeric(team2_score_prev))
                    )

# weekly_ff_elo <- weekly_ff_elo %>% 
#             replace_na(list(elo1_pre = "missing", 
#                         elo2_pre = "missing"))

# vroom_write(weekly_ff_elo,
#                 path = "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\data collection\\weekly_ff_elo.csv",
#                 delim = ",")

vroom_write(ff_hex_impute,
                path = "C:\\Users\\darre\\Documents\\_cornell 20-21\\orie 4741\\dream-team\\data collection\\weekly_ff_elo_impute.csv",
                delim = ",")