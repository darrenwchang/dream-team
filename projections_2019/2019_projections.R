## -- 2019 ESPN projections

## -- Load packages
library(tidyverse)
library(vroom)

setwd("C:/Users/darre/Documents/_cornell 20-21/orie 4741/dream-team/projections_2019")

## -- Load data
data <- vroom("2019projections.csv")

data <- data %>% 
        select(-c(...1, Team)) %>% 
        rename(Pos_FF = Pos)

proj <- vroom("points_proj.csv")

proj <- proj %>% 
        rename(Week = week) %>% 
        filter(season == 2019) %>% 
        select(-season)

proj$std_points_pred[proj$std_points_pred < 0] <- 0
proj$std_points_pred <- round(proj$std_points_pred)

season <- data %>%
            inner_join(proj, by = c("Player", "Week")) %>% 
            arrange(Week, desc = T)

season <- season %>% 
            mutate(diff_espn = Actual - Proj,
                    diff_orie = Actual - std_points_pred)

hi_espn <- hist(season$diff_espn, breaks = 40)
hi_orie <- hist(season$diff_orie, breaks = 40)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

plot(hi_espn, col = c1)
plot(hi_orie, add = T, col = c2)

sum(abs(season$diff_espn), na.rm = T) #11672.51
sum(abs(season$diff_orie), na.rm = T) #10843.9

season_stats <- season %>% 
    group_by(Week) %>% 
    summarize(mean_e = mean(diff_espn),
                mean_o = mean(diff_orie),
                sum_e = sum(diff_espn),
                sum_o = sum(diff_orie),
                sum_abs_e = sum(abs(diff_espn)),
                sum_abs_o = sum(abs(diff_orie)))

season_stats

ggplot(data = season_stats,
        mapping = aes(x = Week)) +
        geom_line(aes(y = sum_abs_e)) +
        geom_line(aes(y = sum_abs_o))