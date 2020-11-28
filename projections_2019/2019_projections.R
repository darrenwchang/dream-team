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

hi_espn <- hist(season$diff_espn, breaks = 40,
        xlab  = "Difference from Actual (Points)",
        main  = "Histogram of 2019 NFL Season Fantasy Projections")
hi_orie <- hist(season$diff_orie, breaks = 40,
        xlab  = "Difference from Actual (Points)",
        main = "Histogram of 2019 NFL Season Fantasy Projections")

c1 <- adjustcolor("#30a2da", alpha.f = 0.5)
c2 <- adjustcolor("#fc4f30", alpha.f = 0.5)

png(height = 500, width = 500, file = "espn_2019_hist.png", type = "cairo")

plot(hi_espn, col = c1,
    xlab  = "Difference from Actual (Points)",
    main = "Histogram of 2019 NFL Season Fantasy Projection Accuracy")
plot(hi_orie, add = T, col = c2)
legend(20, 100, legend = c("ESPN", "Dream Team"),
        col = c("#30a2da", "#fc4f30"),
        title = "Projection",
        lty = 1)

dev.off()

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


theme_set(theme_bw())
theme_update(text = element_text(size=12),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
strip.background = element_blank()
)

png(height=428, width=534, file="espn_2019.png", type = "cairo")
ggplot(data = season_stats,
        mapping = aes(x = Week)) +
        geom_line(aes(y = sum_abs_e, color = "blue"), size = 1.2) +
        geom_line(aes(y = sum_abs_o, color = "red"), size = 1.2) +
        scale_color_manual(name = "Projection",
        values = c("blue" = "#30a2da", "red" = "#fc4f30"),
        labels = c("ESPN", "Dream Team")) +
        labs(title = "2019 NFL Season Fantasy Projections") +
        ylab("Sum of Absolute Difference from Actual (Points)") +
        scale_x_continuous(breaks = seq(1, 16, 1))
dev.off()