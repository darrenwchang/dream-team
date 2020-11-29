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

proj$std_lm[proj$std_lm < 0] <- 0 # 0 for nonnegative values
proj$std_lm <- round(proj$std_lm) # round
proj$std_lasso[proj$std_lasso < 0] <- 0
proj$std_lasso <- round(proj$std_lasso)

season <- data %>%
            inner_join(proj, by = c("Player", "Week")) %>% 
            arrange(Week, desc = T)

# take differences
season <- season %>% 
        mutate(diff_espn = Actual - Proj,
                diff_lm = Actual - std_lm,
                diff_lasso = Actual - std_lasso)

# -- draw histograms for errors
hi_espn <- hist(season$diff_espn, breaks = 40,
        xlab  = "Difference from Actual (Points)",
        main  = "Histogram of 2019 NFL Season Fantasy Projections")
hi_orie_ols <- hist(season$diff_lm, breaks = 40,
        xlab  = "Difference from Actual (Points)",
        main = "Histogram of 2019 NFL Season Fantasy Projections")
hi_orie_lasso <- hist(season$diff_lasso, breaks = 40,
        xlab  = "Difference from Actual (Points)",
        main = "Histogram of 2019 NFL Season Fantasy Projections")

c1 <- adjustcolor("#30a2da", alpha.f = 0.5)
c2 <- adjustcolor("#fc4f30", alpha.f = 0.5)
c3 <- adjustcolor("#6d904f", alpha.f = 0.5)

# png(height = 500, width = 500, file = "hist_2019_ols.png", type = "cairo")

# plot(hi_espn, col = c1,
#     xlab  = "Difference from Actual (Points)",
#     main = "Histogram of 2019 NFL Season Fantasy Projection Accuracy")
# plot(hi_orie_ols, add = T, col = c2)
# # plot(hi_orie_lasso, add = T, col = c3)
# legend(15, 100, legend = c("ESPN", "Dream Team OLS"),
# #, "Dream Team Lasso"),
#         col = c("#30a2da", "#fc4f30"), 
# #"#6d904f"),
#         title = "Projection",
#         lty = 1)

# dev.off()

# png(height = 500, width = 500, file = "hist_2019_lasso.png", type = "cairo")

# plot(hi_espn, col = c1,
#     xlab  = "Difference from Actual (Points)",
#     main = "Histogram of 2019 NFL Season Fantasy Projection Accuracy")
# plot(hi_orie_lasso, add = T, col = c3)
# # plot(hi_orie_lasso, add = T, col = c3)
# legend(15, 100, legend = c("ESPN", "Dream Team Lasso"),
# #, "Dream Team Lasso"),
#         col = c("#30a2da", "#6d904f"), 
# #"#6d904f"),
#         title = "Projection",
#         lty = 1)

# dev.off()

## put the plots together on one
png(height = 500, width = 1000, file = "hist_2019_combined.png",
        type = "cairo")
par(mfcol=c(1,2))
plot(hi_espn, col = c1,
    xlab  = "Difference from Actual (Points)",
    main = "")
    #main = "Histogram of 2019 NFL Season Fantasy Projection Accuracy")
plot(hi_orie_ols, add = T, col = c2)
# plot(hi_orie_lasso, add = T, col = c3)
legend(15, 100, legend = c("ESPN", "OLS"),
#, "Dream Team Lasso"),
        col = c("#30a2da", "#fc4f30"), 
#"#6d904f"),
        title = "Projection",
        lty = 1)
plot(hi_espn, col = c1,
    xlab  = "Difference from Actual (Points)",
    main = "")
    #,
    #main = "Histogram of 2019 NFL Season Fantasy Projection Accuracy")
plot(hi_orie_lasso, add = T, col = c3)
# plot(hi_orie_lasso, add = T, col = c3)
legend(15, 100, legend = c("ESPN", "Lasso"),
#, "Dream Team Lasso"),
        col = c("#30a2da", "#6d904f"), 
#"#6d904f"),
        title = "Projection",
        lty = 1)
dev.off()


sum(abs(season$diff_espn), na.rm = T) #11672.51
sum(abs(season$diff_lm), na.rm = T) #10843.9
sum(abs(season$diff_lasso), na.rm = T) #10838.82

season_stats <- season %>% 
    group_by(Week) %>% 
    summarize(mean_e = mean(diff_espn),
                mean_o = mean(diff_lm),
                mean_l = mean(diff_lasso),
                sum_e = sum(diff_espn),
                sum_o = sum(diff_lm),
                sum_l = sum(diff_lasso),
                sum_abs_e = sum(abs(diff_espn)),
                sum_abs_o = sum(abs(diff_lm)),
                sum_abs_l = sum(abs(diff_lasso)))

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
        geom_line(aes(y = sum_abs_l, color = "green"), size = 1.2) +
        scale_color_manual(name = "Projection",
        values = c("blue" = "#30a2da", "red" = "#fc4f30",
                "green" = "#6d904f"),
        labels = c("ESPN", "OLS", "Lasso")) +
        labs(title = "2019 NFL Season Fantasy Projections") +
        ylab("Sum of Absolute Difference from Actual (Points)") +
        scale_x_continuous(breaks = seq(1, 16, 1))
dev.off()