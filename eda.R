## -- EDA for Fantasy Football Projection Project
## ORIE 4741
## Darren Chang, Zach Schaffer, Jack Vaughan

## -- load packages
library(nflfastR)
library(tidyverse)
library(vroom)
#install.packages("corrplot", dependencies = TRUE, INSTALL_opts = '--no-lock')
library(corrplot)

setwd("C:/Users/darre/Documents/_cornell 20-21/orie 4741/dream-team")

# -- import data
weekly_ff <- vroom("weekly_ff.csv")
weekly.cor <- cor(select(Filter(is.numeric, weekly_ff), -c(season, game_id, week)))

# -- correlation plot
png(height=900, width=900, file="weekly.png", type = "cairo")
corrplot(weekly.cor,
        title = "Correlation between Features",
        method = "color",
        order = "FPC",
        # tl.pos = "td", 
        tl.cex = 0.5,
        diag = F,
        type = "lower")

dev.off()

# -- histograms
png(height=900, width=900, file="pass_yds_hist.png", type = "cairo")
hist(filter(weekly_ff, PassingYds_prev !=0)$PassingYds_prev,
        breaks = 30,
        col = "blue",
        xlab = "Previous Week Passing Yards",
        main = ""
        )

dev.off()
