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
weekly.cor <- cor(select(Filter(is.numeric, weekly_ff), 
        -c(season, game_id, week, StandardFantasyPoints,
        HalfPPRFantasyPoints,
        PPRFantasyPoints,
        ends_with("_cum"),
        ends_with("_prev"))))

# -- correlation plot
png(height=1100, width=1100, file="weekly.png", type = "cairo")
corrplot(weekly.cor,
        # title = "Correlation between Features",
        method = "color",
        order = "FPC",
        # tl.pos = "td", 
        # tl.cex = 1,
        tl.cex = 2,
        diag = F,
        type = "lower")

dev.off()

## -- full correlation plot
weekly.cor_full <- cor(select(Filter(is.numeric, weekly_ff), 
        -c(season, game_id, week, StandardFantasyPoints,
        HalfPPRFantasyPoints,
        PPRFantasyPoints)))

# -- correlation plot
png(height=1100, width=1100, file="weekly_full.png", type = "cairo")
corrplot(weekly.cor_full,
        # title = "Correlation between Features",
        method = "color",
        order = "FPC",
        # tl.pos = "td", 
        tl.cex = 0.5,
        # tl.cex = 2,
        diag = F,
        type = "lower")

dev.off()

# -- histograms
png(height = 1080, 
    width = 2340, 
    file = "yds_hist.png", 
    type = "cairo-png", 
    units = "px", 
    res = 300)

par(mfcol=c(2,3))
hist(filter(weekly_ff, PassingYds_prev !=0)$PassingYds_prev,
        breaks = 30,
        col = "#30a2da",
        xlab = "Previous Week Passing Yards",
        main = ""
        )
hist(filter(weekly_ff, PassingYds_cum !=0)$PassingYds_cum,
        breaks = 30,
        col = "#30a2da",
        xlab = "Cumulative Passing Yards",
        main = ""
        )
hist(filter(weekly_ff, RushingYds_prev !=0)$RushingYds_prev,
        breaks = 30,
        col = "#fc4f30",
        xlab = "Previous Week Rushing Yards",
        main = ""
        )
hist(filter(weekly_ff, RushingYds_cum !=0)$RushingYds_cum,
        breaks = 30,
        col = "#fc4f30",
        xlab = "Cumulative Rushing Yards",
        main = ""
        )
hist(filter(weekly_ff, ReceivingYds_prev !=0)$ReceivingYds_prev,
        breaks = 30,
        col = "#6d904f",
        xlab = "Previous Week Receiving Yards",
        main = ""
        )
hist(filter(weekly_ff, ReceivingYds_cum !=0)$ReceivingYds_cum,
        breaks = 30,
        col = "#6d904f",
        xlab = "Cumulative Receiving Yards",
        main = ""
        )

dev.off()
