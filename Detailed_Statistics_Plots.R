library(data.table)

#Creating Data Frames-----------------------------------------------------------
seas_detail = read.csv('raw_data//MRegularSeasonDetailedResults.csv')
copy = seas_detail
relevantseasons = c(2014,2015,2016,2017,2018,2019)
seas_detail = subset(seas_detail, Season %in% relevantseasons)

dim(copy[copy$Season == 2014 & copy$DayNum == 4,])
dim(seas_detail[seas_detail$Season == 2014 & seas_detail$DayNum == 4,])

win_stats <- data.frame(
  Season = seas_detail$Season,
  DayNum = seas_detail$DayNum,
  TeamID = seas_detail$WTeamID,
  Outcome = rep('W', nrow(seas_detail)),
  FGM = seas_detail$WFGM,
  FGA = seas_detail$WFGA,
  FGP = seas_detail$WFGM / seas_detail$WFGA,
  FGP2 = (seas_detail$WFGM - seas_detail$WFGM3) / (seas_detail$WFGA - seas_detail$WFGA3),
  FGM3 = seas_detail$WFGM3,
  FGA3 = seas_detail$WFGA3,
  FGP3 = seas_detail$WFGM3 / seas_detail$WFGA3,
  FTM = seas_detail$WFTM,
  FTA = seas_detail$WFTA,
  FTP = seas_detail$WFTM / seas_detail$WFTA,
  OR = seas_detail$WOR,
  DR = seas_detail$WDR,
  AST = seas_detail$WAst,
  TO = seas_detail$WTO,
  STL = seas_detail$WStl,
  BLK = seas_detail$WBlk,
  PF = seas_detail$WPF,
  ORP = seas_detail$WOR / (seas_detail$WOR + seas_detail$LDR),
  DRP = seas_detail$WDR / (seas_detail$WDR + seas_detail$LOR),
  POS = 0.96 * (seas_detail$WFGA + seas_detail$WTO + 0.44 * seas_detail$WFTA - seas_detail$WOR)
)

los_stats <- data.frame(
  Season = seas_detail$Season,
  DayNum = seas_detail$DayNum,
  TeamID = seas_detail$LTeamID,
  Outcome = rep('L', nrow(seas_detail)),
  FGM = seas_detail$LFGM,
  FGA = seas_detail$LFGA,
  FGP = seas_detail$LFGM / seas_detail$LFGA,
  FGP2 = (seas_detail$LFGM - seas_detail$LFGM3) / (seas_detail$LFGA - seas_detail$LFGA3),
  FGM3 = seas_detail$LFGM3,
  FGA3 = seas_detail$LFGA3,
  FGP3 = seas_detail$LFGM3 / seas_detail$LFGA3,
  FTM = seas_detail$LFTM,
  FTA = seas_detail$LFTA,
  FTP = seas_detail$LFTM / seas_detail$LFTA,
  OR = seas_detail$LOR,
  DR = seas_detail$LDR,
  AST = seas_detail$LAst,
  TO = seas_detail$LTO,
  STL = seas_detail$LStl,
  BLK = seas_detail$LBlk,
  PF = seas_detail$LPF,
  ORP = (seas_detail$LOR / (seas_detail$LOR + seas_detail$WDR)),
  DRP = seas_detail$LDR / (seas_detail$LDR + seas_detail$WOR),
  POS = 0.96 * (seas_detail$LFGA + seas_detail$LTO + 0.44 * seas_detail$LFTA - seas_detail$LOR)
)

stats_all <- rbindlist(list(win_stats, los_stats))

View(stats_all)
save(stats_all, file = "tidy_data/stats_all.rdata")

load("tidy_data/stats_all.rdata")
#Plots--------------------------------------------------------------------------
#Field Goal Percentage
pdf("statistics_plots.pdf", paper="a4")
par(mfrow = c(3, 2))

density1 = density(stats_all$FGP[stats_all$Outcome == "W"])
density2 = density(stats_all$FGP[stats_all$Outcome == "L"])

plot(density1, col = "black", 
     main = 'Regular Season Field Goal Shooting', xlab = 'Field goal %', ylab = '', 
     ylim = c(0,7), yaxt = 'n')
# Fill the area under the curve for Outcome1
polygon(c(min(density1$x), density1$x, max(density1$x)),
        c(0, density1$y, 0), col = rgb(0, 0, 1, 0.3), border = NA)
#lines for loosing density
lines(density2, col = "black")
# Fill the area under the curve for Outcome1
polygon(c(min(density2$x), density2$x, max(density2$x)),
        c(0, density2$y, 0), col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)
#adding legend
legend('topright', legend = c('W', 'L'), 
              fill = c(rgb(0, 0, 1, 0.3), rgb(0.5, 0.5, 0.5, 0.3)))

#3 point percentage
density1 = density(stats_all$FGP3[stats_all$Outcome == "W"])
density2 = density(stats_all$FGP3[stats_all$Outcome == "L"])

plot(density1, col = "black", 
     main = 'Regular Season 3 Point Shooting', xlab = '3 Point %', ylab = '', 
     ylim = c(0,4.5), yaxt = 'n')
# Fill the area under the curve for Outcome1
polygon(c(min(density1$x), density1$x, max(density1$x)),
        c(0, density1$y, 0), col = rgb(0, 0, 1, 0.3), border = NA)
#lines for loosing density
lines(density2, col = "black")
# Fill the area under the curve for Outcome1
polygon(c(min(density2$x), density2$x, max(density2$x)),
        c(0, density2$y, 0), col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)
#adding legend
legend('topright', legend = c('W', 'L'), 
       fill = c(rgb(0, 0, 1, 0.3), rgb(0.5, 0.5, 0.5, 0.3)))

#Assists
density1 = density(stats_all$AST[stats_all$Outcome == "W"])
density2 = density(stats_all$AST[stats_all$Outcome == "L"])

plot(density2, col = "black", 
     main = 'Regular Season Assists Per Game', xlab = 'No. Rebounds', 
     ylab = '', yaxt = 'n')
# Fill the area under the curve for Outcome1
polygon(c(min(density2$x), density2$x, max(density2$x)),
        c(0, density2$y, 0), col = rgb(0, 0, 1, 0.3), border = NA)
#lines for loosing density
lines(density1, col = "black")
# Fill the area under the curve for Outcome1
polygon(c(min(density1$x), density1$x, max(density1$x)),
        c(0, density1$y, 0), col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)
#adding legend
legend('topright', legend = c('W', 'L'), 
       fill = c(rgb(0, 0, 1, 0.3), rgb(0.5, 0.5, 0.5, 0.3)))

#Defensive Rebounds
density1 = density(stats_all$DR[stats_all$Outcome == "W"])
density2 = density(stats_all$DR[stats_all$Outcome == "L"])

plot(density2, col = "black", 
     main = 'Regular Season Defensive Rebounds Per Game', xlab = 'No. Rebounds', 
     ylab = '', yaxt = 'n')
# Fill the area under the curve for Outcome1
polygon(c(min(density2$x), density2$x, max(density2$x)),
        c(0, density2$y, 0), col = rgb(0, 0, 1, 0.3), border = NA)
#lines for loosing density
lines(density1, col = "black")
# Fill the area under the curve for Outcome1
polygon(c(min(density1$x), density1$x, max(density1$x)),
        c(0, density1$y, 0), col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)
#adding legend
legend('topright', legend = c('W', 'L'), 
       fill = c(rgb(0, 0, 1, 0.3), rgb(0.5, 0.5, 0.5, 0.3)))

#Turnovers
density1 = density(stats_all$TO[stats_all$Outcome == "W"])
density2 = density(stats_all$TO[stats_all$Outcome == "L"])

plot(density1, col = "black", 
     main = 'Regular Season Turnovers Per Game', xlab = 'No. Turnovers', 
     ylab = '', yaxt = 'n')
# Fill the area under the curve for Outcome1
polygon(c(min(density1$x), density1$x, max(density1$x)),
        c(0, density1$y, 0), col = rgb(0, 0, 1, 0.3), border = NA)
#lines for loosing density
lines(density2, col = "black")
# Fill the area under the curve for Outcome1
polygon(c(min(density2$x), density2$x, max(density2$x)),
        c(0, density2$y, 0), col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)
#adding legend
legend('topright', legend = c('W', 'L'), 
       fill = c(rgb(0, 0, 1, 0.3), rgb(0.5, 0.5, 0.5, 0.3)))

#Fouls
density1 = density(stats_all$PF[stats_all$Outcome == "W"])
density2 = density(stats_all$PF[stats_all$Outcome == "L"])

plot(density1, col = "black", 
     main = 'Regular Season Fouls Per Game', xlab = 'No. Fouls', 
     ylab = '', yaxt = 'n')
# Fill the area under the curve for Outcome1
polygon(c(min(density1$x), density1$x, max(density1$x)),
        c(0, density1$y, 0), col = rgb(0, 0, 1, 0.3), border = NA)
#lines for loosing density
lines(density2, col = "black")
# Fill the area under the curve for Outcome1
polygon(c(min(density2$x), density2$x, max(density2$x)),
        c(0, density2$y, 0), col = rgb(0.5, 0.5, 0.5, 0.3), border = NA)
#adding legend
legend('topright', legend = c('W', 'L'), 
       fill = c(rgb(0, 0, 1, 0.3), rgb(0.5, 0.5, 0.5, 0.3)))

dev.off()