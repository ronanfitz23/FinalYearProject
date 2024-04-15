#Using Lowess and stepfunctions for moving averages-----------------------------
library(mgcv)

load("tidy_data/stats_all.rdata")
load("tidy_data/mydf.RData")

#Defing some functions----------------------------------------------------------
getstat_day <- function(stat, team, season){
  out = stats_all[stats_all$TeamID == team & stats_all$Season == season, c(stat,'DayNum')]
  return(out)
}

plot_stat_stepfun <- function(stat, team, season){
  x = getstat_day(stat,team,season)
  x = na.omit(x)
  t = x$DayNum
  x = x[[stat]]
  s = stepfuns[[paste(team,stat,season, sep = '')]]
  plot(t,x, main = paste("Stat =",stat, " Team =", team, " Season =",season),
       xlab = "Day-Number", ylab = paste(stat, sep = ''), pch = 16, cex = 0.8)
  curve(s,0,132,add=TRUE,col="red", lwd = 2)
}

#Plot For choosing F------------------------------------------------------------
stat = 'FGP'
team = 1101
season = 2014
x = getstat_day(stat,team,season)
t = x$DayNum
x = x$FGP
# t = length(x)
l1 = lowess(t,x,f=0.1)
l2 = lowess(t,x,f=0.2)
l3 = lowess(t,x,f=0.8)
s11011 = stepfun(l1$x,c(l1$y,l1$y[132]),right=TRUE)
s11012 = stepfun(l2$x,c(l2$y,l2$y[132]),right=TRUE)
s11013 = stepfun(l3$x,c(l3$y,l3$y[132]),right=TRUE)
plot(t,x, main = "Plot of FGP against day number, 1101",
     xlab = "Day-Number", ylab = "FGP", pch = 16, cex = 0.8)
#lines(l)
curve(s11011,0,132,add=TRUE,col="red", lwd = 2)
curve(s11012,0,132,add=TRUE,col='blue',lwd = 2)
curve(s11013,0,132,add=TRUE,col="orange", lwd = 2)
abline(h = s11011(132))
legend('topleft',c('f = 0.1','f = 0.2', 'f = 0.8'),col = c('red','blue','orange'), lwd = 2)

s11011(124)

#Stepfunctions------------------------------------------------------------------

#Lowess
TeamIDs = unique(unlist(list(c(mydf$LTeamID), c(mydf$WTeamID))))
Stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
Seasons = seq(2014, 2019, 1)
stepfuns = list()
for (team in TeamIDs){
  for (season in Seasons){
    for (stat in Stats){
      x = getstat_day(stat,team,season)
      x = na.omit(x)
      t = x$DayNum
      x = x[[stat]]
      if (any(is.na(x))) {
        print(paste("WARNING: Missing values in team", team, ", stat", stat, ", season", season))
        next  # Skip this iteration if x contains missing values
      }
      if (length(x) == 0) {
        print(paste("WARNING: X is Empty", team, ", stat", stat, ", season", season))
        next  # Skip this iteration if x contains missing values
      }
      l = lowess(t,x,f=0.8)
      stepfuns[[paste(team, stat, season, sep = "")]] = stepfun(l$x,c(l$y,l$y[132]),right=TRUE)
    }
  }
}

#Point to raise - at what point should we evaluate the stepfunction at??
#If its smooth enough it should not matter
plot_stat_stepfun(stat = 'FGP', team = 1306, season = 2014)
x = getstat_day('FGP', 1306, 2014)
t = x$DayNum
x = x[['FGP']]
l = lowess(t,x,f=0.2)
lines(l, lty = 'dashed', lwd = 2)

save(stepfuns, file = 'tidy_data/stepfuns_extrasmooth.rdata')

#Gam----------------------------------------------------------------------------
TeamIDs = unique(unlist(list(c(mydf$LTeamID), c(mydf$WTeamID))))
Stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
Seasons = seq(2014, 2019, 1)
gamfuns = list()
for (team in TeamIDs){
  for (season in Seasons){
    for (stat in Stats){
      x = getstat_day(stat,team,season)
      x = na.omit(x)
      t = x$DayNum
      x = x[[stat]]
      if (any(is.na(x))) {
        print(paste("WARNING: Missing values in team", team, ", stat", stat, ", season", season))
        next  # Skip this iteration if x contains missing values
      }
      if (length(x) == 0) {
        print(paste("WARNING: X is Empty", team, ", stat", stat, ", season", season))
        next  # Skip this iteration if x contains missing values
      }
      mod = gam(x ~ s(t))
      dayseq = seq(min(t), max(t), length.out = 1000)
      est = predict(mod, newdata = data.frame(t=tseq))
      gamfuns[[paste(team, stat, season, sep = "")]] = cbind(est,dayseq)
    }
    print(season)
  }
}

outs = list()
x = getstat_day('FGP', 1306, 2014)
t = x$DayNum
x = x[['FGP']]
mod = gam(x ~ s(t))
tseq = seq(min(t), max(t), length.out = 1000)
out = predict(mod, newdata = data.frame(t=tseq))
outs[['1306FGP2014']] = cbind(out,tseq)

pred = outs[['1306FGP2014']][tseq = 25]

x = getstat_day('FGP', 1306, 2014)
plot(x$DayNum, x$FGP, pch = 16, cex = 0.8, 
     main = "Stat = FGP Team = 1306 Season = 2014", 
     xlab = 'Day-Number', ylab = 'FGP')
lines(tseq,outs[['1306FGP2014']][,'out'], type = 'l', lwd = 3, col = 'blue')

library(gratia)
draw(mod, residuals=TRUE, rug=FALSE)

save(gamfuns, file = 'tidy_data/gamfuns.rdata')

#games_rolling_stats_stepfuns---------------------------------------------------
load('tidy_data/stepfuns.rdata')

games_rolling_stats_stepfuns = mydf
games_rolling_stats_stepfuns$Home_Team_id = NULL
games_rolling_stats_stepfuns$Home_Team_Conf = NULL
games_rolling_stats_stepfuns$NumOT = NULL

stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
for (stat in stats){
  games_rolling_stats_stepfuns[[paste('wr_', stat, sep = '')]] = NA
  games_rolling_stats_stepfuns[[paste('lr_', stat, sep = '')]] = NA
}

TeamIDs = unique(unlist(list(c(mydf$LTeamID), c(mydf$WTeamID))))
stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
Seasons = seq(2014, 2019, 1)
DayNums = seq(1,132)
for (season in Seasons){
  for (day in DayNums){
    for (team in TeamIDs) {
      if (nrow(games_rolling_stats_stepfuns[games_rolling_stats_stepfuns$WTeamID == team &
                                            games_rolling_stats_stepfuns$DayNum == day &
                                            games_rolling_stats_stepfuns$Season == season,]) >= 1){
        for (stat in stats){
          games_rolling_stats_stepfuns[[paste('wr_',stat,sep = "")]][
            which(games_rolling_stats_stepfuns$DayNum == day & 
                    games_rolling_stats_stepfuns$WTeamID == team &
                    games_rolling_stats_stepfuns$Season == season)] = 
            stepfuns[[paste(team, stat, season, sep = "")]](day)
        }
      }
      if (nrow(games_rolling_stats_stepfuns[games_rolling_stats_stepfuns$LTeamID == team &
                                            games_rolling_stats_stepfuns$DayNum == day &
                                            games_rolling_stats_stepfuns$Season == season,]) >= 1){
        for (stat in stats){
          games_rolling_stats_stepfuns[[paste('lr_',stat,sep = "")]][
            which(games_rolling_stats_stepfuns$DayNum == day & 
                    games_rolling_stats_stepfuns$LTeamID == team &
                    games_rolling_stats_stepfuns$Season == season)] =
            stepfuns[[paste(team, stat, season, sep = "")]](day)
          # Evaluating step-function on the day (change this??)
        }
      }
    }
    print(paste("Day Number: ", day, " - Season: ", season, sep = ""))
  }
}

save(games_rolling_stats_stepfuns, file = 'tidy_data/games_rolling_stats_stepfuns_extrasmooth.rdata')

#games_rolling_stats_gamsfuns---------------------------------------------------
load('tidy_data/mydf.rdata')
load('tidy_data/gamfuns.rdata')

games_rolling_stats_gamfuns = mydf
games_rolling_stats_gamfuns$Home_Team_id = NULL
games_rolling_stats_gamfuns$Home_Team_Conf = NULL
games_rolling_stats_gamfuns$NumOT = NULL

stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
for (stat in stats){
  games_rolling_stats_gamfuns[[paste('wr_', stat, sep = '')]] = NA
  games_rolling_stats_gamfuns[[paste('lr_', stat, sep = '')]] = NA
}

TeamIDs = unique(unlist(list(c(mydf$LTeamID), c(mydf$WTeamID))))
stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
Seasons = seq(2014, 2019, 1)
DayNums = seq(1,132)
for (season in Seasons){
  for (day in DayNums){
    for (team in TeamIDs) {
      if (nrow(games_rolling_stats_gamfuns[games_rolling_stats_gamfuns$WTeamID == team &
                                           games_rolling_stats_gamfuns$DayNum == day &
                                           games_rolling_stats_gamfuns$Season == season,]) >= 1){
        for (stat in stats){
          games_rolling_stats_gamfuns[[paste('wr_',stat,sep = "")]][
            which(games_rolling_stats_gamfuns$DayNum == day & 
                    games_rolling_stats_gamfuns$WTeamID == team &
                    games_rolling_stats_gamfuns$Season == season)] = 
            gamfuns[[paste(team, stat, season, sep = "")]][dayseq = day]
        }
      }
      if (nrow(games_rolling_stats_gamfuns[games_rolling_stats_gamfuns$LTeamID == team &
                                           games_rolling_stats_gamfuns$DayNum == day &
                                           games_rolling_stats_gamfuns$Season == season,]) >= 1){
        for (stat in stats){
          games_rolling_stats_gamfuns[[paste('lr_',stat,sep = "")]][
            which(games_rolling_stats_gamfuns$DayNum == day & 
                    games_rolling_stats_gamfuns$LTeamID == team &
                    games_rolling_stats_gamfuns$Season == season)] =
            gamfuns[[paste(team, stat, season, sep = "")]][dayseq = day]
          # Evaluating step-function on the day (change this??)
        }
      }
    }
    print(paste("Day Number: ", day, " - Season: ", season, sep = ""))
  }
}

save(games_rolling_stats_gamfuns, file = 'tidy_data/games_rolling_stats_gamsfuns.rdata')
#tourney_rolling_stats----------------------------------------------------------
load('tidy_data/mydf.rdata')
load('tidy_data/stepfuns.rdata')
load('tidy_data/games_rolling_stats_stepfuns.rdata')
TourneyResults = read.csv("raw_data/MNCAATourneyCompactResults.csv")

TourneyResults = TourneyResults[TourneyResults$Season == 2019,]
TourneyResults$rando = sample(0:1, nrow(TourneyResults), replace = TRUE)

for (i in 1:nrow(TourneyResults)){
  if (TourneyResults$rando[i] == 1){
    TourneyResults$Team1[i] = TourneyResults$WTeamID[i]
    TourneyResults$Team2[i] = TourneyResults$LTeamID[i]
    
    TourneyResults$Team1_Score[i] = TourneyResults$WScore[i]
    TourneyResults$Team2_Score[i] = TourneyResults$LScore[i]
  } else {
    TourneyResults$Team2[i] = TourneyResults$WTeamID[i]
    TourneyResults$Team1[i] = TourneyResults$LTeamID[i] 
    
    TourneyResults$Team2_Score[i] = TourneyResults$WScore[i]
    TourneyResults$Team1_Score[i] = TourneyResults$LScore[i]
  }
}

touney_rolling_stats = TourneyResults
touney_rolling_stats$WTeamID = NULL
touney_rolling_stats$LTeamID = NULL
touney_rolling_stats$rando = NULL

stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
for (stat in stats){
  touney_rolling_stats[[paste('t1_', stat, sep = '')]] = NA
  touney_rolling_stats[[paste('t2_', stat, sep = '')]] = NA
}

TeamIDs = unique(unlist(list(c(mydf$LTeamID), c(mydf$WTeamID))))
stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
season = 2019
DayNums = seq(134,154)

for (day in DayNums){
  for (team in TeamIDs) {
    if (nrow(touney_rolling_stats[touney_rolling_stats$Team1 == team &
                                  touney_rolling_stats$DayNum == day,]) >= 1){
      for (stat in stats){
        sf = stepfuns[[paste(team,stat, '2019', sep = '')]]
        x_string = capture.output(print(sf))[3]
        last_x = as.numeric(
          regmatches(
            x_string, gregexpr("\\d+", x_string))[[1]][length(
              regmatches(x_string, gregexpr("\\d+", x_string))[[1]])])
        
        touney_rolling_stats[[paste('t1_',stat,sep = "")]][
              which(touney_rolling_stats$DayNum == day & 
                      touney_rolling_stats$Team1 == team)] = 
              stepfuns[[paste(team, stat, season, sep = "")]](last_x)
      }
    }
    if (nrow(touney_rolling_stats[touney_rolling_stats$Team2 == team &
                                  touney_rolling_stats$DayNum == day,]) >= 1){
      for (stat in stats){
        sf = stepfuns[[paste(team,stat, '2019', sep = '')]]
        x_string = capture.output(print(sf))[3]
        last_x = as.numeric(
          regmatches(
            x_string, gregexpr("\\d+", x_string))[[1]][length(
              regmatches(x_string, gregexpr("\\d+", x_string))[[1]])])
        
        touney_rolling_stats[[paste('t2_',stat,sep = "")]][
              which(touney_rolling_stats$DayNum == day & 
                      touney_rolling_stats$Team2 == team)] =
              stepfuns[[paste(team, stat, season, sep = "")]](last_x)
      }
    }
  }
} 

tourney_rolling_stats = touney_rolling_stats

save(tourney_rolling_stats, file = 'tidy_data/tourney_rolling_stats.rdata')
