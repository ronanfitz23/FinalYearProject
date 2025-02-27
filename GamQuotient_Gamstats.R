#-------------------------------------------------------------------------------
#17th March 2024
#FYP - Quotient Model Using Gams
#-------------------------------------------------------------------------------
#imports
library(mgcv)
library(bayesGAM)
library(ggplot2)
library(mgcViz)
library(ROCit)
set.seed(04032024)

#data
confs = read.csv('raw_data/MTeamConferences.csv')
load('tidy_data/games_rolling_stats_gamsfuns.rdata')
NewGam_df = games_rolling_stats_gamfuns

#Data Cleaning and Feature Engineering------------------------------------------
#List of Steps
#1. Randomly making a team 1 and a team 2
#2. Scorediff of team1 minus team2
#3. Stats should be quotients
#4. Features - Team1Location, Team1 Conf, Team2Conf

#Creating team 1 and team 2
for (i in 1:nrow(NewGam_df)){
  NewGam_df$rando[i] = sample(0:1, size = 1)
}

stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
for (i in 1:nrow(NewGam_df)){
  if (NewGam_df$rando[i] == 0){
    NewGam_df$Team1[i] = NewGam_df$WTeamID[i]
    NewGam_df$Team2[i] = NewGam_df$LTeamID[i]
    
    for (stat in stats){
      NewGam_df[[paste('t1r_', stat, sep = '')]][i] = 
        NewGam_df[[paste('wr_', stat, sep = '')]][i]
      NewGam_df[[paste('t2r_', stat, sep = '')]][i] = 
        NewGam_df[[paste('lr_', stat, sep = '')]][i]
    }
  }
  if (NewGam_df$rando[i] == 1){
    NewGam_df$Team2[i] = NewGam_df$WTeamID[i]
    NewGam_df$Team1[i] = NewGam_df$LTeamID[i]
    for (stat in stats){
      NewGam_df[[paste('t2r_', stat, sep = '')]][i] = 
        NewGam_df[[paste('wr_', stat, sep = '')]][i]
      NewGam_df[[paste('t1r_', stat, sep = '')]][i] = 
        NewGam_df[[paste('lr_', stat, sep = '')]][i]
    }
  }
}

for (stat in stats){
  NewGam_df[[paste('wr_', stat, sep = '')]] = NULL
  NewGam_df[[paste('lr_', stat, sep = '')]] = NULL
}

#Location
#if loc is Neutral team1 location is neutral
#else check rando
#radno = 0 -> winning team is team 1 -> team1 loc is win loc
#rando = 1 -> winning team is team 2 -> if wloc =  'h' team1loc = 'A' etc...
NewGam_df$WLoc = as.character(NewGam_df$WLoc)
for (i in 1:nrow(NewGam_df)){
  if (NewGam_df$WLoc[i] == 'N') {NewGam_df$Team1Loc[i] = 'N'}
  else if (NewGam_df$rando[i] == 0){
    NewGam_df$Team1Loc[i] = NewGam_df$WLoc[i]
  }
  else if (NewGam_df$rando[i] == 1) {
    if (NewGam_df$WLoc[i] == 'H') {NewGam_df$Team1Loc[i] = 'A'}
    if (NewGam_df$WLoc[i] == 'A') {NewGam_df$Team1Loc[i] = 'H'}
  }
}
NewGam_df$Team1Loc = as.factor(NewGam_df$Team1Loc)
NewGam_df$Team1 = as.factor(NewGam_df$Team1)
NewGam_df$Team2 = as.factor(NewGam_df$Team2)

#score_diff
#team1 minus team2
for (i in 1:nrow(NewGam_df)){
  if (NewGam_df$rando[i] == 0){
    NewGam_df$t1s_minus_t2s[i] = NewGam_df$score_diff[i]
  }
  if (NewGam_df$rando[i] == 1){
    NewGam_df$t1s_minus_t2s[i] = -(NewGam_df$score_diff[i])
  }
}

#Conference
confs = confs[confs$Season == 2019,]
for (i in 1:nrow(NewGam_df)){
  NewGam_df$Team1Conf[i] = confs[confs$TeamID == NewGam_df$Team1[i], "ConfAbbrev"]
  NewGam_df$Team2Conf[i] = confs[confs$TeamID == NewGam_df$Team2[i], "ConfAbbrev"]
}

#Reorder
columns_to_move = c("Season", "WTeamID", "WScore", "LTeamID", "LScore",
                    "WLoc", "score_diff", "Team1", "Team2", "Team1Conf",
                    "Team2Conf","Team1Loc", "t1s_minus_t2s")
remaining_columns = setdiff(names(NewGam_df), columns_to_move)
NewGam_df = NewGam_df[, c(columns_to_move, remaining_columns)]

NewGam_df2 = NewGam_df
NewGam_df2$WTeamID = NULL
NewGam_df2$WScore = NULL
NewGam_df2$LTeamID = NULL
NewGam_df2$LScore = NULL
NewGam_df2$WLoc = NULL
NewGam_df2$score_diff = NULL
NewGam_df2$DayNum = NULL
NewGam_df2$rando = NULL

stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
for (stat in stats){
  NewGam_df2[[paste('t1_t2_ratio_', stat, sep = '')]] = 
    NewGam_df2[[paste('t1r_', stat, sep = '')]] / NewGam_df2[[paste('t2r_', stat, sep = '')]]
  NewGam_df2[[paste('t1r_', stat, sep = '')]] = NULL
  NewGam_df2[[paste('t2r_', stat, sep = '')]] = NULL
}

#Checking for inf, nas, and nans (going to remove them - think Gams should solve this)
NewGam_df2 <- do.call(data.frame, lapply(NewGam_df2, 
                                         function(x) replace(x, is.infinite(x), NA)))

which(is.na(NewGam_df2), arr.ind = TRUE)
NewGam_df2[,13]

NewGam_df2 = na.omit(NewGam_df2)
which(is.na(NewGam_df2), arr.ind = TRUE)

Gam_model_df = NewGam_df2

saveRDS(Gam_model_df, 'tidy_data/Gam_model_df.rdata')
#Quot Gam Model ----------------------------------------------------------------
Gam_model_df = readRDS('tidy_data/Gam_model_df.rdata')
Gam_model_df$Team1Conf = as.factor(Gam_model_df$Team1Conf)
Gam_model_df$Team2Conf = as.factor(Gam_model_df$Team2Conf)

mod_quot = gam(t1s_minus_t2s ~ s(t1_t2_ratio_FGP) + s(t1_t2_ratio_FGP3) + s(t1_t2_ratio_FTP) +
                 s(t1_t2_ratio_OR) + s(t1_t2_ratio_DR) + s(t1_t2_ratio_AST) + s(t1_t2_ratio_STL) +
                 s(t1_t2_ratio_PF) + s(t1_t2_ratio_TO) + 
                 s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), 
                 data = Gam_model_df)

plot(mod_quot)
