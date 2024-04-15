#-------------------------------------------------------------------------------
#4th March 2024
#FYP - Trying a GAM made of quotients of stats (from meeting notes)
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
load('tidy_data/games_rolling_stats_stepfuns.rdata')
NewGam_df = games_rolling_stats_stepfuns

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

saveRDS(NewGam_df2, 'tidy_data/NewGam_df2.rdata')

#Testing------------------------------------------------------------------------
NewGam_df2 = readRDS('tidy_data/NewGam_df2.rdata')

NewGam_df2$Team1Conf = as.factor(NewGam_df2$Team1Conf)
NewGam_df2$Team2Conf = as.factor(NewGam_df2$Team2Conf)

mod_stats = gam(t1s_minus_t2s ~ s(t1_t2_ratio_FGP) + s(t1_t2_ratio_FGP3) + s(t1_t2_ratio_FTP)
          + s(t1_t2_ratio_OR) + s(t1_t2_ratio_DR) + s(t1_t2_ratio_AST) + s(t1_t2_ratio_STL) + 
            s(t1_t2_ratio_BLK) + s(t1_t2_ratio_PF) + s(t1_t2_ratio_TO), data = NewGam_df2)
summary(mod_stats)
plot(mod_stats)
gam.check(mod_stats)
table(sign(mod_stats$fitted.values) == sign(NewGam_df2$t1s_minus_t2s))

b = getViz(mo)

mod_stats_ha = gam(t1s_minus_t2s ~ s(t1_t2_ratio_FGP) + s(t1_t2_ratio_FGP3) + s(t1_t2_ratio_FTP)
                + s(t1_t2_ratio_OR) + s(t1_t2_ratio_DR) + s(t1_t2_ratio_AST) + s(t1_t2_ratio_STL) + 
                  s(t1_t2_ratio_BLK) + s(t1_t2_ratio_PF) + s(t1_t2_ratio_TO) + 
                  Team1Loc, data = NewGam_df2)
summary(mod_stats_ha)
plot(mod_stats_ha)
gam.check(mod_stats_ha)
table(sign(mod_stats_ha$fitted.values) == sign(NewGam_df2$t1s_minus_t2s))

#Big Model - Location and Conferences-------------------------------------------
mod_stats_conf = gam(t1s_minus_t2s ~ s(t1_t2_ratio_FGP) + s(t1_t2_ratio_FTP)
                   + s(t1_t2_ratio_OR) + s(t1_t2_ratio_DR) + s(t1_t2_ratio_AST) + s(t1_t2_ratio_STL) + 
                    s(t1_t2_ratio_PF) + s(t1_t2_ratio_TO) + 
                   s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), data = NewGam_df2)
summary(mod_stats_conf)
plot(mod_stats_conf)
par(mfrow = c(2,2))
gam.check(mod_stats_conf)
table(sign(mod_stats_conf$fitted.values) == sign(NewGam_df2$t1s_minus_t2s))


b = getViz(mod_stats_conf)
plot(b)


ranef(mod_stats_conf)

ROCit_obj = rocit(sign(mod_stats_conf$fitted.values),
                  class = sign(NewGam_df2$t1s_minus_t2s))
plot(ROCit_obj)

#Plotting from meeting
library(mgcViz)
b = getViz(mod_stats_conf)
plot(b)
plot(b)

library(gratia)
summary(mod_stats_conf)
smooth_coefs(mod_stats_conf, 's(Team1Conf,Team1Loc)')
matplot(matrix(smooth_coefs(mod_stats_conf, 's(Team1Conf,Team1Loc)'), ncols = 3), type = 'l')
matplot(t(matrix(smooth_coefs(mod_stats_conf, 's(Team2Conf,Team1Loc)'), ncol = 3)), type = 'l')
smooth_estimates(mod_stats_conf)
print(smooth_estimates(mod_stats_conf))
tail(smooth_estimates(mod_stats_conf))


#Observed vs Predicted Conferences
observed = NewGam_df2[, c('t1s_minus_t2s', 'Team1Conf')]
predicted = data.frame(mod_stats_conf$fitted.values, NewGam_df2$Team1Conf)
colnames(predicted) = c('t1s_minus_t2s', 'Team1Conf')

formula_string <- paste("t1s_minus_t2s ~ Team1Conf")
formula <- as.formula(formula_string)
observed_claim <- aggregate(formula, data = observed, FUN = mean)
predicted_claim <- aggregate(formula, data = predicted, FUN = mean)

observed_claim$Type <- "Observed"
predicted_claim$Type <- "Predicted"

combined_data <- rbind(predicted_claim, observed_claim)

ggplot(data = combined_data, aes_string(x = 'Team1Conf', y = 't1s_minus_t2s', 
                                        group = 'Type', color = 'Type', linetype = 'Type')) +
  geom_line() +
  geom_point() +
  labs(title = 'Preds vs Obs',
       x = 'Team 1 Conf', y = "Score Difference") +
  theme(legend.position = 'bottom') + scale_x_discrete(guide = guide_axis(n.dodge = 2))


#Non smooth terms - which does not work for some reason-------------------------
mod2 = gam(t1s_minus_t2s ~ t1_t2_ratio_FGP + t1_t2_ratio_FGP3 + t1_t2_ratio_FTP + 
           t1_t2_ratio_OR + t1_t2_ratio_DR + t1_t2_ratio_AST + t1_t2_ratio_STL + 
            t1_t2_ratio_BLK + t1_t2_ratio_PF + t1_t2_ratio_TO + Team1Loc, 
          data = NewGam_df2)
summary(mod2)

mod2$fitted.values

hist(NewGam_df2$t1s_minus_t2s)

#BayesGAM-----------------------------------------------------------------------
bayes = bayesGAM(t1s_minus_t2s ~ t1_t2_ratio_FGP + t1_t2_ratio_FGP3 + t1_t2_ratio_FTP + 
              t1_t2_ratio_OR + t1_t2_ratio_DR + t1_t2_ratio_AST + t1_t2_ratio_STL + 
              t1_t2_ratio_BLK + t1_t2_ratio_PF + t1_t2_ratio_TO + 
              Team1Loc + Team1Conf + Team2Conf, data = NewGam_df2)
summary(bayes)

plot(bayes)

mcmc_hist_by_chain(bayes)