#-------------------------------------------------------------------------------
#11th March 2024
#FYP - Trying a GAM made of differencing the stats
#-------------------------------------------------------------------------------
#imports
library(mgcv)
library(bayesGAM)
library(ggplot2)
library(mgcViz)
library(ROCit)
library(cvms)
library(gratia)
library(tidyverse)
library(reshape2)
library(rjags)
library(dplyr)
set.seed(04032024)

#data
confs = read.csv('raw_data/MTeamConferences.csv')
confs_names = read.csv("raw_data/Conferences.csv")
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

NewGam_df3 = NewGam_df
NewGam_df3$WTeamID = NULL
NewGam_df3$WScore = NULL
NewGam_df3$LTeamID = NULL
NewGam_df3$LScore = NULL
NewGam_df3$WLoc = NULL
NewGam_df3$score_diff = NULL
NewGam_df3$DayNum = NULL
NewGam_df3$rando = NULL

stats = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 'BLK', 'PF', 'TO')
for (stat in stats){
  NewGam_df3[[paste('t1_t2_diff_', stat, sep = '')]] = 
    NewGam_df3[[paste('t1r_', stat, sep = '')]] - NewGam_df3[[paste('t2r_', stat, sep = '')]]
  NewGam_df3[[paste('t1r_', stat, sep = '')]] = NULL
  NewGam_df3[[paste('t2r_', stat, sep = '')]] = NULL
}

#Checking for inf, nas, and nans (going to remove them - think Gams should solve this)
NewGam_df3 <- do.call(data.frame, lapply(NewGam_df3, 
                                         function(x) replace(x, is.infinite(x), NA)))

which(is.na(NewGam_df3), arr.ind = TRUE)
NewGam_df3[,13]

NewGam_df3 = na.omit(NewGam_df3)
which(is.na(NewGam_df3), arr.ind = TRUE)

NewGam_df3$Team1Conf = as.factor(NewGam_df3$Team1Conf)
NewGam_df3$Team2Conf = as.factor(NewGam_df3$Team2Conf)

saveRDS(NewGam_df3, 'tidy_data/NewGam_df3.rdata')

#Building a model---------------------------------------------------------------
NewGam_df3 = readRDS('tidy_data/NewGam_df3.rdata')

mod_diff = gam(t1s_minus_t2s ~ s(t1_t2_diff_FGP) + s(t1_t2_diff_FTP) + s(t1_t2_diff_FGP3) +
                 s(t1_t2_diff_OR) + s(t1_t2_diff_DR) + s(t1_t2_diff_AST) + 
                 s(t1_t2_diff_STL) + s(t1_t2_diff_BLK) + 
                 s(t1_t2_diff_PF) + s(t1_t2_diff_TO) + 
                 s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), 
               data = NewGam_df3)

colnames(NewGam_df3)[8:17] = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 
                               'BLK', 'PF', 'TO')

mod_diff = gam(t1s_minus_t2s ~ s(FGP) + s(FTP) + s(FGP3) + s(OR) + s(DR) + s(AST) + 
                 s(STL) + s(BLK) + s(PF) + s(TO) + 
                 s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), 
               data = NewGam_df3)

summary(mod_diff)
plot(mod_diff)

#pdf('gam_check.pdf')
par(mfrow = c(2,2))
gam.check(mod_diff)
#dev.off()

pdf('draw_model.pdf')
draw(mod_diff)
dev.off()

bayes = qgam(t1s_minus_t2s ~ s(t1_t2_diff_FGP) + s(t1_t2_diff_FTP)
         + s(t1_t2_diff_OR) + s(t1_t2_diff_DR) + s(t1_t2_diff_AST) + s(t1_t2_diff_STL) + 
           s(t1_t2_diff_PF) + s(t1_t2_diff_TO) + 
           s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), 
         data = NewGam_df3, qu = 0.9)

library(rstanarm)
stan = stan_gamm4(t1s_minus_t2s ~ s(t1_t2_diff_FGP) + s(t1_t2_diff_FTP)
           + s(t1_t2_diff_OR) + s(t1_t2_diff_DR) + s(t1_t2_diff_AST) + s(t1_t2_diff_STL) + 
             s(t1_t2_diff_PF) + s(t1_t2_diff_TO) + 
             s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), 
           data = NewGam_df3)

saveRDS(stan, 'models/stan.rds')

plot_nonlinear(stan, smooths = "s(t1_t2_diff_FGP)")

jags.file = ('models/jagam.jags')

bayes = jagam(t1s_minus_t2s ~ s(t1_t2_diff_FGP) + s(t1_t2_diff_FTP)
               + s(t1_t2_diff_OR) + s(t1_t2_diff_DR) + s(t1_t2_diff_AST) + s(t1_t2_diff_STL) + 
                 s(t1_t2_diff_PF) + s(t1_t2_diff_TO) + 
                 s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), 
               data = NewGam_df3, file = jags.file)

jm = jags.model(jags.file, data = bayes$jags.data, inits = bayes$jags.ini, n.chains = 1)

list.samplers(jm)

sam = jags.samples(jm, c('b', 'rho', 'scale'), n.iter = 10000, thin = 10)
saveRDS(sam, 'models/sam.rds')

#Confusion Matrix---------------------------------------------------------------
pred = actual = rep(NA, nrow(NewGam_df3))

for (i in 1:(nrow(NewGam_df3))){
  if (sign(mod_diff$fitted.values[i]) == 1){
    pred[i] = 'Team1'
  }
  if (sign(mod_diff$fitted.values[i]) == -1){
    pred[i] = 'Team2'
  }
  if (sign(NewGam_df3$t1s_minus_t2s[i]) == 1){
    actual[i] = 'Team1'
  }
  if (sign(NewGam_df3$t1s_minus_t2s[i]) == -1){
    actual[i] = 'Team2'
  }
}

table(pred,actual)
confm = confusion_matrix(actual, pred)

library(cvms)
plot_confusion_matrix(confm)

#Plots--------------------------------------------------------------------------
#Model Plots
b = getViz(mod_diff)
plot(b)

#Home - Away - Neutral by conference
matrix = t(matrix(smooth_coefs(mod_diff, 's(Team1Conf,Team1Loc)'), ncol = 3))
row.names(matrix) = c('Away', 'Home', 'Neutral')
matplot(matrix, type = 'l')

data <- (matrix)

confs = levels(NewGam_df3$Team1Conf)
loc = c('Away', 'Home', 'Neutral')

data_long = expand.grid(confs, loc)
vec = c(t(data))
data_long$Val = vec
colnames(data_long) = c('Conference', 'Location', 'ScoreDiff')

ggplot(data_long, aes(x = Location, y = ScoreDiff, 
                      group = Conference, colour = Conference)) +
  geom_line() +
  geom_point() + 
  labs(title = 'Effect of Team1Conf and Team1Loc on Score Difference') +
  theme_minimal()

test = dcast(data_long, Conference ~ Location)
test[test$Away > test$Home, ]

#Table for comparison
# Calculate the average ScoreDiff by conference
average_score_diff <- data_long %>%
  group_by(Conference) %>%
  summarise(Average_ScoreDiff = mean(ScoreDiff)) %>%
  arrange(desc(Average_ScoreDiff))

colnames(average_score_diff) = c('Conference', 'Average Score Difference')
colnames(confs_names) = c('Conference', 'Description')
merged = merge(average_score_diff, confs_names, by ='Conference') 
merged = merged[,c(3,1,2)]
colnames(merged) = c('Conference', 'Conference Abbreviation', 'sd')

merged <- merged %>% arrange(desc(sd))

colnames(merged) = c('Conference', 'Conference Abbreviation', 'Average Score Difference')
library(xtable)
print(xtable(merged), include.rownames=FALSE)


#standard errors
outs = (confint(mod_diff, 's(Team1Conf,Team1Loc)', type = "confidence"))
outs = outs[outs$Team1Loc == 'H',]
outs$smooth = outs$type = outs$by = outs$crit = outs$Team1Loc = NULL
colnames(outs)[1] = c('Conference')
outs = merge(outs, confs_names, by ='Conference')
outs = outs[,c(6,1,2,3,4,5)]
outs <- outs %>% arrange(desc(est))
colnames(outs) = c('Conference', 'Abbreviation', 'Score Diff', 'SE', 'Upper', 'Lower')

print(xtable(outs), include.rownames=FALSE)

#Score differences table--------------------------------------------------------
library(xtable)
table(abs(NewGam_df3$t1s_minus_t2s) > 20)

#Getting Probabilities----------------------------------------------------------
fit = predict.gam(mod_diff, NewGam_df3, se.fit = TRUE)

actual = rep(NA, nrow(NewGam_df3))

for (i in 1:nrow(NewGam_df3)){
  if (sign(NewGam_df3$t1s_minus_t2s[i]) == 1){
    actual[i] = 1
  }
  if (sign(NewGam_df3$t1s_minus_t2s[i]) == -1){
    actual[i] = 0
  }
}

#Example - turn this into a plot maybe? 
fit[[1]][3]
fit[[2]][3]
pnorm(0, 0.79644, 0.6315, lower.tail = FALSE)

fit.df = data.frame(fit)

fit.df$prob = pnorm(0, fit.df$fit, fit.df$se.fit, lower.tail = FALSE)

library(pROC)
roc = roc(actual, fit.df$prob)
best = coords(roc, "best")
plot(roc, main = 'ROC Curve')
points(best$specificity, best$sensitivity, pch = 16, col = 'red')
text(best$specificity + 0.1, best$sensitivity + 0.07, 'Optimal Threshold')

fit.df$thres0.5 = ifelse(fit.df$prob > 0.5, 1, 0)
fit.df$thres0.73 = ifelse(fit.df$prob > 0.734, 1, 0)
fit.df$actual = actual

tab0.5 = table(fit.df$thres0.5, fit.df$actual)
tab0.73 = table(fit.df$thres0.73, fit.df$actual)

sum(diag(tab0.5)) / sum(tab0.5)
sum(diag(tab0.73)) / sum(tab0.73)

confm0.5 = confusion_matrix(fit.df$actual, fit.df$thres0.5)
plot_confusion_matrix(confm0.5)

#New correct confusion matrix
fit.df$actual = ifelse(fit.df$actual == 1, c('Team 1'), c('Team 2'))
fit.df$thres0.73 = ifelse(fit.df$thres0.73 == 1, c('Team 1'), c('Team 2'))
confm0.73 = confusion_matrix(fit.df$actual, fit.df$thres0.73)
plot_confusion_matrix(confm0.73)

table(fit.df$prob > 0.99 | fit.df$prob < 0.01)
