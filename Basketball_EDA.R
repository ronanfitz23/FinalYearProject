#Setup--------------------------------------------------------------------------
library(xtable)

confs_names = read.csv("raw_data/Conferences.csv")
load("tidy_data/mydf.rdata")
load("tidy_data/confs2019.rdata")

#Tables-------------------------------------------------------------------------
#first 6 rows of the data
xtable(head(mydf))
#the number of overtime games
table(mydf$NumOT)
xtable(table(mydf$NumOT))
mydf[mydf$NumOT == 4,]
#the number of games home, away, neutral
xtable(table(mydf$WLoc))

#Neutral Score Diff Graph-------------------------------------------------------
nsd = df_compact[df_compact$WLoc == 'N',]
nsd$tf = sample(c(TRUE, FALSE), nrow(nsd), replace=TRUE, prob=c(0.5, 0.5))

#random subtracts half the scores from each other
score_diff_n = matrix(NA, nrow = nrow(nsd))
for (i in 1:nrow(nsd))
  if (nsd$tf[i] == T) {
    score_diff_n[i] = nsd$WScore[i] - nsd$LScore[i]
  } else {
    score_diff_n[i] = nsd$LScore[i] - nsd$WScore[i]
  }

y = table(score_diff_n)
x = as.numeric(names(y))
plot(x, y, type = 'l')

score_diff_n[score_diff_n == 0]

#Home vs Away Graph-------------------------------------------------------
hsd = df_compact[df_compact$WLoc != 'N',]

#Home team score minus away team score
score_diff_h = matrix(NA, nrow = nrow(hsd))
for (i in 1:nrow(hsd))
  if (hsd$WLoc[i] == 'H') {
    score_diff_h[i] = hsd$WScore[i] - hsd$LScore[i]
  } else {
    score_diff_h[i] = hsd$LScore[i] - hsd$WScore[i]
  }

y = table(score_diff_h)
x = as.numeric(names(y))
plot(x, y, type = 'l')
abline(v = 0, col = 'red', lty = 'dashed')

#Conferences-------------------------------------------------------------
summary(confs)
confs$ConfAbbrev = as.factor(confs$ConfAbbrev)
summary(confs$ConfAbbrev)

confs2023 = confs[confs$Season == 2023,]
confsconts = summary(confs2023$ConfAbbrev)
confsconts
(confsconts[confsconts != 0])

confs2023 = subset(confs2023, select = -c(Season))

merged_results = merge(df_compact, confs2023, by.x = "WTeamID", by.y = "TeamID", all.x = TRUE)
colnames(merged_results)[colnames(merged_results) == 'ConfAbbrev'] = 'W_Conf'

View(merged_results)

#Now we have correct data - find the score diffs -------------------------------
conf_scored = matrix(NA, nrow = nrow(merged_results))
for (i in 1:nrow(merged_results))
  if (merged_results$WLoc[i] == 'H') {
    conf_scored[i] = merged_results$WScore[i] - merged_results$LScore[i]
  } else {
    conf_scored[i] = merged_results$LScore[i] - merged_results$WScore[i]
  }
conf_scored = as.data.frame(conf_scored)
conf_scored$W_Conf = merged_results$W_Conf

#View(conf_scored)
conf_values = levels(confs2023$ConfAbbrev)

#conf_values = c('wac', 'mwc', 'southern', 'ivy')

plotme = function(conf){
  subset_df = conf_scored[conf_scored$W_Conf == conf, ]
  #print(dim(subset_df))
  y = table(subset_df$V1)
  if(length(y)>0){
    #print(y)
    x = as.numeric(names(y))
    #print(x)
    plot(x,y, main = paste("W_Conf =", conf), 
         xlab = "Index", ylab = "Value", type = 'l')
    abline(v = 0, col = 'red', lty = 'dashed')
  }
}

par(mfrow = c(1,1))
for (conf in conf_values) {
  plotme(conf)
  readline("Press a key to continue")
}

#confs that have games since 1985
#Sunbelt, southern, southland, sec
#impossible to do some kind of merge - 



#Plots--------------------------------------------------------------------------
hist(score_diff, breaks = 10000)

y = table(score_diff)
x = as.numeric(names(y)) 
names(y) = NULL
plot(x,y , type = 'l')
max(score_diff)
