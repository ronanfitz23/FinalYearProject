#Imports
library(readr)

#Raw Data Files-----------------------------------------------------------------
df_compact = read.csv("raw_data/MRegularSeasonCompactResults.csv")
df_detailed = read.csv("raw_data/MRegularSeasonDetailedResults.csv")
confs_names = read.csv("raw_data/Conferences.csv")
confs = read.csv("raw_data/MTeamConferences.csv")

#confs2019----------------------------------------------------------------------
confs2019 = confs[confs$Season == 2019,]
confs2019$ConfAbbrev = as.factor(confs2019$ConfAbbrev)
confs2019 = subset(confs2019, select = -c(Season))
save(confs2019, file = "tidy_data/confs2019.rdata")

#Mydf---------------------------------------------------------------------------
mydf = df_compact
#Relevant years
relevantseasons = c(2014,2015,2016,2017,2018,2019)
mydf = subset(mydf, Season %in% relevantseasons)
#Correcting Variable years
mydf$WLoc = as.factor(mydf$WLoc)
mydf$Season = as.factor(mydf$Season)
#Resetting the row index
rownames(mydf) <- NULL
#score differences
mydf$score_diff = mydf$WScore - mydf$LScore
#home team ID (random for Neutral Games)
set.seed(170124)
mydf$tf = sample(c(TRUE, FALSE), nrow(mydf), replace=TRUE, prob=c(0.5, 0.5))
for (i in 1:nrow(mydf)){
  if (mydf$WLoc[i] == 'H')
    mydf$Home_Team_id[i] = mydf$WTeamID[i]
  else if (mydf$WLoc[i] == 'A')
    mydf$Home_Team_id[i] = mydf$LTeamID[i]
  else if (mydf$tf[i] == T)
    mydf$Home_Team_id[i] = mydf$WTeamID[i]
  else if (mydf$tf[i] == F)
    mydf$Home_Team_id[i] = mydf$LTeamID[i]
}
mydf$tf = NULL
mydf = merge(mydf, confs2019, by.x = 'Home_Team_id', by.y = 'TeamID')
colnames(mydf)[colnames(mydf) == 'ConfAbbrev'] = 'Home_Team_Conf'
#saving the data frame
save(mydf, file="tidy_data/mydf.rdata")
