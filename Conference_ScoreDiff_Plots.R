#Setup--------------------------------------------------------------------------
confs_names = read.csv("raw_data/Conferences.csv")
load("tidy_data/mydf.rdata")
load("tidy_data/confs2019.rdata")

#General Plots------------------------------------------------------------------
plot_HA(mydf)
plot_N(mydf)

#Plotting 32 Plots - score difference by conference-----------------------------
conf_values = levels(confs2019$ConfAbbrev) #32 Conferences in the 2019 Season

#Need to think about getting this to plot neutral games

plotme = function(conf){
  score_diff = matrix(NA, nrow = nrow(mydf))
  for (i in 1:nrow(mydf))
    if (mydf$WLoc[i] == 'H') {
      score_diff[i] = mydf$score_diff[i]
    } else if (mydf$WLoc[i] == 'A'){
      score_diff[i] = -mydf$score_diff[i]
    }
  score_diff = as.data.frame(score_diff)
  score_diff$Home_Team_Conf = mydf$Home_Team_Conf
  subset_df = score_diff[score_diff$Home_Team_Conf == conf,]
  y = table(subset_df$V1)
  y = y#/nrow(subset_df)
  if(length(y)>0){
    x = as.numeric(names(y))
    plot(x, y, 
         main = paste("", 
                      confs_names$Description[confs_names$ConfAbbrev == conf]),
         type = 'l',
         xlab = '',
         ylab = '',
         xlim = c(-40,40))
         #ylim = c(0,.06))
    abline(v = 0, col = 'red', lty = 'dashed')
  }
}

#Loop for Viewing plots one by one
par(mfrow = c(1,1))
for (conf in conf_values) {
  plotme(conf)
  readline("Press a key to continue")
}

#Plot for Report 4x4 plots
conf_values = conf_values[conf_values != "ind"]
conf_values1 = conf_values[1:16]
conf_values2 = conf_values[17:32]

par(mfrow = c(4,4))
for (conf in conf_values1) {
  plotme(conf)
}

par(mfrow = c(4,4))
for (conf in conf_values2) {
  plotme(conf)
}


#This one still needs to be fixed ---
#Plotting all 32 on one plot using rbg and alpha -------------------------------
testconf = c('big_east')

#Function for adding lines
linesme = function(conf){
  subset_df = score_diff[score_diff$Home_Team_Conf == conf, ]
  y = table(subset_df$V1)
  y = y/nrow(subset_df)
  if(length(y)>0){
    x = as.numeric(names(y))
    #lines(x,y, main = paste("Home Team Conference =", conf), type = 'l')
    polygon(x,y, col = rgb(0, 0, 1, alpha = 0.3), border = NA)
    abline(v = 0, col = 'red', lty = 'dashed')
  }
}

#Creating plot
par(mfrow = c(1,1))
plotme("big_ten")
for (conf in conf_values) {
  linesme(conf)
}

plotme(testconf)
linesme(testconf)
linesme('big_west')
