plot_HA = function(dataframe, title = ""){
  mydf = dataframe
  score_diff = matrix(NA, nrow = nrow(mydf))
  for (i in 1:nrow(mydf))
    if (mydf$WLoc[i] == 'H') {
      score_diff[i] = mydf$score_diff[i]
    } else if (mydf$WLoc[i] == 'A'){
      score_diff[i] = -mydf$score_diff[i]
    }
  score_diff = as.data.frame(score_diff)
  
  y = table(score_diff$V1)
  y = y/nrow(score_diff)
  x = as.numeric(names(y))
  plot(x, y, 
       main = paste("", title), 
       type = 'l',
       xlab = '',
       ylab = '')
  abline(v = 0, col = 'red', lty = 'dashed')
}

plot_N = function(dataframe, title = ""){
  set.seed(170124)
  testdf = dataframe[dataframe$WLoc == 'N',]
  score_diff <- testdf$score_diff
  # Randomly select half of the scores and change their signs to negative
  indices_to_negate <- sample(seq_along(score_diff), size = length(score_diff) / 2)
  score_diff[indices_to_negate] <- -score_diff[indices_to_negate]
  score_diff = as.data.frame(score_diff)
  
  y = table(score_diff$score_diff)
  y = y/nrow(score_diff)
  x = as.numeric(names(y))
  plot(x, y, 
       main = paste("", title), 
       type = 'l',
       xlab = '',
       ylab = '')
  abline(v = 0, col = 'red', lty = 'dashed')
}