#-------------------------------------------------------------------------------
#17th March 2024
#FYP - Cross Validation
#-------------------------------------------------------------------------------
#imports
library(mgcv)
library(tidyverse)
library(bayesGAM)
library(ggplot2)
library(mgcViz)
library(ROCit)
set.seed(04032024)

#10 Fold cv
NewGam_df2 = readRDS('tidy_data/NewGam_df2.rdata')
NewGam_df2$Team1Conf = as.factor(NewGam_df2$Team1Conf)
NewGam_df2$Team2Conf = as.factor(NewGam_df2$Team2Conf)

NewGam_df3 = readRDS('tidy_data/NewGam_df3.rdata')
colnames(NewGam_df3)[8:17] = c('FGP', 'FGP3', 'FTP', 'OR', 'DR', 'AST', 'STL', 
                               'BLK', 'PF', 'TO')

mod_diff = gam(t1s_minus_t2s ~ s(FGP) + s(FTP) + s(FGP3) + s(OR) + s(DR) + s(AST) + 
                 s(STL) + s(BLK) + s(PF) + s(TO) + 
                 s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), 
               data = NewGam_df3)

Gam_model_df = readRDS('tidy_data/Gam_model_df.rdata')
Gam_model_df$Team1Conf = as.factor(Gam_model_df$Team1Conf)
Gam_model_df$Team2Conf = as.factor(Gam_model_df$Team2Conf)

Gam_model_diff_df = readRDS('tidy_data/Gam_model_diff_df.rdata')

K = 10
N = (nrow(NewGam_df2))
folds = cut(1:N, K, labels=FALSE)
test_rmse_quote = train_rmse_quote = test_rmse_diff = train_rmse_diff = numeric(K)
test_rmse_quote_gam = train_rmse_quote_gam = test_rmse_diff_gam = train_rmse_diff_gam = numeric(K)

#Cross-Validation
for(k in 1:K){ # 10-fold CV loop
  
  #split into train and test samples:
  i.train	= which(folds!=k)
  dat.train_quot = NewGam_df2[i.train, ]
  dat.train_diff = NewGam_df3[i.train,]
  dat.train_quot_gam = Gam_model_df[i.train,]
  dat.train_diff_gam = Gam_model_diff_df[i.train,]
  
  dat.test_quot = NewGam_df2[-i.train, ]
  dat.test_diff = NewGam_df3[-i.train, ]
  dat.test_quot_gam = Gam_model_df[-i.train,]
  dat.test_diff_gam = Gam_model_diff_df[-i.train,]
  
  #Quotient Model Using Step fns
  mod_quot = gam(t1s_minus_t2s ~ s(t1_t2_ratio_FGP) + s(t1_t2_ratio_FGP3) + s(t1_t2_ratio_FTP) +
                   s(t1_t2_ratio_OR) + s(t1_t2_ratio_DR) + s(t1_t2_ratio_AST) + s(t1_t2_ratio_STL) +
                   s(t1_t2_ratio_PF) + s(t1_t2_ratio_TO) +
                   s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'),
                   data = dat.train_quot)
  
  p = predict(mod_quot, se.fit = TRUE)
  p2 = predict(mod_quot, newdata = dat.test_quot, se.fit = TRUE)
  pw = 1/p$se.fit^2
  pw = pw / sum(pw)
  p2w = 1/p2$se.fit^2
  p2w = p2w / sum(p2w)
  
  train_rmse_quote[k] = sqrt(sum((
    mod_quot$fitted.values - dat.train_quot$t1s_minus_t2s)^2 * pw))
  test_rmse_quote[k] = sqrt(sum((
    predict(mod_quot, newdata = dat.test_quot) - dat.test_quot$t1s_minus_t2s)^2 * p2w))

  #Diff Model Using Step fns
  mod_diff = gam(t1s_minus_t2s ~ s(FGP) + s(FTP) + s(FGP3) + s(OR) + s(DR) + s(AST) + 
                   s(STL) + s(BLK) + s(PF) + s(TO) + 
                   s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'),
                 data = dat.train_diff)
  
  p = predict(mod_diff, se.fit = TRUE)
  p2 = predict(mod_diff, newdata = dat.test_diff, se.fit = TRUE)
  pw = 1/p$se.fit^2
  pw = pw / sum(pw)
  p2w = 1/p2$se.fit^2
  p2w = p2w / sum(p2w)
  
  train_rmse_diff[k] = sqrt(sum((
    mod_diff$fitted.values - dat.train_diff$t1s_minus_t2s)^2 * pw))
  test_rmse_diff[k] = sqrt(sum((
    predict(mod_diff, newdata = dat.test_diff) - dat.test_diff$t1s_minus_t2s)^2 * p2w))

  #Quotient Model Using Gam_Stats
  mod_quot_gam = gam(t1s_minus_t2s ~ s(t1_t2_ratio_FGP) + s(t1_t2_ratio_FGP3) + s(t1_t2_ratio_FTP) +
                   s(t1_t2_ratio_OR) + s(t1_t2_ratio_DR) + s(t1_t2_ratio_AST) + s(t1_t2_ratio_STL) +
                   s(t1_t2_ratio_PF) + s(t1_t2_ratio_TO) +
                   s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'),
                 data = dat.train_quot_gam)
  
  p = predict(mod_quot_gam, se.fit = TRUE)
  p2 = predict(mod_quot_gam, newdata = dat.test_quot_gam, se.fit = TRUE)
  pw = 1/p$se.fit^2
  pw = pw / sum(pw)
  p2w = 1/p2$se.fit^2
  p2w = p2w / sum(p2w)
  
  train_rmse_quote_gam[k] = sqrt(sum((
    mod_quot_gam$fitted.values - dat.train_quot_gam$t1s_minus_t2s)^2 * pw))
  test_rmse_quote_gam[k] = sqrt(sum((
    predict(mod_quot_gam, newdata = dat.test_quot_gam) - dat.test_quot_gam$t1s_minus_t2s)^2 * p2w))
  
  #Diff Model Using Gam_Stats
  mod_diff_gamm = gam(t1s_minus_t2s ~ s(t1_t2_diff_FGP) + s(t1_t2_diff_FTP)
                 + s(t1_t2_diff_OR) + s(t1_t2_diff_DR) + s(t1_t2_diff_AST) + s(t1_t2_diff_STL) + 
                   s(t1_t2_diff_PF) + s(t1_t2_diff_TO) + 
                   s(Team1Conf, Team1Loc, bs= 're') + s(Team2Conf, Team1Loc, bs = 're'), 
                 data = dat.train_diff_gam)
  
  p = predict(mod_diff_gamm, se.fit = TRUE)
  p2 = predict(mod_diff_gamm, newdata = dat.test_diff_gam, se.fit = TRUE)
  pw = 1/p$se.fit^2
  pw = pw / sum(pw)
  p2w = 1/p2$se.fit^2
  p2w = p2w / sum(p2w)
  
  train_rmse_diff_gam[k] = sqrt(sum((
    mod_diff_gamm$fitted.values - dat.train_diff_gam$t1s_minus_t2s)^2 * pw))
  test_rmse_diff_gam[k] = sqrt(sum((
    predict(mod_diff_gamm, newdata = dat.test_diff_gam) - dat.test_diff_gam$t1s_minus_t2s)^2 * p2w))
  
  print(paste('This is loop ', k, sep = ''))
}

cv_output = data.frame(train_rmse_quote, test_rmse_quote, train_rmse_diff, test_rmse_diff, 
                          train_rmse_quote_gam, test_rmse_quote_gam, train_rmse_diff_gam, 
                          test_rmse_diff_gam)

data_tidy <- cv_output %>%
  pivot_longer(cols = everything(), names_to = c("model_type", ".value"), names_sep = "_")

boxplot(train_rmse_quote, test_rmse_quote, train_rmse_diff, test_rmse_diff, 
        train_rmse_quote_gam, test_rmse_quote_gam, train_rmse_diff_gam, test_rmse_diff_gam,
        names = c('Train Stepfn Quot', 'Test Stepfn Quote',
                  'Train Stepfn Diff', 'Test Stepfn Diff',
                  'Train Gam Quot', 'Test Gam Quote',
                  'Train Gam Diff', 'Test Gam Diff'), 
        main = '10 Fold CV Train and Test Errors', ylab = 'RMSE')

#Nicer plot
long_data <- gather(cv_output, key, value)

# Add a new column for the main level
long_data$main_level <- ifelse(grepl("gam", long_data$key), "GAM Smoothing", "Step Functions")

for (i in 1:nrow(long_data)){ 
  if (long_data$key[i] == 'train_rmse_quote'){ 
    long_data$xlabs[i] = 'Quotient Train Err'
  } else if(long_data$key[i] == 'test_rmse_quote'){ 
    long_data$xlabs[i] = 'Quotient Test Err'
  } else if(long_data$key[i] == 'train_rmse_diff'){ 
    long_data$xlabs[i] = 'Diff Train Err'
  } else if(long_data$key[i] == 'test_rmse_diff'){ 
    long_data$xlabs[i] = 'Diff Test Err'
  } else if(long_data$key[i] == 'train_rmse_quote_gam'){ 
    long_data$xlabs[i] = 'Quotient Train Err'
  } else if(long_data$key[i] == 'test_rmse_quote_gam'){ 
    long_data$xlabs[i] = 'Quotient Test Err'
  } else if(long_data$key[i] == 'train_rmse_diff_gam'){ 
    long_data$xlabs[i] = 'Diff Train Err'
  } else if(long_data$key[i] == 'test_rmse_diff_gam'){ 
    long_data$xlabs[i] = 'Diff Test Err'
  }
}

long_data$xlabs <- factor(long_data$xlabs, levels = c('Quotient Train Err', 'Quotient Test Err', 'Diff Train Err', 'Diff Test Err'))

ggplot(long_data, aes(x = xlabs, y = value, fill = key)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(x = "", y = "Weighted RMSE", 
       title = "10-Fold Cross Validated Train and Test Weighted RMSE") +
  facet_grid(~main_level, 
             scales = "free_x", # Let the x axis vary across facets.
             space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x") + 
  theme(legend.position = 'none') + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))


table(long_data$xlabs)


