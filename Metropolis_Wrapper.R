#Wrapper for finding the best standard deviation
#should take in just a standard deviation 
#Should use the optim function

metropolis_wrapper_sd <- function(sd){
  #Fixing: Inital Guess, No Its and target function
  xn = 0
  N = 10000
  outputs = metropolis(target_function = target, xn = xn, N = N, sd = sd)
  a = acf(outputs, plot = FALSE)
  return(sum(abs(a$acf)))
}

pb <- winProgressBar(title="Progress bar", 
                     label="0% done", min=0, max=1000, initial=0)
m = 1000
sds = c()
val = NULL
for (i in 1:m) {
  sds[i] = runif(1,0,10)
  val[i] = metropolis_wrapper_sd(sds[i])
  info <- sprintf("%d%% done", round((i/100)*100)) 
  setWinProgressBar(pb, i/(100)*100, label=info)
}
min = min(val)
min_index <- which.min(val)

sds[min_index]

plot(sds, val, xlab = 'Standard Deviation', ylab = 'ACF', 
     main = 'Plot of Standard Deviations and AutoCorrelation', pch = 20)
abline(h = min, lt = 'dashed', col = 'red')
