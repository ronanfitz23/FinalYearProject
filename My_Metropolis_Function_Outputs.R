target <- function(x) {
  exp(-x^2) * (2 + sin(5 * x) + sin(2 * x))
}

outputs1 = metropolis(target_function = target, 
                     xn = 0, N = 10000, sd = 2.5)
outputs2 = metropolis(target_function = target, 
                      xn = -3, N = 10000, sd = 0.1)
outputs3 = metropolis(target_function = target, 
                      xn = 3, N = 10000, sd = 10)

acceptance1 = outputs1[[2]]
acceptance2 = outputs2[[2]]
acceptance3 = outputs3[[2]]

#TracePlots for report ---------------------------------------------------------
outputs_trace1 = outputs1[[1]]
outputs_trace2 = outputs2[[1]]
outputs_trace3 = outputs3[[1]]

par(mfrow=c(3,1))
plot(outputs_trace1, type = 's')
plot(outputs_trace2, type = 's')
plot(outputs_trace3, type = 's')

par(mar = c(1,1,1,1))
plot(outputs_trace1, type = 's')

#Burn-in Plot for Report--------------------------------------------------------
par(mfrow = c(1,1))
plot(outputs_trace1, type = 's', main = 'Establishing Burn-In Period')
abline(v = 1000, col = 'red', lty = 'dashed', lwd = 2)

#Acf Plots for report-----------------------------------------------------------

test = metropolis(target_function = target, 
                  xn = 0, N = 10000, sd = 2.1)
test = test[[1]]

test_thinn = test[1000:10000]
test_thinn = test_thinn[seq(1, length(test_thinn), 3)]

acf1 = acf(test, plot = FALSE)
acf2 = acf(test_thinn, plot = FALSE)

par(mfrow = c(1,2))
plot(acf1, main = 'ACF Plot')
plot(acf2, main = 'ACF after Burn-in and Thinning')
#Histogram Plot for report------------------------------------------------------

hist(outputs_trace1, breaks = seq(-3, 5, .05), xlim = c(-3,3), 
     main = 'Target Fucntion Approximated by Metropolis-Hastings',
     xlab = '',
     freq = FALSE)

x <- seq(-3, 3, .05)
p <- target(seq(-3, 3, .05))
expected <- (p / sum(p))*20
lines(
  x = x, 
  y = expected, 
  type = "l", 
  col = "red",
  lwd = 2
)

