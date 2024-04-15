metropolis = function(target_function, xn, N, sd){
  outputs = matrix(0,N,1)
  acceptances = 0
  for (i in 1:N) {
    #Propose xprop
    xprop = rnorm(1, xn, sd = sd)
    #Acceptance Probability of going from xn -> xprop
    #proposal is symmetric so prod2 of the prob is always = 1
    prod1 = target_function(xprop) / target_function(xn)
    prod2 = (pnorm(xn , xprop, sd = sd) /
             pnorm(xprop , xn, sd = sd, lower.tail = FALSE))
    accprob = min(1, prod1 * prod2)
    #If statement to check see if we are accepting proposal
    if (accprob > runif(1)){
      outputs[i] = xprop
      acceptances = acceptances + 1
    } else{
      outputs[i] = xn
    }
    xn = outputs[i]
  }
  acceptance_rate = acceptances / N
  return(list(outputs, acceptance_rate))
}