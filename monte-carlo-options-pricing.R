getUnifDist <- function(seed, n) {
  x0 <- seed
  a <- 7^5
  m <- 2^31 -1
  lgm <- vector()
  lgm[1] <-  x0
  for (i in 2:n) {
    lgm[i] <- (a*lgm[i-1]) %% m
  }

  unif_dist <- vector()
  for (i in 1:n) {
    unif_dist[i] <- (lgm[i]+0.5)/m
  }
  return(unif_dist)
}


getNormalDist <- function(n) {
  u1 <- getUnifDist(as.numeric(as.POSIXct(Sys.time())),n/2)  
  u2 <- getUnifDist(as.numeric(as.POSIXct(Sys.time())),n/2)
  z1_vect <- vector()
  z2_vect <- vector()

  pi = 22/7
  for (i in 1:n/2) {
    z1_vect [i] <- sqrt (-2*log(u1[i]))*cos(2*pi*u2[i])
    z2_vect [i] <- sqrt (-2*log(u1[i]))*sin(2*pi*u2[i])
  } 
  
  norm_dist <- c(z1_vect,z2_vect)
  return(norm_dist)
}

#Accepting input parameters from user

r <- as.numeric( readline(prompt="Enter Risk-free Rate (r): "))
sigma <- as.numeric (readline(prompt="Enter Volatility (v): "))
s0 <- as.numeric(readline(prompt="Enter Spot Price (s0): "))
t <- as.numeric(readline(prompt="Enter Time to expiration (t): "))
k <- as.numeric (readline(prompt="Enter Strike Price (k): "))

#sample parameters if you want to skip user input
# r = 0.06
#sigma = 0.15
# s0 = 128
# t = 7
# k = 135

z <- getNormalDist(10000)
antiz = -1*z
Wt <-  sqrt (t)*z
antiWt <- sqrt(t)*antiz
St <- 0
totalpayoff <- 0
sim1 <- vector()
for (i in 1:10000) {
  payoff <- 0
  powerterm <- (sigma*Wt[i]) + (t*(r-(sigma^2/2)))
  St <-  s0*exp(powerterm)
  
  if (St > k) {
    payoff = exp(-1*r*t)*(St-k)
    
    }
  #print(paste("Payoff for Path ", i, " is: ", payoff))
  sim1 <- c(sim1,payoff)
  totalpayoff <- totalpayoff + payoff 
}
avgpayoff <-  totalpayoff/10000
print (paste("Option Price using Monte Carlo Simulation: ", round(avgpayoff, 4)))



# Price using Black Scholes Formula: 
d1 = (log(s0/k) + (r+sigma^2/2)*t)/(sigma*t^.5)
d2 = d1 - (sigma*t^.5)
BS_price = pnorm(d1)*s0 - pnorm(d2)*k*exp(-r*t)

print(paste("Option Price using Black Scholes Formula: ",round(BS_price,5)))
print(paste("Deviation between Monte-Carlo and Black Scholes Forumula: ", round(abs(BS_price-avgpayoff)*100/BS_price,2),"%"))

