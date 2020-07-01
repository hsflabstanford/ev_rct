
make_food_Y = function(yName,
                       betaFreq, 
                       betaAmount) {
  
  # yName = "eggs"
  # betaFreq = .2
  # betaAmount = 0
  
  
  freqString = paste(yName, "Freq", sep = "")
  amountString = paste(yName, "Ounces", sep = "")
  
  d[ , freqString ] <<- rnorm( n = nrow(d),
                             mean = betaFreq * d$treat,
                             sd = 1)
  
  d[ , amountString ] <<- rnorm( n = nrow(d),
                             mean = betaFreq * d$treat,
                             sd = 1)
}