

########################### FN: MAKE A FOOD OUTCOME VARIABLE ###########################

# make variables for each subcomponent of a scale
make_psych_scale = function(yName,
                        n.items,
                        n.responses){
  
  # yName = "spec"
  # n.items = 6
  # n.responses = 7
  
  for ( i in 1:n.items ){
    string = paste(yName, i, sep="")
    d[,string] <<- draw_likert(x = rnorm( n = nrow(d) ),
                               type = n.responses )
  }

}

#make_psych_scale("spec", n.items = 6, n.responses = 7)

# make 2-item food measures
make_food_Y = function(yName) {
  
  # yName = "eggs"
  # betaFreq = .2
  # betaAmount = 0
  
  
  freqString = paste(yName, "Freq", sep = "")
  amountString = paste(yName, "Ounces", sep = "")
  
  d[ , freqString ] <<- draw_categorical( prob = rep(1/7, 7),
                                          N = nrow(d),
                                          category_labels = c("a.Never",
                                                              "b.1Weekly",
                                                              "c.2Weekly",
                                                              "d.3to4Weekly",
                                                              "e.5to6Weekly",
                                                              "f.1Daily",
                                                              "g.2PlusDaily") )
  
  d[ , amountString ] <<- draw_categorical( prob = rep(1/3, 3),
                                            N = nrow(d),
                                            category_labels = c("a.lessThan2",
                                                                "b.2to5",
                                                                "c.moreThan5") )
}

########################### FN: INDUCE MCAR MISSINGNESS ###########################

# this was only used for testing the code on fake data

# induce missingness
# https://stackoverflow.com/questions/18837896/degrading-data-randomly-with-pre-existing-missingness

# del.amount = number of observations in dataset to make missing

degradefunction <- function(x, del.amount){
  
  # 1) indicate which cells are NA (works with matrix or df)
  preNAs     <- is.na(x)
  # 2) how many cells are eligible to be degraded?
  OpenSpots  <- prod(dim(x)) - sum(preNAs)
  # 3) of these, select del.amount for replacement with NA
  newNas     <- sample(1:OpenSpots, size = del.amount, replace = FALSE)
  # 4) impute these NAs, ignoring the original NAs
  x[!preNAs][newNas] <- NA
  x
}