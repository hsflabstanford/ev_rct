
# take the code for a ride

# to do: look into voter registration by zip code

data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"

setwd(code.dir)
source("simulate_helper.R")

library(fabricatr)
library(noncensus)

N=650
d = fabricate( N = N,
               treat = rep( c(1, 0), each = N/2),
               
               ##### Demographics #####
               sex = draw_categorical( prob = c(.48, .48, .02),
                                       N = N,
                                       category_labels = c("a.Female", "b.Male", "c.Other") ),
               
               age = round( runif(n = N, 
                                  min = 18, 
                                  max = 89) ),
               
               educ = draw_categorical( prob = rep(1/5, 5),
                                        N = N,
                                        category_labels = c("a.subHS",
                                                            "b.HS",
                                                            "c.2yr",
                                                            "d.4yr",
                                                            "e.post") ),
               
               # non-exclusive race indicators
               cauc = rbinom( prob = 0.7, size = 1, n = N),
               hisp = rbinom( prob = .1, size = 1, n = N),
               black = rbinom( prob = .13, size = 1, n = N),
               midEast = rbinom( prob = .09, size = 1, n = N),
               pacIsl = rbinom( prob = .05, size = 1, n = N),
               natAm = rbinom( prob = .05, size = 1, n = N),
               SAsian = rbinom( prob = .1, size = 1, n = N),
               EAsian = rbinom( prob = .1, size = 1, n = N),
               SEAsian = rbinom( prob = .1, size = 1, n = N),
               
               # need to do this one! 
               #zip = 
               
               party = draw_categorical( prob = c(.35, .35, .15, .15),
                                         N = N,
                                         category_labels = c("a.Democrat", "b.Republican", "c.Indep", "d.Other") ),
               
               ##### Secondary Outcomes #####
               spec = rnorm( N, mean = .08 * treat, sd = 1),
               dom = rnorm( N, mean = 0, sd = 1),  # no effect with this one
               activ = rnorm( N, mean = .2 * treat, sd = 1),
               
               ##### Misc #####
               finished.vid = rbinom( prob = 0.8, size = 1, n = N),
                 
               aware = rbinom( prob = 0.1, size = 1, n = N)
              
)


##### Add Primary (Food) Outcomes #####
meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)

for ( i in allFoods){
  make_food_Y(yName = i,
              betaFreq = .03,
              betaAmount = 0)
}


##### Missing Data and Attrition #####
retention = 0.85

foodVars = c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
              names(d)[ grepl(pattern = "Ounces", names(d) ) ] )

secondaryY = c("spec",
               "dom",
               "activ")

fu.vars = c(foodVars, secondaryY, "aware" )

lost.to.fu = rbinom(nrow(d), size = 1, prob = 1 - retention)

d[ lost.to.fu == 1, fu.vars ] = NA


##### Save the Fake Dataset #####
setwd(data.dir)
write.csv(d, "raw_FAKE_data.csv")


