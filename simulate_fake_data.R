
# take the code for a ride

# to do: look into voter registration by zip code

data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"

setwd(code.dir)
source("simulate_helper.R")

library(fabricatr)

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
                                         category_labels = c("a.Democrat",
                                                             "b.Republican",
                                                             "c.Indep",
                                                             "d.Other") ),
               
               # ##### Secondary Outcomes #####
               # # sub-measures of speciesism
               # spec1 = draw_likert(),
               # 
               # dom = rnorm( N, mean = 0, sd = 1),  # no effect with this one
               # 
               # activ = rnorm( N, mean = .2 * treat, sd = 1),
               
               ##### Misc #####
                
               # awareness probe #1
               guessPurpose1 = draw_categorical( prob = rep(1/10, 10),
                                          N = N,
                                          category_labels = c("a.totalCal",
                                                              "b.protein",
                                                              "c.fat",
                                                              "d.vegetables",
                                                              "e.fruit",
                                                              "f.wholeGrains",
                                                              "g.meatAnimProd",
                                                              "h.refined",
                                                              "i.beverages",
                                                              "j.dontKnow") ),
               
               guessPurpose2 = draw_categorical( prob = rep(1/4, 4),
                                          N = N,
                                          category_labels = c("a.generalPatterns",
                                                              "b.increase",
                                                              "c.decrease",
                                                              "d.dontKnow") ),
              
)

#### Add Time Spent on Video #####
d$video.time = 0  # no one in control group watches video
d$video.time[ d$treat == 1] = pmin(20, runif(n=sum(d$treat == 1), min=16, max=30))  # for treated group, create point mass at 20
# sanity check
d %>% group_by(treat) %>%
  summarise( mean(video.time) )

##### Add Secondary Psychological Outcomes #####
secondaryY = c("spec",
               "dom",
               "activ")

make_psych_scale(yName = "spec", n.items = 6, n.responses = 7)
make_psych_scale(yName = "dom", n.items = 8, n.responses = 7)
make_psych_scale(yName = "activ", n.items = 5, n.responses = 7)


##### Add Primary (Food) Outcomes #####
meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)

for ( i in allFoods){
  make_food_Y(yName = i)
}


##### Attrition #####
retention = 0.85

foodVars = c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
              names(d)[ grepl(pattern = "Ounces", names(d) ) ] )


fu.vars = c(foodVars,
            names(d)[ grepl(pattern = "spec", x = names(d) ) ],
            names(d)[ grepl(pattern = "dom", x = names(d) ) ],
            names(d)[ grepl(pattern = "activ", x = names(d) ) ],
            names(d)[ grepl(pattern = "guessPurpose", x = names(d) ) ] )

lost.to.fu = rbinom(nrow(d), size = 1, prob = 1 - retention)

d[ lost.to.fu == 1, fu.vars ] = NA

table(is.na(d))

##### Sporadic Missing Data #####

d = degradefunction(d, del.amount = .08)


##### Save the Fake Dataset #####
setwd(data.dir)
write.csv(d, "raw_FAKE_data.csv")


