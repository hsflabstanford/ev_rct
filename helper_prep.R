########################### FN: RECODE VARIABLES DURING DATA PREP ###########################

# recodes a Qualtrics checkbox question (i.e., a single column with comma-separated options)
#  into its consituent non-mutually-exclusive dummy variables
recode_checkboxes = function( .d, 
                              var ) {
  
  # split race into dummies
  # https://stackoverflow.com/questions/27630588/split-string-column-to-create-new-binary-columns/27630769#27630769
  t = mtabulate( strsplit( .d[[var]], ",") )
  
  # remove the old variable and replace with the new one
  .d = .d %>% select(-var)
  .d = cbind(.d, t)
  
  return(.d)
}

make_derived_vars = function(.d){
  
  # recode food variables
  for ( i in allFoods){
    .d = recode_food_Y(.d = .d,
                       food = i)
  }
  
  # secondary outcome: total meat (total ounces over the week)
  .d$totalMeat = rowSums( .d[, meats])
  
  # secondary outcome: total animal products (total ounces over the week)
  .d$totalAnimProd = rowSums( .d[, animProds])
  
  # primary outcome: total meat + animal product consumption
  .d$mainY = .d$totalMeat + .d$totalAnimProd
  # binary version for measurement error sensitivity analysis
  .d$mainYLow = .d$mainY < median( .d$mainY[.d$treat == 0], na.rm = TRUE )
  # frequency-only version for sensitivity analysis
  vars = c("chickenFreq",
           "turkeyFreq",
           "fishFreq",
           "porkFreq",
           "beefFreq",
           "otherMeatFreq",
           "dairyFreq",
           "eggsFreq")
  .d$mainYFreqOnly = rowSums( .d[,vars] ) 
  
  # secondary: total good plant-based foods
  .d$totalGood = rowSums( .d[, goodPlant])
  
  # recode secondary psych scales
  for ( i in secondaryY){
    .d = recode_psych_scale(.d = .d,
                            scale = i)
  }
  
  # recode compliance (finished watching video)
  .d$finishedVid = (.d$video.time >= 20)
  
  # recode awareness
  .d$aware = (.d$guessPurpose1 == "g.meatAnimProd") & (.d$guessPurpose2 == "c.decrease")
  
  
  
  ##### Dichotomize efect modifiers #####
  # ~~~will need post-hoc editing
  .d$female = NA; .d$female[ .d$sex == "a.Female" ] = 1; .d$female[ .d$sex == "b.Male" ] = 0
  .d$old = (.d$age > 25)
  .d$democrat = NA; .d$democrat[ .d$party == "a.Democrat" ] = 1; .d$democrat[ .d$party == "b.Republican" ] = 0  # exclude independents
  .d$collegeGrad = .d$educ %in% c("c.2yr", "d.4yr", "e.post")
  # ~~~need to make one for ratio of liberals to conservatives in zip code
  

  return(.d)
}





# yName: just the root of the string, e.g., "dairy"
recode_food_Y = function(.d,
                         food){
  
  freqString = paste(food, "Freq", sep = "")
  amountString = paste(food, "Ounces", sep = "")
  
  # overwrite old frequency variable
  .d[ , freqString ] = dplyr::recode( .d[ , freqString ],
                                      a.Never = 0,
                                      b.1Weekly = 1,
                                      c.2Weekly = 2,
                                      d.3to4Weekly = 3.5,
                                      e.5to6Weekly = 5.5,
                                      f.1Daily = 7,
                                      g.2PlusDaily = 14 ) 
  
  # overwrite old ounces variable
  .d[ , amountString ] = dplyr::recode( .d[ , amountString ],
                                        a.lessThan2 = 2,
                                        b.2to5 = 3.5,
                                        c.moreThan5 = 5 ) 
  
  # make the ounces-per-week variable
  .d[, food] = .d[ , freqString ] * .d[ , amountString ]
  
  return(.d)
}


# turns likert into numeric


# makes composite from the Likert psych scales
recode_psych_scale = function(.d,
                              scale){
  
  # # ~~ TEST ONLY
  # scale = "spec"
  
  subscales = names(.d)[ grepl( x = names(.d), pattern = scale ) ]
  
  for( j in subscales ){
    .d[ , j] = dplyr::recode( .d[ , j],
                              `Strong Agree` = 3,
                              Agree = 2,
                              `Lean Agree` = 1,
                              `Don't Know / Neutral` = 0,
                              `Lean Disagree` = -1,
                              Disagree = -2,
                              `Strongly Disagree` = -3 )
  }
  
  # composite is mean of the subscales
  .d[, scale] = rowMeans( .d[ , subscales] )
  
  return(.d)
}

# FROM OWP: 
# # standardize a variable
# standardize = function(x) {
#   (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
# }
# 
# # var.names: variable names to standardize (otherwise standardizes all
# #  continuous vars)
# make_derived_vars = function(dat,
#                              var.names = NULL) {
#   
#   # if no var.names provided, standardize all continuous vars
#   if ( is.null(var.names) ) {
#     # find all continuous variables in dataset, not just continuous
#     n.levels = apply( dat, 2,
#                       function(x) length( unique( x[ !is.na(x) ] ) ) )
#     library(dplyr)
#     numerics = select_if(d, is.numeric)
#     
#     # truly continuous variables have >2 levels (not 0/1)
#     #  and aren't factors or characters
#     is.cont = (n.levels > 2) & ( names(dat) %in% names(numerics) )
#     
#     # standardize the continuous variables
#     dat[ , is.cont ] = apply( dat[ , is.cont ],
#                               2, 
#                               function(x) standardize(x) )
#   } else {
#     # if provided variable names to standardize
#     dat[ , var.names ] = apply( dat[ , var.names ],
#                                 2, 
#                                 function(x) standardize(x) )
#   }
#   
#   # dichotomize the dietary quality score variable at its top tertile
#   cutoff = as.numeric( quantile( dat$nAHEI11a, 2/3, na.rm = TRUE ) )
#   temp = rep(NA, nrow(dat))
#   temp[ dat$nAHEI11a > cutoff ] = 1
#   temp[ dat$nAHEI11a <= cutoff ] = 0
#   dat$nAHEI11a = temp
#   
#   return(dat)
# }
# 
# # recode binary variables in several different formats into 0/1
# # for undoing the above function
# binarize = function(x) {
#   
#   # needs to be character, otherwise TRUE/FALSE and 0/1 variables
#   # aren't distinguished in the conditional statements to follow
#   levels.char = as.character( sort( unique(x) ) )
#   
#   if ( all( levels.char %in% c("a.No", "b.Yes") ) ) {
#     x = as.character(x)
#     xbin = rep(NA, length(x))
#     xbin[ x == "b.Yes" ] = 1
#     xbin[ x == "a.No" ] = 0
#     return(xbin)
#   } 
#   
#   if ( all( levels.char %in% c("FALSE", "TRUE") ) ) {
#     x = as.character(x)
#     xbin = rep(NA, length(x))
#     xbin[ x == TRUE ] = 1
#     xbin[ x == FALSE ] = 0
#     return(xbin)
#   } 
#   
#   # if the variable is already 0/1, leave it alone
#   # this is used in the Poisson part of analysis functions
#   # so that it can flexibly handle a binary variable coded as factor
#   #  or as 0/1
#   if ( all( levels.char %in% c("0", "1") ) ) return(x)
#   
#   stop("x needs to be either already 0/1, TRUE/FALSE, or 'a.No'/'b.Yes'")
#   
# }
# 
# # examples
# # binarize( c(1,1,1,0,1) )
# # binarize( c("a.No", "b.Yes") )
# # binarize(c("No", "Yes"))
# # binarize( c(FALSE, TRUE, FALSE) )
