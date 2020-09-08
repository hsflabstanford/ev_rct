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


make_derived_vars = function(.d,
                             # variable lists
                             .meats = meats,
                             .animProds = animProds,
                             .goodPlant = goodPlant,
                             
                             printCorMat = TRUE  # print subscale correlation matrices for psych vars?
                             ){
  
  # # test only
  # .d = d
  # .meats = meats
  # .animProds = animProds
  # .goodPlant = goodPlant
  
  # recode food variables
  for ( i in allFoods){
    .d = recode_food_Y(.d = .d,
                       food = i)
  }
  
  #browser()
  # secondary outcome: total meat (total ounces over the week)
  .d$totalMeat = rowSums( .d[, .meats])
  
  # sanity check
  # express in pounds of meat
  hist(.d$totalMeat/16)
  
  # secondary outcome: total animal products (total ounces over the week)
  .d$totalAnimProd = rowSums( .d[, .animProds])
  
  # primary outcome: total meat + animal product consumption
  .d$mainY = .d$totalMeat + .d$totalAnimProd
  # # binary version for measurement error sensitivity analysis
  # .d$mainYLow = .d$mainY < median( .d$mainY[.d$treat == 0], na.rm = TRUE )
  # frequency-only version for sensitivity analysis
  vars = c("chicken_Freq",
           "turkey_Freq",
           "fish_Freq",
           "pork_Freq",
           "beef_Freq",
           "otherMeat_Freq",
           "dairy_Freq",
           "eggs_Freq")
  expect_equal(TRUE, all(vars %in% names(d)) )
  .d$mainYFreqOnly = rowSums( .d[,vars] ) 
  
  # secondary: total good plant-based foods
  .d$totalGood = rowSums( .d[, .goodPlant])
  
  hist(.d$totalGood/16)
  
  # bm1
  
  # recode secondary psych scales
  # @need to use lowercase because I changed Qualtrics
  .d = recode_psych_scale( .d = .d,
                           scale = "spec",
                           revCode = "X5_spec")

  .d = recode_psych_scale( .d = .d,
                           scale = "activ",
                           revCode = NA)
  
  .d = recode_psych_scale( .d = .d,
                           scale = "dom",
                           revCode = c("X3_dom",
                                       "X4_dom",
                                       "X7_dom",
                                       "X8_dom") )
  
  
  # standardize the importance variables?
  
  

  # # recode compliance (finished watching video)
  # .d$finishedVid = (.d$video.time >= 20)
  
  # recode awareness
  .d$aware = (.d$guessPurpose == "g.meatAnimProd") & (.d$guessPurpose2 == "c.decrease")
  
  
  ##### Dichotomize effect modifiers #####
  # @will need post-hoc editing
  .d$female = NA; .d$female[ .d$sex == "a.Female" ] = 1; .d$female[ .d$sex == "b.Male" ] = 0  # NA for those marking "Other"
  .d$old = (.d$age > 25)
  .d$democrat = NA; .d$democrat[ .d$party == "Democrat" ] = 1; .d$democrat[ .d$party == "Republican" ] = 0  # exclude independents  # excludes Independents and "don't knows"
  .d$collegeGrad = .d$educ %in% c("c.2yr", "d.4yr", "e.post")

  return(.d)
}





# yName: just the root of the string, e.g., "dairy"
# NOTE: overwrites the frequency and amount variables in the returned dataset
recode_food_Y = function(.d,
                         food){
  
  # # test only
  # .d = d
  # food = "legumes"
  
  freqString = paste(food, "Freq", sep = "_")
  amountString = paste(food, "Ounces", sep = "_")
  
  # duplicate data just to facilitate sanity checks
  .d2 = .d
  
  # overwrite old frequency variable
  # recode as servings per week
  .d2[[freqString]] = dplyr::recode( .d2[ , freqString ],
                                     a.Never = 0,
                                     b.1Weekly = 1,
                                     c.2Weekly = 2,
                                     d.3to4Weekly = 3.5,
                                     e.5to6Weekly = 5.5,
                                     f.1Daily = 7,
                                     g.2PlusDaily = 14 ) 
  
  # # sanity check
  table( .d2[[freqString]], .d[[freqString]], useNA = "ifany" )
  
  # recode ounces variable
  # when they answered "never" to frequency question, ounces question
  #  was not asked, so will be NA
  .d2[[ amountString ]] = as.numeric( as.character(.d2[[ amountString ]]) )
  .d2[[ amountString ]][ .d2[[freqString]] == 0 ] = 0
  
  # # sanity check
  # # when freqString is zero, amountString should always be 0
  # # and when freqString is more than 0, amountString should be more than 0
  # .d2 %>% group_by( get(freqString) == 0, get(amountString) ) %>%
  #   summarise(n())
  

  # make the ounces-per-week variable
  .d2[, food] = as.numeric( as.character(.d2[[freqString]]) ) * as.numeric( as.character(.d2[[amountString]]) )
  
  return(.d2)
}

# overwrites subscales with reverse-coded versions and creates a new standardized composite
#  variable from the subscales' sum
#
# scale: part of scale string that appears in each subscale's variable name (e.g., "spec" for speciesism); also becomes the name of the new composite variable
# revCode: quoted names of any subscales that need to be reverse-coded
# @NOT TESTED YET
# bm
recode_psych_scale = function(.d,
                              scale, 
                              revCode = NA,
                              printCorMat = TRUE) {
  # bm2
  
  # # test only
  # .d = d
  # scale = "dom"
  # revCode = c("X3_dom",
  #             "X4_dom",
  #             "X7_dom",
  #             "X8_dom")
  # printCorMat = TRUE
  
  # duplicate dataset just for ease of sanity-checking
  .d2 = .d
  
  subscales = names(.d2)[ grepl( x = names(.d2), pattern = scale ) ]
  
  # make numeric instead of factor
  .d2 = .d2 %>% mutate_at( subscales, function(x) as.numeric( as.character(x) ) )

  # sanity check
  # correlation matrix prior to reverse-coding
  if (printCorMat == TRUE){
    library(corrr)
    corrs = .d2 %>% select(subscales) %>%
      correlate( use = "pairwise.complete.obs" )
    
    print( paste( "Reverse-coded: ", paste(revCode, collapse = ", "), sep = "" ) )
    print(corrs)
  }
  
  # just use negative values for any scales that need reverse-coding
  if ( !any( is.na(revCode) ) ) .d2[ , revCode] = -.d2[ , revCode]
  
  head( .d2 %>% select(subscales) )
  
  # make new variable whose name is just the root
  # new variable is mean by subject of the subscales
  .d2[, scale ] = rowMeans( .d2[ , subscales] )
  
  hist( .d2[, scale ]  )
  
  # standardize the new variable
  .d2[, scale ] = ( .d2[, scale ] - mean( .d2[, scale ], na.rm = TRUE ) ) / sd( .d2[, scale ], na.rm = TRUE )
  
  return(.d2)
}


# # makes composite from the Likert psych scales
# recode_psych_scale = function(.d,
#                               scale){
#   
#   # # ~~ TEST ONLY
#   # scale = "spec"
#   
#   stop("Edit recode_psych_scale to handle reverse-coding")
#   
#   subscales = names(.d)[ grepl( x = names(.d), pattern = scale ) ]
#   
#   for( j in subscales ){
#     .d[ , j] = dplyr::recode( .d[ , j],
#                               `Strong Agree` = 3,
#                               Agree = 2,
#                               `Lean Agree` = 1,
#                               `Don't Know / Neutral` = 0,
#                               `Lean Disagree` = -1,
#                               Disagree = -2,
#                               `Strongly Disagree` = -3 )
#   }
#   
#   # composite is mean of the subscales
#   .d[, scale] = rowMeans( .d[ , subscales] )
#   
#   return(.d)
# }

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
