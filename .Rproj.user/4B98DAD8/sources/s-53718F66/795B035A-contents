
########################### FNs FOR FORMATTING RESULTS ###########################

# round while keeping trailing zeroes
my_round = function(x, digits) {
  formatC( round( x, digits ), format='f', digits=digits )
}


# format a p-value with scientific notation stars for cutoffs
# star.cutoffs: cutoffs for *, **, ***, etc., provided in any order
format_pval = function( p,
                        digits = 3,
                        star.cutoffs = NA ) {
  
  if (p >= 0.01) string = as.character( my_round( p, digits ) )
  if (p < 0.01 & p > 10^-5 ) string = formatC( p, format = "E", digits = 2 )
  if ( p < 10^-5 ) string = "< 1E-05"
  
  if ( ! is.na(star.cutoffs[1]) ) {
    
    # put in descending order
    star.cutoffs = sort(star.cutoffs, decreasing = TRUE)
    
    for ( i in 1 : length(star.cutoffs) ) {
      if ( p < star.cutoffs[i] ) string = paste( string, "*", sep="" )
    }
  }
  
  return(string)
}

# example
# p = seq( 0, .2, 0.001 )
# vapply( p, format_pval, "asdf" )
# vapply( p, function(x) format_pval( x, star.cutoffs = c( 0.01, 0.05) ), "asdf" )


########################### FN: RECODE VARIABLES DURING DATA PREP ###########################

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
  .d$mainYbin = .d$mainY > median( .d$mainY[.d$treat == 0], na.rm = TRUE )
  
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


########################### FN: POOL IMPUTED SEs VIA RUBIN'S RULES ###########################

# see Marshall "Combining estimates" paper, pg 3

# ests: ests from m imputations
# ses: ses from m imputations
rubin_se = function( ests, ses ){
  
  m = length(ests)
  
  # within-imputation variance
  Ubar = mean( ses^2 )
  
  # between-imputation variance
  B = (1 / (m-1)) * sum( ( ests - mean(ests) )^2 )
  
  # overall SE
  return( sqrt( Ubar + (1 + (1/m)) * B ) )
}


################################ MISCELLANEOUS ################################

# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
update_result_csv = function( name,
                              section = NA,
                              value = NA,
                              print = FALSE ) {
  setwd(results.dir)
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  if ( "stats_for_paper.csv" %in% list.files() ) {
    res = read.csv( "stats_for_paper.csv",
                    stringsAsFactors = FALSE,
                    colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% res$name) ) res[ res$name %in% name, ] = new.rows
    else res = rbind(res, new.rows)
  }
  
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    res = new.rows
  }
  
  write.csv( res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  # also write to Overleaf
  setwd(overleaf.dir)
  write.csv( res, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  if ( print == TRUE ) {
    View(res)
  }
}


# make my own Table 1
# x: variable to be summarized
# type: "cat", "bin01", "cont"
# countNA: should we count NA as its own category for cat and bin01?
# tab1: the current table 1 (if NA, starts generating one from scratch)
table1_add_row = function( x, # vector
                           var.header,  # variable name to use in table
                           type,
                           perc.digits = 0,
                           num.digits = 2,
                           countNA = TRUE,
                           .tab1 = NULL,
                           print = TRUE ) {
  
  useNA = ifelse( countNA == TRUE, "ifany", "no" )
  
  if ( type == "cat" ) {
    t = table(x, useNA = useNA)
    pt = prop.table(t)
    
    row.names = names(t)
    row.names[ is.na(row.names) ] = "Not reported"
    
    stat.string = paste( t, " (", round( 100 * pt, digits = perc.digits ), "%)", sep = "" )
  }
  
  if ( type == "bin01" ) {
    # force "1" entry to be ordered first
    t = table(x == 0, useNA = useNA)
    pt = prop.table(t)
    
    row.names = names(t)
    # reverse the coding again
    row.names[ row.names == "FALSE" ] = "Yes"
    row.names[ row.names == "TRUE" ] = "No"
    row.names[ is.na(row.names) ] = "Not reported"
    
    stat.string = paste( t, " (", round( 100 * pt, digits = perc.digits ), "%)", sep = "" )
  }
  
  if ( type == "cont") {
    # assume we want the median and IQR
    if ( countNA == TRUE ) {
      
      stat.string = paste( round( median( x, na.rm = TRUE ), digits = num.digits ),
                           " (", 
                           round( quantile( x, 0.25, na.rm = TRUE ), digits = num.digits ),
                           ", ",
                           round( quantile( x, 0.75, na.rm = TRUE ), digits = num.digits ),
                           ")", 
                           sep = "" )
      
      n.NA = sum( is.na(x) )
      perc.NA = mean( is.na(x) )
      
      stat.string2 = paste( n.NA, " (", round( 100 * perc.NA, digits = perc.digits ), "%)", sep = "" )
      
      # first row is just the median, so no row name
      row.names = c("Not reported")
    }
    # haven't written the case of countNA == FALSE yet because not relevant for this paper
    
    new.row = data.frame( 
      "Characteristic" = c( var.header, row.names ),
      "Summary" = c( stat.string, stat.string2 ) )
  }
  
  if ( type %in% c("cat", "bin01") ) {
    new.row = data.frame( 
      "Characteristic" = c( var.header, row.names ),
      "Summary" = c( NA, stat.string ) )
  }
  
  # add the new row to existing Table 1, if applicable
  if ( !is.null(.tab1) ) .tab1 = rbind(.tab1, new.row)
  else .tab1 = new.row
  if ( print == TRUE ) print(.tab1)
  return(.tab1)
}



# return percent true for 0/1 variable, counting NA as own category
percTRUE_incl_NA = function(x) {
  prop.table( table(x, useNA = "ifany") )[2]
}



make_table_one = function(.d){
  t = table1_add_row( x = .d$sex,
                      var.header = "Sex",  
                      type = "cat",
                      countNA = TRUE )
  
  t = table1_add_row( x = .d$age,
                      var.header = "Age",  
                      type = "cont",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .d$educ,
                      var.header = "Education",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$party,
                      var.header = "Political party",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$cauc,
                      var.header = "Caucasian",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$hisp,
                      var.header = "Hispanic",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$black,
                      var.header = "Black/African American",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  
  t = table1_add_row( x = .d$midEast,
                      var.header = "Middle Eastern",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  
  t = table1_add_row( x = .d$pacIsl,
                      var.header = "Pacific Islander",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  
  t = table1_add_row( x = .d$natAm,
                      var.header = "Native American",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$SAsian,
                      var.header = "South Asian",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$EAsian,
                      var.header = "East Asian",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$SEAsian,
                      var.header = "Southeast Asian",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  return(t)
}



