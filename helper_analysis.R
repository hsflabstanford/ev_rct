
########################### FNs FOR STATISTICS ###########################

##### Fn: Calculate Crude RR #####

# gets the raw RR 
# assumes outcome is called "intentionReduce"
get_rr_unadj = function(condition,
                        condition.var.name = "condition",
                        control.name = "control",
                        dat) {
  
  # remove other interventions in case the study was more than 2 arms
  temp = droplevels( dat[ dat[[condition.var.name]] %in% c(condition, control.name), ] )
  
  tab = table( temp[[condition.var.name]], temp$intentionReduce )
  
  # state sample size
  print( paste( "Analyzed N:", nrow(temp) ) )
  
  library(metafor)
  es = escalc( measure = "RR",
               ai = tab[condition, 2], # X=1, Y=1
               bi = tab[condition, 1],  # X=1, Y=0
               ci = tab[control.name, 2], # X=0, Y=1
               di = tab[control.name, 1] ) # X=0, Y=0
  
  return(es)
}



##### Fn: Nicely Organize Welch t-test Results #####
my_ttest = function( yName, dat ){
  
  tres = t.test( dat[[yName]] ~ treat,
                 data = dat,
                 var.equal = FALSE )
  
  dat$Y = dat[[yName]]
  tab = suppressMessages( dat %>% group_by(treat) %>%
                            summarise( m = mean(Y),
                                       sd = sd(Y),
                                       n = n() ) )
  
  # standardized mean difference (Hedges' g)
  es = escalc( m1i = tab[2,]$m,
               sd1i = tab[2,]$sd,
               n1i = tab[2,]$n,
               
               m2i = tab[1,]$m,
               sd2i = tab[1,]$sd,
               n2i = tab[1,]$n,
               
               # Hedges' g by default
               measure = "SMD")
  summ = summary(es)
  
  #browser()
  
  return( data.frame( # documentary - control
    est = tres$estimate[2] - tres$estimate[1],
    se = tres$stderr,
    # note the reversed order because t-test calculates 
    #  control - documentary:
    lo = -tres$conf.int[2],
    hi = -tres$conf.int[1],
    pval = tres$p.value,
    
    # standardized mean difference (Hedges' g)
    g = es$yi,
    g.se = summ$sei,
    g.lo = summ$ci.lb,
    g.hi = summ$ci.ub ) )
}


##### Do OLS with Effect Modifier with Robust SEs #####

# ols: the OLS model with all the effect modifiers
my_ols_hc0 = function( coefName, dat, ols ){
  
  ( se.ols = sqrt( vcov(ols)[coefName, coefName] ) )
  ( bhat.ols = coef(ols)[coefName] )
  
  # heteroskedasticity-consistent robust SEs:
  (se.hc0 = sqrt( vcovHC( ols, type="HC0")[coefName, coefName] ) )
  
  tcrit = qt(.975, df = ols$df.residual)
  t = as.numeric( abs(bhat.ols / se.hc0) )
  
  # standardized mean difference
  # **note for paper: standardizing by SD(Y|X) rather than SD(Y|X,Z) where
  #  Z is the effect modifiers because former is more directly comparable
  #  to the effect sizes in main analysis
  # note also that we need to calculate sd.pooled for each MI dataset rather than 
  #  just transforming the final pooled estimate to an SMD, because SD(Y|X) differs 
  #  in each imputed dataset
  tab = suppressMessages( dat %>% group_by(treat) %>%
                            summarise( m = mean(mainY, na.rm = TRUE),
                                       sd = sd(mainY, na.rm = TRUE),
                                       n = n() ) )
  num = (tab$n[1] - 1) * tab$sd[1]^2 + (tab$n[2] - 1) * tab$sd[2]^2
  denom = (tab$n[1] - 1) + (tab$n[2] - 1)
  sd.pooled = sqrt(num/denom)
  # adjustment factor for Hedges' g
  # https://www.statisticshowto.com/hedges-g/#:~:text=Hedges'%20g%20is%20a%20measure,of%20up%20to%20about%204%25.
  N = sum(tab$n)
  J = ( (N-3) / (N-2.25) ) * sqrt( (N-2) / N )
  # factor to multiply with the raw mean difference to get Hedges' g
  term = J / sd.pooled
  
  return( data.frame(
    est = bhat.ols,
    se = se.hc0,
    lo = bhat.ols - tcrit * se.hc0,
    hi = bhat.ols + tcrit * se.hc0,
    pval =  2 * ( 1 - pt(t, df = ols$df.residual ) ),
    
    # standardized mean difference (Hedges' g)
    g = bhat.ols * term,
    g.se = se.hc0 * term,
    g.lo = (bhat.ols - tcrit * se.hc0) * term,
    g.hi = (bhat.ols + tcrit * se.hc0) * term ) )
}



# OLD VERSION THAT FITS A SEPARATE MODEL FOR EACH EFFECT MODIFIER:
# # ASSUMES MODERATOR IS BINARY
# my_ols_hc0 = function( modName, dat ){
#   
#   # make sure effect modifier is binary
#   levels = unique( dat[[modName]] )
#   if( length( levels[ !is.na(levels) ] ) > 2 ) stop("Effect modifier has more than two levels -- not allowed!")
#   
#   ols = lm( mainY ~ treat*dat[[modName]], data = dat )
#   
#   # coef name of interest could be either "treat:dat[[modName]]" or "treat:dat[[modName]]TRUE"
#   #  depending on how the effect modifier is coded
#   string = names(coef(ols))[ grepl( pattern = "treat:", x = names(coef(ols)) ) ]
#   
#   ( se.ols = sqrt( vcov(ols)[string, string] ) )
#   ( bhat.ols = coef(ols)[string] )
#   
#   # heteroskedasticity-consistent robust SEs:
#   (se.hc0 = sqrt( vcovHC( ols, type="HC0")[string, string] ) )
#   
#   tcrit = qt(.975, df = ols$df.residual)
#   t = as.numeric( abs(bhat.ols / se.hc0) )
#   
#   return( data.frame( 
#     est = bhat.ols,
#     se = se.hc0,
#     lo = bhat.ols - tcrit * se.hc0,
#     hi = bhat.ols + tcrit * se.hc0,
#     pval =  2 * ( 1 - pt(t, df = ols$df.residual ) ) ) )
# }


##### Fn: Nicely Organize 2-proportion Z-test results #####
# for the sensitivity analysis with dichotomized outcome
# returns on log-RR scale
my_log_RR = function( dat ){
  

  es = escalc( measure = "RR",
          ai = sum( dat$treat == 1 & dat$mainYLow == 1 ), # Tx with low consumption
          bi = sum( dat$treat == 1 & dat$mainYLow == 0 ),  # Tx with high consumption
          ci = sum( dat$treat == 0 & dat$mainYLow == 1 ),  # control with low consumption
          di = sum( dat$treat == 0 & dat$mainYLow == 0 ) )  # control with high consumption
  
  
  zcrit = qnorm(.975)
  z = abs(es$yi / sqrt(es$vi))
  
  return( data.frame( 
    est = es$yi,
    se = sqrt(es$vi),
    lo = es$yi - zcrit * sqrt(es$vi),
    hi = es$yi + zcrit * sqrt(es$vi),
    pval = 2 * ( 1 - pnorm(z) ) ) )
}



##### Fn: Pool Multiple Imputations Via Rubin's Rules #####
# ests: ests from m imputations
# ses: ses from m imputations
mi_pool = function( ests, ses ){
  
  m = length(ests)
  
  ##### Pooled Estimate #####
  est.pool = mean(ests)
  
  ##### Pooled SE #####
  # Dong & Peng (2013), pg 5
  # within-imputation variance
  Ubar = mean( ses^2 )
  # between-imputation variance
  B = (1 / (m-1)) * sum( ( ests - mean(ests) )^2 )
  # see Marshall "Combining estimates" paper, pg 3
  se.pool = sqrt( Ubar + (1 + (1/m)) * B ) 
  
  ##### CI and P-value #####
  # Dong & Peng (2013), pg 5
  # relative increase in variance due to missing data
  r = ( ( 1 + (1/m) ) * B ) / Ubar
  # degrees of freedom without the small-sample adjustment
  vm = (m-1) * ( 1 + (1/r) )^2
  tcrit = qt(0.975, df = vm)
  
  lo.pool = est.pool - tcrit * se.pool
  hi.pool = est.pool + tcrit * se.pool
  t.pool = abs(est.pool) / se.pool
  p.pool = 2 * ( 1 - pt(t.pool, df = vm) )
  
  return( data.frame( est = est.pool,
                      se = se.pool, 
                      lo = lo.pool,
                      hi = hi.pool,
                      pval = p.pool ) )
}



##### IV Regression #####

my_ivreg = function(dat){
  
  iv = ivreg(mainY ~ passCheck | treat, data = dat)
  
  est = coef(iv)["finishedVidTRUE"]
  summ = summary(iv, vcov = sandwich, diagnostics = TRUE)
  se = sqrt( summ$vcov["passCheckTRUE", "passCheckTRUE"] )
  t = abs(est)/se
  tcrit = qt(.975, df = iv$df.residual)
  
  return( data.frame( 
    est = est,
    se = se,
    lo = est - tcrit * se,
    hi = est + tcrit * se,
    pval = 2 * ( 1 - pt(t, df = iv$df.residual) ),
    # test for weak instruments
    # from AER package docs:
    # "an F test of the first stage regression for weak instruments"
    # so we want the stage 1 p-value to be LOW (i.e., non-weak instrument)
    stage1.pval = summ$diagnostics["Weak instruments", "p-value"] ) )
}

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
    res.overleaf <<- read.csv( "stats_for_paper.csv",
                    stringsAsFactors = FALSE,
                    colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% res.overleaf$name) ) res.overleaf[ res.overleaf$name %in% name, ] <<- new.rows
    else res.overleaf <<- rbind(res.overleaf, new.rows)
  }
  
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    res.overleaf <<- new.rows
  }
  
  write.csv( res.overleaf, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  # also write to Overleaf
  setwd(overleaf.dir)
  write.csv( res.overleaf, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  if ( print == TRUE ) {
    View(res.overleaf)
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
                           print = FALSE ) {
  
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
  
  t = table1_add_row( x = .d$pDem,
                      var.header = "County liberalism",  
                      type = "cont",
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



