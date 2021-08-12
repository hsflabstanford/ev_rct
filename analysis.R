

# BM: Running code for each study to make sure it runs without errors and updating Overleaf,
#  but NOT yet writing new sanity checks. 
# Tables in main text will be submitted as Excel files.

# FOR CC SANITY CHECKS, COULD USE THE SCRIPT I DELETED ("**" COMMIT ON GIT)

# This script writes results locally and to Overleaf

# To do:
#  - When writing tables, keep names consistent with paper

# 0. PRELIMINARIES ------------------------------------------------

# Analyzes Study 1, 2, or 3 depending on the argument to prelims()
#  specified below. 

# Meta-notes about this script:
#  - Each section is self-contained. You can start from anywhere by first running prelims() and then 
#   running just that section.

#@IMPORTANT: This code still needs more sanity checks, especially analyze_all_outcomes!
# Data prep already has its own sanity checks in a separate file. 


rm( list = ls() )

# set your parameters here
study = 3

# should we delete existing stats_for_paper.csv and start over?
# note: since studies all write to same results file,
#  setting to TRUE will also wipe other studies' results
overwrite.res = FALSE

run.sanity = TRUE

# other packages are loaded by prelims() below
library(here)
code.dir = here("Code (git)")
setwd(code.dir)
source("helper_analysis.R")

# restore the environment and package versions MM used to analyze
# to use more recent versions, see package list in 
# helper_analysis.R::prelims()
library(renv)
setwd(here())
restore()
# only if you want to update the renv file:
# snapshot()

# makes a bunch of global variables for different directories, lists of variables, etc.,
#  and reads in datasets
prelims(study = study,
        overwrite.res = overwrite.res)
if ( overwrite.res == TRUE ) wr()


# 1. SANITY CHECKS ------------------------------------------------

# Table 1 (Wave 1)
CreateTableOne( vars = c(demo.raw), strata = "treat", data = dcc )


# # dropout 
# # completers
# CreateTableOne( vars = c(demo.raw, "passCheck", "aware"), strata = "treat", data = d %>% filter( !is.na(mainY)))

# sanity check: treatment effects
# main and secondaries t-tests
CreateTableOne( vars = c("mainY", secFoodY, psychY), strata = "treat", data = d )


# 2. DESCRIPTIVE & TABLE 1 ------------------------------------------------

# for keeping results csv organized
section = 2

# ~ Sample Sizes and Retention (For Study 1 & 3) ------------------------------------------------
update_result_csv( name = "N wave 1",
                   value = nrow(d) )


if ( study %in% c(1,3) ){
  
  update_result_csv( name = "N wave 2",
                     value = nrow(dcc) )
  
  # differential attrition by treatment group and demographics
  # logit(P(missingness))
  string = paste( "is.na(mainY) ~ ", paste( "treat +", paste( demo.raw, collapse=" + "), sep = "" ) )
  missMod = glm( eval( parse( text = string ) ),
                 family = binomial(link="logit"),
                 data = d )
  setwd(results.dir)
  # write model results to text file
  sink("logit_missingness_model.txt")
  summary(missMod)
  closeAllConnections()  # needed to get output in console again after this
  
  # test entire missingness model vs. null model
  nullModel = glm( is.na(mainY) ~ 1,
                   data = d,
                   family = binomial(link = "logit") )
  # p-value for entire missingness model
  ftest = anova(missMod, nullModel, test = "Chisq" )
  update_result_csv( name = paste( "Logit missingness model global pval" ),
                     value = format.pval(ftest$`Pr(>Chi)`[2], 2 ) )
  
  # probability of being missing by treatment group
  # not conditional on covariates
  t = d %>% group_by(treat) %>%
    summarise( Pretained = mean(!is.na(mainY) ) )
  
  update_result_csv( name = paste( "Retention perc overall" ),
                     value = round( mean(!is.na(d$mainY)) * 100, 0) )
  
  update_result_csv( name = paste( "Retention perc treat", t$treat ),
                     value = round(t$Pretained * 100, 0) )
  
}


if ( study == 1 ) {
  # follow-up times
  update_result_csv( name = "Perc fuDays 12 to 14",
                     value = round( mean(d$fuDays <= 12, na.rm = TRUE) * 100, 0) )
  
  update_result_csv( name = "Mean fuDays",
                     value = mean(d$fuDays, na.rm = TRUE) )
  
  update_result_csv( name = "Median fuDays",
                     value = median(d$fuDays, na.rm = TRUE) )
  
  update_result_csv( name = "Min fuDays",
                     value = min(d$fuDays, na.rm = TRUE) )
  
  update_result_csv( name = "Max fuDays",
                     value = max(d$fuDays, na.rm = TRUE) )
}




# ~ Table 1 (Demographics Among All Wave 1 Subjects) ------------------------------------------------

# stratify demographics by treatment group
t1.treat = make_table_one(.d = d %>% filter( treat == 1 ) )
t1.cntrl = make_table_one(.d = d %>% filter( treat == 0 ) )

# if not equal, it's because a level occurs in one stratum but not the other 
nrow(t1.treat)
nrow(t1.cntrl)
# find the offending row
t1.cntrl$Characteristic[ !t1.cntrl$Characteristic %in% t1.treat$Characteristic ]

# ad hoc fix for row mismatches
if ( study ==1 ){
  t1.treat = t1.treat %>% add_row( data.frame(Characteristic = "a.subHS",
                                              Summary = "0 (0%)"),
                                   .after = 7)
}


t1 = data.frame( Characteristic = t1.treat$Characteristic,
                 Intervention = t1.treat$Summary,
                 Control = t1.cntrl$Summary )


# save it
if( overwrite.res == TRUE ){
  setwd(results.dir)
  write.csv(t1, "table1.csv")
}


# ~ One-Off Stats for Paper ------------------------------------------------

# these are separate from the tables because they're specifically mentioned in the text
if ( study %in% c(1,3 ){
  
  # sex
  update_result_csv( name = "Perc female",
                     value = round( 100 * mean( d$female == 1 ), 0 ) )
  
  # age
  update_result_csv( name = "Age median",
                     value = round( median(d$age), 0 ) )
  
  # at least college-educated
  update_result_csv( name = "Perc educ at least college",
                     value = round( 100 * mean( d$educ %in% c("d.4yr", "e.post") ), 0 ) )
  
  # politics
  update_result_csv( name = "Perc Democrats",
                     value = round( 100 * mean( d$party == "Democrat" ), 0 ) )
  
  update_result_csv( name = "Perc Republicans",
                     value = round( 100 * mean( d$party == "Republican" ), 0 ) )
  
  update_result_csv( name = "Median county liberalism",
                     value = round( median( 100 * d$pDem ), 0 ) )
  
}


if ( study == 1) {
  # COVID effect on choices
  table(d$covid)
  update_result_csv( name = "Perc COVID less choice",
                     value = round( 100 * mean(d$covid[ !is.na(d$covid) ] %in% c("a.muchLess",
                                                                                 "b.somewhatLess",
                                                                                 "c.slightlyLess") ), 0 ) )
  
  update_result_csv( name = "Perc COVID more choice",
                     value = round( 100 * mean(d$covid[ !is.na(d$covid) ] %in% c("e.slightlyMore",
                                                                                 "f.somewhatMore",
                                                                                 "g.muchMore") ), 0 ) )
  
  update_result_csv( name = "Perc COVID no change",
                     value = round( 100 * mean(d$covid == "d.noChange", na.rm = TRUE), 0 ) )
}


if ( study %in% c(1,3)) {
  # attention check
  update_result_csv( name = "Perc videoContent animals treat 1",
                     value = round( 100 * mean( grepl(pattern = "animals", x = d$videoContent[d$treat == 1]) ), 0 ) )
  
  update_result_csv( name = "Perc videoContent animals treat 0",
                     value = round( 100 * mean( grepl(pattern = "animals", x = d$videoContent[d$treat == 0]) ), 0 ) )
  
  update_result_csv( name = "Perc pass check treat 1",
                     value = round( 100 * mean(d$passCheck[ d$treat == 1] == TRUE), 0 ) )
  
  update_result_csv( name = "Perc pass check treat 0",
                     value = round( 100 * mean(d$passCheck[ d$treat == 0] == TRUE), 0 ) )
  
}


#bm
table(d$treat, useNA = "ifany")

table( d$videoContent=="" )
table( d$videoContent[d$problemsBin ]=="" )


# ~ Plot Complete-Case Treatment Group Differences ------------------------------------------------

# examine skewed outcome (immaterial given sample size)
hist(dcc$mainY)

# treatment group differences...!
ggplot( data = dcc, 
        aes( x = treat.pretty,
             y = mainY ) ) +
  geom_violin(draw_quantiles = c(.25, .5, .75)) + 
  theme_bw()

setwd(results.dir)
ggsave( "descriptive_violin_mainY.pdf",
        height = 6,
        width = 6 )

# and for the intention outcome measures
if ( study == 2 ){
  # continuous intentions
  ggplot( data = dcc, 
          aes( x = treat.pretty,
               y = intentionCont ) ) +
    geom_violin(draw_quantiles = c(.25, .5, .75)) + 
    theme_bw()
  
  # binary intentions
  dcc %>% group_by(treat) %>%
    summarise( mean(intentionReduce) )
  
  setwd(results.dir)
  ggsave( "descriptive_violin_intentionReduce.pdf",
          height = 6,
          width = 6 )
}





# 4. TABLE 2: ALL TREATMENT EFFECTS (MI AND CC; ALL OUTCOMES)  ------------------------------------------------

# this script automatically knows that Study 2 also needs the intention outcome measures

# Primary analyses: We will conduct a 2-sample Welch’s t-test of total consumption by treatment group, reporting
# the mean difference, a 95% confidence interval, and a p-value treated as a continuous measure.
# We expect that errors may be highly skewed and heteroskedastic. The Welch’s t-test
# accommodates this heteroskedasticity and and is robust to error skewness by the Central
# Limit Theorem (Fagerland, 2012; Stapleton, 2009). We will therefore not transform the
# outcome to reduce skewness.

# Secondary outcomes: We will conduct a counterpart to the primary analysis for each
# secondary outcome, comprising the secondary consumption outcomes as well as the exploratory
# psychological outcomes. We will report inference for all secondary outcomes both with and
# without Bonferroni correction, counting one test per secondary outcome. As outcome-wide
# measures of the intervention’s effect on the secondary outcomes, we will report: (i) the
# harmonic mean p-values (Wilson, 2019) for all secondary food outcomes considered together,
# for all secondary food outcomes considered together, and for all exploratory psychological
# outcomes considered together; and (ii) the number of secondary outcomes with a Bonferronicorrected
# p < 0:05. The latter can be interpreted with 95% confidence as the number of
# secondary outcomes on which the intervention has an effect (VanderWeele & Mathur, 2019).


section = 4


if ( exists("res.raw") ) rm(res.raw)

# this fn also write key stats to stats_for_paper.csv
( res.CC = analyze_all_outcomes(missMethod = "CC") )
# to see results
# View(res.CC$res.nice)

# study 2 doesn't have any missing data, so doesn't get MI analyses
if ( study %in% c(1,3)) {
  ( res.MI = analyze_all_outcomes(missMethod = "MI") )
  # to see results
  # View(res.MI$res.nice)
}



# for pasting into TeX supplement
# maybe also Study 3?
if ( study %in% c(1,2) ) {
  
  setwd(results.dir)
  
  short = res.CC$res.nice %>% select(analysis,
                                     est,
                                     g.est,
                                     pval)
  write.table( print( xtable( short,
                              include.rownames = FALSE ) ),
               file = "table2_trt_effect_all_outcomes_cc_pretty_tex.txt"
  )
}




# ~~ Sanity Checks on Main Analyses


# Manually reproduce est and SE in res.raw for a single outcome


# reproduce MI results for study 3
# controls for targetDemographics
if ( run.sanity == TRUE & study == 3 ) {
  
  # names of outcomes to check
  toCheck = unlist( lapply( strsplit( res.MI$res.raw$analysis, " MI" ), function(x) x[[1]] ) )
  
  # check MI results (est and CI)
  for (yName in toCheck) {
    
    my.mi.res = lapply( imps, function(.imp) {
      string = paste( yName, " ~ treat + targetDemographics", sep = "" )
      # handle the weird one
      if ( yName == "mainY targetDemoSimple-subset") {
        string = paste( "mainY ~ treat + targetDemographics", sep = "" )
        .imp = .imp[ .imp$targetDemoSimple == TRUE, ]
      }
      
      ols = lm( eval( parse( text = string ) ), data = .imp )
      est = coef(ols)["treat"]
      
      # robust SE
      se = sqrt( vcovHC( ols, type="HC0")["treat", "treat"] )
      
      t = as.numeric( abs(est / se) )
      pval = 2 * ( 1 - pt(t, df = ols$df.residual) )
      
      # only extract this one coefficient
      return( data.frame( est = est, se = se, pval = pval ) )
    }  ) 
    
    my.mi.res = do.call( rbind, my.mi.res )
    
    # pool via Rubin's Rules and confirm above results
    M = length(imps)
    my.est = mean(my.mi.res$est)
    # between-imp variance
    B = var(my.mi.res$est)
    my.se = sqrt( mean(my.mi.res$se^2) + ( 1 + (1/M) ) * B )
    
    expect_equal( round( my.est, 2),
                  res.MI$res.raw$est[ res.MI$res.raw$analysis == paste( yName, "MI" ) ] )
    expect_equal( round( my.se, 2 ),
                  res.MI$res.raw$se[ res.MI$res.raw$analysis == paste( yName, "MI" ) ] )
    
    cat( paste("\nJust checked Study 3, MI, outcome", yName) )
  }
  
  # check CC results
  for (yName in toCheck) {
    dat = dcc
    string = paste( yName, " ~ treat + targetDemographics", sep = "" )
    # handle the weird one
    if ( yName == "mainY targetDemoSimple-subset") {
      string = paste( "mainY ~ treat + targetDemographics", sep = "" )
      dat = dat[ dat$targetDemoSimple == TRUE, ]
    }  
    
    
    ols = lm( eval( parse( text = string ) ), data = dat )
    est = coef(ols)["treat"]
    
    # robust SE
    se = sqrt( vcovHC( ols, type="HC0")["treat", "treat"] )
    
    expect_equal( as.numeric(round( est, 2)),
                  res.CC$res.raw$est[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    expect_equal( round( se, 2 ),
                  res.CC$res.raw$se[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    cat( paste("\nJust checked Study 3, CC, outcome", yName) )
  }
}




# ~~ One-Off Stats for Study 2 ------------------------------------------------

if ( study == 2 ) {
  res.raw = res.CC$res.raw
  
  # intentions in each group
  update_result_csv( name = "intentionCont mean cntrl",
                     value = round( res.raw$mn0[ res.raw$analysis == "intentionCont CC" ], 2 ) )
  
  update_result_csv( name = "Preduce cntrl",
                     value = round( 100 * mean( d$intentionReduce[d$treat ==0] ), 0 ) )
  
  update_result_csv( name = "intentionCont mean treat",
                     value = round( res.raw$mn1[ res.raw$analysis == "intentionCont CC" ], 2 ) )
  
  # sanity check
  mean( d$intentionCont[ d$treat == 0 ] )
  
  update_result_csv( name = "Preduce treat",
                     value = round( 100 * mean( d$intentionReduce[d$treat ==1] ), 0 ) )
  
  
  # continuous outcomes
  toReport = c("intentionCont CC", "mainY CC", "totalMeat CC", "totalAnimProd CC" )
  
  options( scipen = 999 )
  ( pvals = format.pval( res.raw$pval[ res.raw$analysis %in% toReport ],eps =  0.0001 ) )
  
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "diff" ),
                     value = round( res.raw$est[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "lo" ),
                     value = round( res.raw$lo[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "hi" ),
                     value = round( res.raw$hi[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "pval" ),
                     value = pvals )
  
  
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "g" ),
                     value = round( res.raw$g[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "g lo" ),
                     value = round( res.raw$g.lo[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "g hi" ),
                     value = round( res.raw$g.hi[ res.raw$analysis %in% toReport ], 2 ) )
  
  # binary outcome
  # this is actually a risk ratio even though it's in the "g" column
  
  update_result_csv( name = "intentionReduce RR",
                     value = round( res.raw$g[ res.raw$analysis == "intentionReduce CC" ], 2 ) )
  
  
  update_result_csv( name = "intentionReduce RR lo",
                     value = round( res.raw$g.lo[ res.raw$analysis == "intentionReduce CC" ], 2 ) )
  
  update_result_csv( name = "intentionReduce RR hi",
                     value = round( res.raw$g.hi[ res.raw$analysis == "intentionReduce CC" ], 2 ) )
  
  update_result_csv( name = "intentionReduce RR pval",
                     value = format.pval( res.raw$pval2[ res.raw$analysis == "intentionReduce CC" ], eps = 0.0001 ) )
  
  
}


# ~~ One-Off Stats for Study 3 ------------------------------------------------

# "We will report descriptively on the distribution of responses to the new intervention engagement items above, but will not include them in primary analyses. We may conduct post hoc exploratory analyses with these variables."

if ( study == 3 ) {
  
  # proportion of subjects making pledges 
  pledgeVars = names(dcc)[ grepl(pattern = "pledge", x = names(dcc)) ]
  pledgeVars = pledgeVars[ !pledgeVars %in% c("pledgeDateGoal", "pledgeStrategies", "pledgeStrategiesFreeText")]
  
  # percent of subjects RANDOMIZED to treat=1 who made a pledge about each food
  #  if someone was randomized, then dropped out of W1, but then came back for W2
  # conservatively count these people as not having made a pledge
  # these people have "" for the pledge
  t = sort( dcc %>%
            filter(treat == 1) %>%
            summarise_at( .vars = pledgeVars,
                          .funs = function(x) round( 100*mean( x %in% c("Yes, I pledge to eat this food less often",
                                                                        "Yes, I pledge to stop eating this food") ) ) ),
            decreasing = TRUE )
  
  # confirm sample sizes
  temp = dcc %>%
    filter(treat == 1) %>%
    summarise_at( .vars = pledgeVars,
                  .funs = function(x) length(x) )
  expect_equal( all.equal( unique( as.numeric(temp) ), sum(dcc$treat == 1) ),
                TRUE )
  
  t = sort(t)
  
  update_result_csv( name = paste( "Perc any pledge", names(t) ),
                     value = as.numeric(t) )
  
  
  # any "eliminate" pledge or any "reduce" pledge
  update_result_csv( name = paste( "Perc at least one pledge" ),
                     value = round( 100*mean(dcc$madeEliminatePledge[ dcc$treat == 1] == TRUE | dcc$madeReducePledge[ dcc$treat == 1] == TRUE ) ) )
  
  update_result_csv( name = paste( "Perc at least one eliminate pledge" ),
                     value = round( 100*mean(dcc$madeEliminatePledge[ dcc$treat == 1] == TRUE) ) )
  
  update_result_csv( name = paste( "Perc at least one reduce pledge" ),
                     value = round( 100*mean(dcc$madeReducePledge[ dcc$treat == 1] == TRUE) ) )
  
  
  # table( dcc$pledgeOtherMeat[dcc$treat==1],
  #        dcc$madeReducePledge[ dcc$treat == 1] )
  
}

#bm: stopped here with re-running Study 3 code :)

# for sanity check, maybe
# dcc %>%
#   filter(treat == 1) %>%
#   summarise( mean(pledgeChicken %in% c("Yes, I pledge to eat this food less often",
#                                        "Yes, I pledge to stop eating this food") ) )
# 
# 
# 
# # note that this could be equal to "" (i.e., not answered)
# #  if someone was randomized, then dropped out of W1, 
# #  but then came back for W2
# # conservatively count these people as not having made a pledge
# table(dcc$pledgeChicken[dcc$treat==1])
# (14+40)/(14+40+17+30)
# 
# 



# 5. TABLE 3: EFFECT MODIFIERS ------------------------------------------------

# Effect modifiers: We will examine two-way interactions of intervention group assignment
# with each of the following variables from the baseline demographic data: sex, age, race/ethnicity,
# individual political affiliation, education level, and the political liberalism vs. conservatism
# of the participant’s current county of residence. We will use the primary consumption outcome
# for this analysis. For the latter, we will use an existing database (MIT Election Data and
# Science Lab, 2018) to calculate the proportion of voters in the participant’s county who voted
# for the Democratic presidential candidate from among all voters who voted for either the
# Democratic or the Republican candidate. To do so, we will include these candidate effect
# modifiers simultaneously in a generalized least-squares model with heteroskedasticity-consistent
# robust standard errors, which is similar to the Welch’s t-test for the multivariable
# case. We anticipate that some effect modifiers may have very few observations in some
# categories (e.g., race/ethnicity). As needed, we may collapse variables into fewer categories
# for these analyses (e.g., Causasian vs. non-Caucasian), with the categories determined by
# the distribution of responses, and/or exclude categories with relatively few responses (e.g.,
# political Independents). We will report the point estimates for each two-way interaction,
# again reporting inference both with and without Bonferroni correction (counting one test per
# effect modifier regression coefficient). We will also report the combination of effect modifiers
# that was associated with the largest effect size, representing the participant demographic to
# which the intervention might best be targeted.


section = 5


# look at effect modifiers as they'll be coded in analysis
CreateTableOne( vars = effect.mods, 
                data = dcc,
                includeNA = TRUE)  # last only works for NA


# ~ Multiple Imputation Effect Modification Analysis ------------------------------------------------

if ( exists("res.raw") ) rm(res.raw)

# Study 2 doesn't use MI, so is handled in next section
if ( study %in% c(1,3) ) {
  
  yName = "mainY"
  
  # for each imputation, fit one model with all effect modifiers
  # returns a list of lm objects, one for each imputation
  mi.res = lapply( imps, function(.imp) {
    # fit one model with all effect modifiers
    if ( study == 1 ) string = paste( yName, " ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
    
    # for Study 3, also need to control for "young", a var used in randomization that wasn't
    #  part of the simplified effect modifier
    if ( study == 3 ) string = paste( yName, " ~ ",
                                      paste( "young + treat*", effect.mods, collapse=" + "),
                                      sep = "" )
    
    ols = lm( eval( parse( text = string ) ), data = .imp )
    
    my_ols_hc0_all( dat = .imp, ols = ols, yName = yName )
    
  }  ) 
  
  
  res.raw = mi_pool_all(.mi.res = mi.res)
  
  # ~~ Save Both Raw and Cleaned-Up Results Tables ----
  # in order to have the unrounded values
  setwd(results.dir)
  write.csv(res.raw, "effect_mods_MI.csv")
  
  # cleaned-up version
  # round it
  # save row names before they get removed by dplyr
  rowNames = row.names(res.raw)
  # rounded version
  res.raw2 = res.raw %>% mutate_at( names(res.raw), function(x) round(x,2) )
  
  res.nice = data.frame( coef = rowNames,
                         est = stat_CI( res.raw2$est, res.raw2$lo, res.raw2$hi),
                         g.est = stat_CI( res.raw2$g.est, res.raw2$g.lo, res.raw2$g.hi),
                         pval = res.raw2$pval,
                         pvalBonf = res.raw2$pvalBonf )
  
  
  setwd(results.dir)
  write.csv(res.nice, "effect_mods_MI_pretty.csv")
  
  
  # ~~ Sanity Check ----
  # manually reproduce all results for a single coefficient
  if ( study == 1 & run.sanity == TRUE ) {
    # coefficient index for treat is 2 (including intercept)
    i = 2
    
    my.mi.res = lapply( imps, function(.imp) {
      string = paste( yName, " ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
      
      
      ols = lm( eval( parse( text = string ) ), data = .imp )
      est = coef(ols)[i]
      
      # robust SE
      se = sqrt( vcovHC( ols, type="HC0")[i, i] )
      
      t = as.numeric( abs(est / se) )
      pval = 2 * ( 1 - pt(t, df = ols$df.residual) )
      
      # only extract this one coefficient
      return( data.frame( est = est, se = se, pval = pval ) )
    }  ) 
    
    my.mi.res = do.call( rbind, my.mi.res )
    
    # check within-imp SEs
    existing.ses = unlist( lapply( mi.res, FUN = function(table) table[i, "se"]) )
    expect_equal( existing.ses, my.mi.res$se )
    
    # check between-imp variance
    B = var(my.mi.res$est)
    existing.B = var( unlist( lapply( mi.res, FUN = function(table) table[i, "est"]) ) )
    expect_equal(existing.B, B)
    
    # pool via Rubin's Rules and confirm above results
    M = length(imps)
    my.est = mean(my.mi.res$est)
    
    my.se = sqrt( mean(my.mi.res$se^2) + ( 1 + (1/M) ) * B )
    
    expect_equal( my.est, res.raw2$est[i], tol = 0.001 )
    expect_equal( my.se, res.raw2$se[i], tol = 0.001 )
  }
  
  if ( study == 3 & run.sanity == TRUE ) {
    
    my.mi.res = lapply( imps, function(.imp) {
      
      string = "mainY ~ young + treat*targetDemoSimple"
      ols = lm( eval( parse( text = string ) ), data = .imp )
      est = coef(ols)["treat:targetDemoSimpleTRUE"]
      
      # robust SE
      se = sqrt( vcovHC( ols, type="HC0")["treat:targetDemoSimpleTRUE", "treat:targetDemoSimpleTRUE"] )
      
      t = as.numeric( abs(est / se) )
      pval = 2 * ( 1 - pt(t, df = ols$df.residual) )
      
      # only extract this one coefficient
      return( data.frame( est = est, se = se, pval = pval ) )
    }  ) 
    
    my.mi.res = do.call( rbind, my.mi.res )
    
    # pool via Rubin's Rules and confirm above results
    M = length(imps)
    my.est = mean(my.mi.res$est)
    # between-imp variance
    B = var(my.mi.res$est)
    my.se = sqrt( mean(my.mi.res$se^2) + ( 1 + (1/M) ) * B )
    
    expect_equal( my.est, res.raw$est["treat:targetDemoSimpleTRUE"] )
    expect_equal( my.se, res.raw$se["treat:targetDemoSimpleTRUE"] )
  }
  
  # ~ One-Off Stats for Paper ----
  
  # best combo of effect modifiers
  if ( study == 1 ){
    
    # number of moderator coefficients we estimated
    nMods = sum( !is.na(res.raw$pvalBonf) ) 
    alpha3 = 0.05 / nMods
    update_result_csv( name = "Bonferroni alpha mods",
                       value = round( alpha3, 4 ) )
    
    update_result_csv( name = "Number mods pass Bonf",
                       value = sum( res.raw$pvalBonf[ res.raw$group == "mod" ] < 0.05 ) )
    
    
    varNames = row.names(res.raw)[ grepl( x = row.names(res.raw), pattern = ":" ) ]
    # can't count coefficients for both political categories
    varNames = varNames[ !varNames == "treat:party2b.Neutral"]
    
    if ( any( res.raw$est[ row.names(res.raw) %in% varNames ] > 0 ) ) {
      stop("Calculation of best effect modifiers below won't work because some coefs were positive!")
    }
    est.best = sum( res.raw$est[ row.names(res.raw) %in% varNames ] )
    g.best = sum( res.raw$g.est[ row.names(res.raw) %in% varNames ] )
    
    # use abs value for easier reporting in paper
    update_result_csv( name = "Best effect mods absolute est",
                       value = round( abs(est.best), 2 ) )
    
    
    update_result_csv( name = "Best effect mods absolute g",
                       value = round( abs(g.best), 2 ) )
  }
  
  
}  # end "if (study %in% c(1,3) )"





# ~ Complete-Case Effect Modification Analysis  ------------------------------------------------


# for Study 1, this is a sensitivity analysis
# for Study 2 (no missing data), this is the only analysis

if ( study == 1 ) yName = "mainY"
if ( study == 2 ) yName = "intentionCont"


coefNames = paste("treat:", effect.mods, sep = "")

string = paste( yName, " ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
ols = lm( eval( parse( text = string ) ), data = d )



res.raw = my_ols_hc0_all( dat = d, ols = ols, yName = yName )

setwd(results.dir)
write.csv(res.raw, "effect_mods_cc.csv")


# cleaned-up version
# round it
# save row names before they get removed by dplyr
rowNames = row.names(res.raw)
res.raw2 = res.raw %>% mutate_at( names(res.raw), function(x) round(x,2) )

res.nice = data.frame( coef = rowNames,
                       est = stat_CI( res.raw2$est, res.raw2$lo, res.raw2$hi),
                       g.est = stat_CI( res.raw2$g, res.raw2$g.lo, res.raw2$g.hi),
                       pval = res.raw2$pval )


setwd(results.dir)
write.csv(res.nice, "effect_mods_cc_pretty.csv")

# for pasting into Supplement
if ( study == 2 ) {
  
  setwd(results.dir)
  
  write.table( print( xtable( res.nice,
                              include.rownames = FALSE ) ),
               file = "supp_table2_effect_mods_cc_pretty_tex.txt"
  )
}

#bm: stopped here because not sure why results aren't matching supplement


# ~ Sanity Check: Compare CC to MI ------------------------------------------------

if ( (study == 1) & run.sanity == TRUE ) {
  setwd(results.dir)
  res.cc = read.csv("effect_mods_cc.csv")
  res.mi = read.csv("effect_mods_mi.csv")
  row.names(res.cc) == row.names(res.mi)
  
  ggplot( data = data.frame( cc = res.cc$est, mi = res.mi$est ),
          aes( x = cc, y = mi ) ) +
    theme_bw() + 
    geom_abline( slope = 1, intercept = 0, lty = 2, color = "gray" ) +
    geom_point()
  
  setwd(results.dir)
  ggsave( "CC_vs_MI_effect_mod_ests.pdf",
          height = 6,
          width = 6 )
  
  ggplot( data = data.frame( cc = res.cc$se, mi = res.mi$se ),
          aes( x = cc, y = mi ) ) +
    theme_bw() + 
    geom_abline( slope = 1, intercept = 0, lty = 2, color = "gray" ) +
    geom_point()
  
  setwd(results.dir)
  ggsave( "CC_vs_MI_effect_mod_SEs.pdf",
          height = 6,
          width = 6 )
  
  summary( res.mi$se / res.cc$se )
  
  # estimates are very similar, which makes sense given
  #  how little missing data there is
  # SEs larger by about 20% with MI 
  
}




# SUPPLEMENT: SENSITIVITY ANALYSES ------------------------------------------------


# ~ SUBJECT AWARENESS ------------------------------------------------ 

# To assess the possible extent of subject awareness of the intervention,
# we will report the proportions of subjects answering the probe correctly within each treatment
# group. Because the awareness probe will be completed after outcome measurement, we
# will not condition on subject awareness in analysis (e.g., via subset analyses or covariate
# adjustment) to avoid inducing collider bias.



update_result_csv( name = "Perc aware tx group",
                   value = round( 100 * mean(dcc$aware[ dcc$treat == 1 ], na.rm = TRUE ), 0 ) )

update_result_csv( name = "Perc aware cntrl group",
                   value = round( 100 * mean(dcc$aware[ dcc$treat == 0 ], na.rm = TRUE ), 0 ) )



# ~ NON-DIFFERENTIAL MEASUREMENT ERROR ------------------------------------------------

# Anticipating that there may be more non-differential
# measurement error in subjects’ reporting of serving sizes than in their reporting of consumption
# frequencies (e.g., because subjects have difficulty estimating volumes of food), we will
# repeat the primary analysis using only frequencies, rather than total amounts consumed, as
# the outcome.

if ( study == 1 ) {
  # repeat main, MI analysis with the dichotomized outcome
  mi.res = lapply( imps, function(.d) my_ttest(yName = "mainYFreqOnly", dat = .d) )
  mi.res = do.call(what = rbind, mi.res)
  raw = mi_pool(ests = mi.res$est, ses = mi.res$se) 
  SMD = mi_pool(ests = mi.res$g, ses = mi.res$g.se) 
  
  
  update_result_csv( name = "mainYFreqOnly diff",
                     value = round( raw$est, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly lo",
                     value = round( raw$lo, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly hi",
                     value = round( raw$hi, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly pval",
                     value = format_pval( raw$pval, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly diff g",
                     value = round( SMD$est, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly lo g",
                     value = round( SMD$lo, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly hi g",
                     value = round( SMD$hi, 2 ) )
}


# ~ EFFECTS OF INTERVENTION NONCOMPLIANCE ------------------------------------------------ 

# To supplement the primary analyses conducted
# by intention to treat, we will account for possible noncompliance with the intervention by
# treating intervention assignment as an instrumental variable for passing the manipulation
# check (Angrist et al., 1996). This analysis estimates a local average treatment effect (Angrist
# et al., 1996) under the exclusion restriction assumption. Because this assumption may
# be violated for manipulation-check items, we may instead conduct a similar instrumental
# variables analysis using methods that relax the exclusion restriction (Flores & Flores-Lagunes,
# 2013) if relevant methodological extensions, currently underway, are ready at the time of
# analysis.

# I think this is only relevant for Study 1? And maybe 3?

if ( study == 1 ) {
  # ~~ Look at CC Data ------------------------------------------------
  # look at relationship between instrument (treat) and X (video duration)
  # in CC data
  dcc %>% group_by(treat) %>%
    summarise( sum( passCheck, na.rm = TRUE) )
  table(dcc$treat, dcc$passCheck)
  # first-stage model (linear probability model; ignore the inference):
  summary( lm( passCheck ~ treat, data = dcc) )
  
  my_ivreg(dat = dcc)
  
  
  # ~~ IV for MI Datasets ------------------------------------------------
  mi.res = lapply( imps, function(.d) my_ivreg(dat = .d) )
  mi.res = do.call(what = rbind, mi.res)
  raw = mi_pool(ests = mi.res$est, ses = mi.res$se) 
  SMD = mi_pool(ests = mi.res$g, ses = mi.res$g.se) 
  
  # look at this manually to make sure we don't have a weak instrument
  #  (though that seems inconceivable in this case):
  mi.res$stage1.pval
  
  # sanity check
  # confirm the fact that the first-stage model has the same p-value for every imputation
  # first-stage model (linear probability model; ignore the inference):
  summary( lm( passCheck ~ treat, data = imps[[3]]) )
  # this seems to be where where the weak instruments p-value is coming from
  summary( ivreg(mainY ~ passCheck | treat, data = imps[[3]]), diagnostics = TRUE )
  
  
  
  update_result_csv( name = "mainY IV diff",
                     value = round( raw$est, 2 ) )
  
  update_result_csv( name = "mainY IV lo",
                     value = round( raw$lo, 2 ) )
  
  update_result_csv( name = "mainY IV hi",
                     value = round( raw$hi, 2 ) )
  
  update_result_csv( name = "mainY IV pval",
                     value = format_pval( raw$pval, 3 ) )
  
  update_result_csv( name = "mainY IV g",
                     value = round( SMD$est, 2 ) )
  
  update_result_csv( name = "mainY IV g lo",
                     value = round( SMD$lo, 2 ) )
  
  update_result_csv( name = "mainY IV g hi",
                     value = round( SMD$hi, 2 ) )
  
  
}





# ~ DUMB VERSIONS OF MAIN ANALYSES ------------------------------------------------

# CC only
# these are just as additional sanity checks and won't exactly match the results in paper

if ( study == 3 ) {
  
  # mainY treatment effect
  
  # targetDemoSimple subset
  
  #bm: stopped here
}








