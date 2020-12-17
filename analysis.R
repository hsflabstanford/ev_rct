
# Ideas:
#   - how much nondiff measurement error would be needed for true effect to be some non-null amount?


rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      0. PRELIMINARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Packages #####
library(dplyr)
library(readr)
library(tableone)
library(ggplot2)
library(tibble)
library(sandwich)
library(EValue)
library(metafor)
library(AER)
library(harmonicmeanp)

# which study to analyze (1 or 2)?
study = 1

##### Working Directories #####

overleaf.dir = "~/Dropbox/Apps/Overleaf/EatingVeg manuscript/R_objects"
# results dir not specific to this study (for saving results csv)
general.results.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Results from R"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"


if ( study == 1 ) {
  prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped/Study 1"
  results.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Results from R/Study 1"
  imputed.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped/Study 1/Saved imputations"
}

if (study == 2) {
  prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped/Study 2"
  results.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Results from R/Study 2"
}


setwd(code.dir)
source("helper_analysis.R")

overwrite.res = TRUE

# res.raw will be a table of estimates for main outcome, secondaries, effect modifiers
# res.overleaf will be individual stats formatted for piping into Overleaf
if ( overwrite.res == TRUE & exists("res.raw") ) rm(res.raw)
if ( overwrite.res == TRUE & exists("res.overleaf") ) rm(res.raw)



##### Dataset #####
setwd(prepped.data.dir)
if (study == 1) d = read.csv("prepped_merged_data.csv")
if (study == 2) d = read.csv("prepped_data.csv")
table(is.na(d$beef))

# complete cases wrt mainY
# might still have sporadic missing data elsewhere
dcc = d %>% filter( !is.na(mainY) )
nrow(dcc)


# read in imputations
if ( study == 1 ) {
  #setwd(imputed.data.dir)
  #load("imputed_datasets.RData")  # load "imps", the mids object
  
  # read in the imputations as a list rather than a mids object so that we can pool manually
  setwd(imputed.data.dir)
  to.read = list.files()[ grepl( pattern = "prepped", x = list.files() ) ]
  imps = lapply( to.read,
                 function(x) suppressMessages(read_csv(x)) )
}

# @maybe unnecessary?
# study 2 doesn't have imputations
#  to enable running the same script, just put the complete-case dataset as the imputations
if ( study == 2 ) {
  imps = list(dcc)
}


##### Lists of Variables #####
meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)

foodVars = c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
              names(d)[ grepl(pattern = "Ounces", names(d) ) ] )

# exploratory psych variables
psychY = c("importHealth",
           "importEnviro",
           "importAnimals",
           "activ",
           "spec",
           "dom")

# secondary food outcomes
secFoodY = c("totalMeat",
             "totalAnimProd",
             meats,
             animProds,
             "totalGood")

# # not used?
# # @ do we need to add covid to this?
# fu.vars = c(foodVars, psychY, "aware" )

# raw demographics, prior to collapsing categories for effect modification analyses
demo.raw = c("sex",
             "age",
             "educ",
             "cauc",
             "hisp",
             "black",
             "midEast",
             "pacIsl",
             "natAm",
             "SAsian",
             "EAsian",
             "SEAsian",
             "party",
             "pDem")

effect.mods = c("female",
                "young",
                "collegeGrad",
                "cauc",  
                "party2",
                "pDem2")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 OBSOLETE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# ##### Quick Analyses #####
# # @ quick look at treatment effect!!!
# hist(d$mainY)
# t.test( d$mainY ~ d$treat, na.rm = TRUE  )
# # in study 2, no evidence of lying about past behavior

# redundant with below
# # and on intentions
# if ( study == 2 ) {
#   # big difference in intentions! 
#   t.test(d$intentionCont ~ d$treat, na.rm = TRUE)
#   # **Cohen's d: -0.64 [-0.88, -0.41]
#   library(effsize)
#   es = cohen.d( d$intentionCont ~ (d$treat==0), hedges.correction = TRUE )
#   
#   # binary version
#   tab = table( d$treat.pretty, d$intentionReduce )
# 
#   library(metafor)
#   es = escalc( measure = "RR",
#                ai = tab["Documentary", 2], # X=1, Y=1
#                bi = tab["Documentary", 1],  # X=1, Y=0
#                ci = tab["Control", 2], # X=0, Y=1
#                di = tab["Control", 1] ) # X=0, Y=0
#   # this is the LOG-RR
#   exp( es$yi - qnorm(.975) * sqrt(es$vi) )
#   exp( es$yi + qnorm(.975) * sqrt(es$vi) )
#   
#   # another way
#   mod = glm( intentionReduce ~ treat,
#        data = d,
#        family = binomial(link="log") )
#   # **RR: 3.42 [2.49, 4.92]
#   exp(mod$coefficients)
#   exp(confint(mod))
#   
#   # wow...HUGE ES on intentions!
#   
# }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   1. SANITY CHECKS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# table one - wave 1
CreateTableOne( vars = c(demo.raw), strata = "treat", data = dcc )


# # dropout 
# # completers
# CreateTableOne( vars = c(demo.raw, "passCheck", "aware"), strata = "treat", data = d %>% filter( !is.na(mainY)))

# sanity check: treatment effects
# main and secondaries t-tests
CreateTableOne( vars = c("mainY", secFoodY, psychY), strata = "treat", data = d )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 2. DESCRIPTIVE & TABLE 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# for keeping results csv organized
section = 2

##### Sample Sizes and Retention (For Study 1) #####
update_result_csv( name = paste( "N wave 1 study", study ),
                   value = nrow(d) )


if ( study == 1 ){
  
  update_result_csv( name = paste( "N wave 2 study", study ),
                     value = nrow(dcc) )
  
  # differential attrition by treatment group and demographics
  string = paste( "!is.na(d$mainY) ~ ", paste( "treat +", effect.mods, collapse=" + "), sep = "" )
  missMod = glm( eval( parse( text = string ) ), family = binomial(link="log"), data = d )
  summary(missMod)
  library(sandwich)
  coeftest(missMod, vcov = vcovHC(missMod, type = "HC0"))
  # more likely to drop out: older subjects, college grads
  
  # probability of being missing by treatment group
  # not conditional on covariates
  t = d %>% group_by(treat) %>%
    summarise( Pmiss = mean(!is.na(mainY) ) )
  
  update_result_csv( name = paste( "Retention perc overall" ),
                     value = round( mean(!is.na(d$mainY)) * 100, 0) )
  
  update_result_csv( name = paste( "Retention perc treat", t$treat ),
                     value = round(t$Pmiss * 100, 0) )
}

##### Table 1 (Demographics Among All Wave 1 Subjects) #####
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


##### One-Off Stats for Paper #####
# @@not yet reported in paper
# COVID influence on food choices (given at wave 2)
if ( study == 1 ){
  
  # demographics to comment on specifically
  # age
  update_result_csv( name = "Age median study 1",
                     value = round( median(d$age), 0 ) )
  
  # at least college-educated
  update_result_csv( name = "Perc educ at least college study 1",
                     value = round( 100 * mean( d$educ %in% c("d.4yr", "e.post") ), 0 ) )
  
  # politics
  update_result_csv( name = "Perc Democrats study 1",
                     value = round( 100 * mean( d$party == "Democrat" ), 0 ) )
  
  update_result_csv( name = "Perc Republicans study 1",
                     value = round( 100 * mean( d$party == "Republican" ), 0 ) )
  
  update_result_csv( name = "Median county liberalism study 1",
                     value = round( median( 100 * d$pDem ), 0 ) )
  
  
  # COVID effect on choices
  table(d$covid)
  update_result_csv( name = "Perc COVID less choice",
                     value = round( 100 * mean(d$covid %in% c("a.muchLess",
                                                              "b.somewhatLess",
                                                              "c.slightlyLess"), na.rm = TRUE), 0 ) )
  
  update_result_csv( name = "Perc COVID more choice",
                     value = round( 100 * mean(d$covid %in% c("e.slightlyMore",
                                                              "f.somewhatMore",
                                                              "g.muchMore") ), 0 ) )
  
  update_result_csv( name = "Perc COVID no change",
                     value = round( 100 * mean(d$covid == "d.noChange", na.rm = TRUE), 0 ) )
  
  # attention check
  update_result_csv( name = "Perc videoContent animals treat 1 study 1",
                     value = round( 100 * mean( grepl(pattern = "animals", x = d$videoContent[d$treat == 1]) ), 0 ) )
  
  update_result_csv( name = "Perc videoContent animals treat 0 study 1",
                     value = round( 100 * mean( grepl(pattern = "animals", x = d$videoContent[d$treat == 0]) ), 0 ) )
  
  update_result_csv( name = "Perc pass check treat 1 study 1",
                     value = round( 100 * mean(d$passCheck[ d$treat == 1] == TRUE), 0 ) )
  
  update_result_csv( name = "Perc pass check treat 0 study 1",
                     value = round( 100 * mean(d$passCheck[ d$treat == 0] == TRUE), 0 ) )
  
}


##### Descriptive Look at Complete-Case Treatment Group Differences #####

# examine skewed outcome (immaterial given sample size)
hist(dcc$mainY)

# treatment group differences...!
ggplot( data = dcc, 
        aes( x = treat.pretty,
             y = mainY ) ) +
  geom_violin(draw_quantiles = c(.25, .5, .75)) + 
  theme_bw()

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
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                            3. COMPLETE-CASE ANALYSES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

section = 3

# for Study 1, these are just a sensitivity analysis and sanity check
# for Study 2, they are the main analysis

##### All Outcomes #####

if ( study == 1 ) vars = c("mainY", secFoodY, psychY)
if ( study == 2 ) vars = c("intentionCont", "mainY", secFoodY, psychY)

rm(res.raw)
for ( i in vars ){
  
  test = t.test( d[[i]] ~ d$treat, na.rm = TRUE  )
  
  m0 = mean( d[[i]][ d$treat == 0], na.rm = TRUE )
  m1 = mean( d[[i]][ d$treat == 1], na.rm = TRUE )
  
  # Hedges' g
  # recode to be more intuitive
  # as.factor in RHS to avoid annoying warnings
  library(effsize)
  es = cohen.d( d[[i]] ~ as.factor(d$treat==0), hedges.correction = TRUE )
  
  new.row = data.frame( outcome = i,
                        mn0 = m0,
                        mn1 = m1,
                        med0 = median( d[[i]][ d$treat == 0], na.rm = TRUE ),
                        med1 = median( d[[i]][ d$treat == 1], na.rm = TRUE ),
                        mn.diff = m1-m0,
                        lo = test$conf.int[1],
                        hi = test$conf.int[2],
                        pval = test$p.value,
                        g = es$estimate,
                        g.lo = es$conf.int[1],
                        g.hi = es$conf.int[2],
                        pval2 = NA, # NA because will be redundant with previous p-value for continuous outcomes
                        note = NA) 
  if ( !exists("res.raw") ) res.raw = new.row else res.raw = rbind(res.raw, new.row)
}



# for Study 2: handle binary intention variable differently
if ( study == 2 ){
  # get inference for risk difference
  mod1 = lm( intentionReduce ~ treat,
             data = d )
  mod1Inf = coeftest(mod1, vcov = vcovHC(mod1, type = "HC0"))["treat",]
  df = nrow(d) - 2
  tcrit = qt(p = 0.975, df = df)
  
  # get estimate and inference for RR
  mod2 = glm( intentionReduce ~ treat,
              data = d,
              family = binomial(link="log") )
  mod2Inf = confint(mod2)
  
  new.row = data.frame( outcome = "intentionReduce",
                        mn0 = mean( d$intentionReduce[d$treat == 0] ),
                        mn1 = mean( d$intentionReduce[d$treat == 1] ),
                        med0 = NA,
                        med1 = NA,
                        mn.diff = m1-m0,
                        lo = mod1Inf["Estimate"] - mod1Inf["Std. Error"] * tcrit,
                        hi = mod1Inf["Estimate"] + mod1Inf["Std. Error"] * tcrit,
                        pval = mod1Inf["Pr(>|t|)"],
                        g = exp( mod2$coef["treat"] ),
                        g.lo = exp( mod2Inf["treat", "2.5 %"] ),
                        g.hi = exp( mod2Inf["treat", "97.5 %"] ),
                        pval2 = summary(mod2)$coefficients["treat","Pr(>|z|)"],
                        note = "Binary outcome, so pval is from robust SEs and g's are actually risk ratios" )
  res.raw = rbind(res.raw, new.row)
}


##### Save Both Raw and Cleaned-Up Results Tables #####

# in order to have the unrounded values
setwd(results.dir)
write.csv(res.raw, "trt_effect_all_outcomes_cc.csv")

# cleaned-up version
# round it
res.raw = res.raw %>% mutate_at( names(res.raw)[ !names(res.raw) %in% c("outcome", "note" ) ], function(x) round(x,2) )

res.nice = data.frame( analysis = res.raw$outcome,
                       est = stat_CI( res.raw$mn.diff, res.raw$lo, res.raw$hi),
                       g.est = stat_CI( res.raw$g, res.raw$g.lo, res.raw$g.hi),
                       pval = res.raw$pval )


setwd(results.dir)
write.csv(res.nice, "trt_effect_all_outcomes_cc_pretty.csv")

# for pasting into TeX supplement
library(xtable)
print( xtable( res.nice,
               include.rownames = FALSE ) )




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                    4. TABLE 2: MAIN ANALYSIS AND ALL SECONDARY FOOD OUTCOMES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

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



##### Analyze Each Outcome (Including Primary) #####

# for Bonferroni
n.secY = sum( length(secFoodY), length(psychY) )
( alpha2 = 0.05 / n.secY ) # Bonferroni-adjusted alpha

# variables to analyze
toAnalyze = c("mainY", secFoodY, psychY )
if ( study == 2 ) toAnalyze = c( "intentionCont", toAnalyze )



if ( exists("res.raw") ) rm(res.raw)
for ( i in toAnalyze ) {
  mi.res = lapply( imps, function(.d) my_ttest(yName = i, dat = .d) )
  mi.res = do.call(what = rbind, mi.res)
  
  part1 = mi_pool(ests = mi.res$est, ses = mi.res$se)
  part2 = mi_pool(ests = mi.res$g, ses = mi.res$g.se)
  names(part2) = paste( "g.", names(part2), sep = "" )
  new.row = cbind(part1, part2)
  
  
  # Bonferroni-corrected p-value
  if( i %in% c(secFoodY, psychY) ) {
    new.row$pvalBonf = min( 1, new.row$pval * n.secY )
    new.row$group = "secY"
    
    if( i %in% secFoodY) new.row$group.specific = "secY food"
    if( i %in% psychY) new.row$group.specific = "secY psych"
    
  } else if (i == "mainY") {
    # for primary outcome
    new.row$pvalBonf = NA
    new.row$group = "mainY"
    new.row$group.specific = "mainY"
  }
  
  # add name of this analysis
  string = paste(i, " MI", sep = "")
  new.row = add_column(new.row, analysis = string, .before = 1)
  
  if ( !exists("res.raw") ) res.raw = new.row else res.raw = rbind(res.raw, new.row)
}

res.raw

##### Save Both Raw and Cleaned-Up Results Tables #####

# in order to have the unrounded values
setwd(results.dir)
write.csv(res.raw, "trt_effect_all_outcomes_mi.csv")

# cleaned-up version
# round it
res.raw = res.raw %>% mutate_at( names(res.raw)[ !names(res.raw) %in% c("analysis", "group", "group.specific" ) ], function(x) round(x,2) )

res.nice = data.frame( analysis = res.raw$analysis,
                       est = stat_CI( res.raw$est, res.raw$lo, res.raw$hi),
                       g.est = stat_CI( res.raw$g.est, res.raw$g.lo, res.raw$g.hi),
                       pval = res.raw$pval,
                       pvalBonf = res.raw$pvalBonf )


setwd(results.dir)
write.csv(res.nice, "table2_trt_effect_all_outcomes_mi_pretty.csv")


##### One-Off Stats for Paper: Main Estimates #####
update_result_csv( name = "mainY diff",
                   value = round( res.raw$est[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff lo",
                   value = round( res.raw$lo[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff hi",
                   value = round( res.raw$hi[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff pval",
                   value = format_pval( res.raw$pval[ res.raw$analysis == "mainY MI"], 2 ),
                   print = TRUE )

update_result_csv( name = "mainY diff g",
                   value = round( res.raw$g.est[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff g lo",
                   value = round( res.raw$g.lo[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff g hi",
                   value = round( res.raw$g.hi[ res.raw$analysis == "mainY MI"], 2 ) )


##### One-Off Stats for Paper: Various Multiple-Testing Metrics for Secondary Outcomes #####
update_result_csv( name = "Bonferroni alpha secY",
                   value = round( alpha2, 4 ) )

update_result_csv( name = "Bonferroni number secY",
                   value = n.secY )

update_result_csv( name = "Number secY pass Bonf",
                   value = sum( res.raw$pvalBonf[ res.raw$group == "secY" ] < 0.05 ) )

# harmonic mean p-values by subsets of effect modifiers
update_result_csv( name = "HMP all secY",
                   value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group == "secY" ],
                                               L = sum(res.raw$group == "secY") ), 2 ) )

update_result_csv( name = "HMP food secY",
                   value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY food" ],
                                               L = sum(res.raw$group.specific == "secY food") ), 2 ) )

update_result_csv( name = "HMP psych secY",
                   value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY psych" ],
                                               L = sum(res.raw$group.specific == "secY psych") ), 2 ) )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                               TABLE 3: EFFECT MODIFIERS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

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



# look at effect modifiers as they'll be coded in analysis
CreateTableOne( vars = effect.mods, 
                data = dcc,
                includeNA = TRUE)  # last only works for NA

# # for Bonferroni
# n.mods = length(effect.mods)
# ( alpha3 = 0.05 / n.mods ) # Bonferroni-adjusted alpha
# 
# # names of coefficient estimates as they'll appear in the model
# coefNames = c("treat:female",
#               "treat:youngTRUE",
#               "treat:collegeGradTRUE",
#               "treat:cauc",
#               "treat:party2b.Democrat",
#               "treat:party2c.Republican",
#               "treat:pDem")


##### Multiple Imputation Effect Modification Analysis #####

# outcome depends on study
#@actually not necessary becase study 2 won't use MI
if ( study == 1 ) yName = "mainY"
#if ( study == 2 ) yName = "intentionCont" #@spelled right?


if ( exists("res.raw") ) rm(res.raw)


if ( study == 1 ) {
  
  # for each imputation, fit one model with all effect modifiers
  # returns a list of lm objects, one for each imputation
  mi.res = lapply( imps, function(.imp) {
    # fit one model with all effect modifiers
    string = paste( yName, " ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
    ols = lm( eval( parse( text = string ) ), data = .imp )
    
    my_ols_hc0_all( dat = .imp, ols = ols, yName = yName )
    
  }  ) 
  
  
  res.raw = mi_pool_all(.mi.res = mi.res)
  
  ##### Save Both Raw and Cleaned-Up Results Tables #####
  # in order to have the unrounded values
  setwd(results.dir)
  write.csv(res.raw, "effect_mods_mi.csv")
  
  # cleaned-up version
  # round it
  # save row names before they get removed by dplyr
  rowNames = row.names(res.raw)
  res.raw2 = res.raw %>% mutate_at( names(res.raw), function(x) round(x,2) )
  
  res.nice = data.frame( coef = rowNames,
                         est = stat_CI( res.raw2$est, res.raw2$lo, res.raw2$hi),
                         g.est = stat_CI( res.raw2$g.est, res.raw2$g.lo, res.raw2$g.hi),
                         pval = res.raw2$pval,
                         pvalBonf = res.raw2$pvalBonf
  )
  
  
  setwd(results.dir)
  write.csv(res.nice, "effect_mods_mi_pretty.csv")
  
  
  ##### Sanity Check #####
  # manually reproduce all results for a single coefficient
  if ( run.sanity == TRUE ) {
    # which coefficient index (including intercept)
    i = 2
    
    my.mi.res = lapply( imps, function(.imp) {
      # fit one model with all effect modifiers
      string = paste( yName, " ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
      ols = lm( eval( parse( text = string ) ), data = .imp )
      
      # robust SE
      se = sqrt( vcovHC( ols, type="HC0")[i, i] )
      
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
    
    expect_equal( my.est, res.raw$est[i] )
    expect_equal( my.se, res.raw$se[i] )
    
    #@: did not check p-values and CI limits (see fn "mi_pool")
  }
  
  
  ##### One-Off Stats for Paper #####
  update_result_csv( name = "Bonferroni alpha mods",
                     section = 0,
                     value = round( alpha3, 4 ),
                     print = TRUE )
  
  update_result_csv( name = "Number mods pass Bonf",
                     section = 0,
                     value = sum( res.raw$pvalBonf[ res.raw$group == "mod" ] < 0.05 ),
                     print = TRUE )
  
 
}  # end "if (study ==1 )"





##### Complete-Case Effect Modification Analysis #####

#bm: also do the comparisons plots for CC analyses

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



##### Sanity Check: Compare CC to MI #####

if ( run.sanity == TRUE ) {
  setwd(results.dir)
  res.cc = read.csv("effect_mods_cc.csv")
  res.mi = read.csv("effect_mods_mi.csv")
  row.names(res.cc) == row.names(res.mi)
  
  ggplot( data = data.frame( cc = res.cc$est, mi = res.mi$est ),
          aes( x = cc, y = mi ) ) +
    theme_bw() + 
    geom_abline( slope = 1, intercept = 0, lty = 2, color = "gray" ) +
    geom_point()
  
  ggplot( data = data.frame( cc = res.cc$se, mi = res.mi$se ),
          aes( x = cc, y = mi ) ) +
    theme_bw() + 
    geom_abline( slope = 1, intercept = 0, lty = 2, color = "gray" ) +
    geom_point()
  
  summary( res.mi$se / res.cc$se )
  
  # estimates are very similar, which makes sense given
  #  how little missing data there is
  # SEs larger by about 20% with MI 
  
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   COST-EFFECTIVENESS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# We will estimate the cost of disseminating the intervention using
# estimates from The Humane League's actual program that is currently doing so. We will
# use these cost figures to give cost-effectiveness estimates in the form of, for example, dollars spent per ounce of reduced meat and animal product consumption.

# ~~~ to be added


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                         SUPPLEMENT: SENSITIVITY ANALYSES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

############################### SOCIAL DESIRABILITY BIAS ################################ 

# NO NEED TO DICHOTOMIZE

# To assess the possible effects of social desirability bias, interpreted
# as differential measurement error, we will conduct statistical sensitivity analyses that
# characterize how severe such bias would have to have been in order for the true intervention
# effect to have been zero (VanderWeele & Li, 2019). We will dichotomize the outcome at the
# baseline median to do so.

# repeat main, MI analysis with the dichotomized outcome
mi.res = lapply( imps, function(.d) my_log_RR(dat = .d) )
mi.res = do.call(what = rbind, mi.res)
pooled = mi_pool(ests = mi.res$est, ses = mi.res$se)  # all on log-RR scale

update_result_csv( name = "mainYLow RR",
                   section = 0,
                   value = round( exp(pooled$est), 2 ) )

update_result_csv( name = "mainYLow RR lo",
                   section = 0,
                   value = round( exp(pooled$lo), 2 ) )

update_result_csv( name = "mainYLow RR hi",
                   section = 0,
                   value = round( exp(pooled$hi), 2 ) )


# **for differential measurement error to completely explain away the the effect, 
#  magnitude of differential measurement error (i.e., the maximum direct effect of 
#  intervention on mismeasured Y*, not through the true Y), must be at least as large
#  as the observed RR itself

# so we will just report the RRs themselves


############################### SUBJECT AWARENESS ################################ 

# To assess the possible extent of subject awareness of the intervention,
# we will report the proportions of subjects answering the probe correctly within each treatment
# group. Because the awareness probe will be completed after outcome measurement, we
# will not condition on subject awareness in analysis (e.g., via subset analyses or covariate
# adjustment) to avoid inducing collider bias.

update_result_csv( name = "perc aware tx group",
                   section = 0,
                   value = round( 100 * mean(dcc$aware[ dcc$treat == 1 ] ), 2 ),
                   print = TRUE)

update_result_csv( name = "perc aware cntrl group",
                   section = 0,
                   value = round( 100 * mean(dcc$aware[ dcc$treat == 0 ] ), 2 ),
                   print = TRUE)



############################### NON-DIFFERENTIAL MEASUREMENT ERROR ################################ 

# Anticipating that there may be more non-differential
# measurement error in subjects’ reporting of serving sizes than in their reporting of consumption
# frequencies (e.g., because subjects have difficulty estimating volumes of food), we will
# repeat the primary analysis using only frequencies, rather than total amounts consumed, as
# the outcome.

# repeat main, MI analysis with the dichotomized outcome
mi.res = lapply( imps, function(.d) my_ttest(yName = "mainYFreqOnly", dat = .d) )
mi.res = do.call(what = rbind, mi.res)
pooled = mi_pool(ests = mi.res$est, ses = mi.res$se)  # all on log-RR scale


update_result_csv( name = "mainYFreqOnly diff",
                   section = 0,
                   value = round( pooled$est, 2 ) )

update_result_csv( name = "mainYFreqOnly lo",
                   section = 0,
                   value = round( pooled$lo, 2 ) )

update_result_csv( name = "mainYFreqOnly hi",
                   section = 0,
                   value = round( pooled$hi, 2 ) )

update_result_csv( name = "mainYFreqOnly pval",
                   section = 0,
                   value = format_pval( pooled$pval, 2 ),
                   print = TRUE )



############################### MISSING DATA METHODS ################################ 

# As a sensitivity analysis for the primary analyses using multiple
# imputation, we will conduct complete-case analyses. However, note that disagreements
# between this analysis and multiple imputation can occur if, for example, data are missing at
# random rather than completely at random, and such a discrepancy would not invalidate the
# multiple imputation approach.

# Welch's t-test: complete cases
( tres.cc = my_ttest(yName = "mainY", dat = dcc) )


update_result_csv( name = "mainY CC diff",
                   section = 0,
                   value = round( tres.cc$est, 2 ) )

update_result_csv( name = "mainY CC lo",
                   section = 0,
                   value = round( tres.cc$lo, 2 ) )

update_result_csv( name = "mainY CC hi",
                   section = 0,
                   value = round( tres.cc$hi, 2 ) )

update_result_csv( name = "mainY CC pval",
                   section = 0,
                   value = format_pval( tres.cc$pval, 2 ),
                   print = TRUE )


############################### EFFECTS OF INTERVENTION NONCOMPLIANCE ################################ 

# To supplement the primary analyses conducted
# by intention to treat, we will account for possible noncompliance with the intervention by
# treating intervention assignment as an instrumental variable for passing the manipulation
# check (Angrist et al., 1996). This analysis estimates a local average treatment effect (Angrist
# et al., 1996) under the exclusion restriction assumption. Because this assumption may
# be violated for manipulation-check items, we may instead conduct a similar instrumental
# variables analysis using methods that relax the exclusion restriction (Flores & Flores-Lagunes,
# 2013) if relevant methodological extensions, currently underway, are ready at the time of
# analysis.

##### Look at CC Data #####
# look at relationship between instrument (treat) and X (video duration)
# in CC data
dcc %>% group_by(treat) %>%
  summarise( sum( passCheck, na.rm = TRUE) )
table(dcc$treat, dcc$passCheck)
# first-stage model (linear probability model; ignore the inference):
summary( lm( passCheck ~ treat, data = dcc) )

##### IV for MI Datasets #####
mi.res = lapply( imps, function(.d) my_ivreg(dat = .d) )
mi.res = do.call(what = rbind, mi.res)
pooled = mi_pool(ests = mi.res$est, ses = mi.res$se) 

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
                   section = 0,
                   value = round( pooled$est, 2 ) )

update_result_csv( name = "mainY IV lo",
                   section = 0,
                   value = round( pooled$lo, 2 ) )

update_result_csv( name = "mainY IV hi",
                   section = 0,
                   value = round( pooled$hi, 2 ) )

update_result_csv( name = "mainY IV pval",
                   section = 0,
                   value = format_pval( pooled$pval, 2 ),
                   print = TRUE )


############################### INTERVENTION EFFECT MAINTENANCE OVER TIME ################################

# As noted above, subjects’ individual follow-up
# times could potentially range from 10 days to 23 days (though the upper bound could
# be less depending on how quickly subjects complete the questionnaires after we make them
# available on Prolific). To estimate the extent to which the intervention effect is maintained
# over time, we will use generalized least-squares, as above, to regress consumption on a main
# effect of intervention group assignment and its interaction with the number of days elapsed
# between a subject’s completion of the baseline wave and of the follow-up wave. If subjects’
# follow-up times show very little variability (e.g., because they all complete the baseline and
# follow-up questionnaires soon after we make them available), we may omit this analysis.



