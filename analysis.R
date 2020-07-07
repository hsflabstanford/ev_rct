
rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      PRELIMINARIES
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

##### Working Directories #####
raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
imputed.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data/Saved fake imputations"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data/Results from R (fake)"
overleaf.dir = "~/Dropbox/Apps/Overleaf/EatingVeg manuscript/R_objects"

setwd(code.dir)
source("helper_analysis.R")

overwrite.res = TRUE

# res.raw will be a table of estimates for main outcome, secondaries, effect modifiers
# res.overleaf will be individual stats formatted for piping into Overleaf
if ( overwrite.res == TRUE & exists("res.raw") ) rm(res.raw)
if ( overwrite.res == TRUE & exists("res.overleaf") ) rm(res.raw)



##### Dataset #####
setwd(prepped.data.dir)
d = read.csv("prepped_FAKE_data.csv")

# complete cases wrt mainY
# might still have sporadic missing data elsewhere
dcc = d %>% filter( !is.na(mainY) )
nrow(dcc)

# read in imputations
setwd(imputed.data.dir)
# load("imputed_datasets.RData")  # load "imps", the mids object

# if we need to pool manually
setwd("Imputed datasets as csvs")
to.read = list.files()[ grepl( pattern = "prepped", x = list.files() ) ]
imps = lapply( to.read,
               function(x) suppressMessages(read_csv(x)) )


##### Lists of Variables #####
meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)

foodVars = c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
              names(d)[ grepl(pattern = "Ounces", names(d) ) ] )

# exploratory psych variables
psychY = c("spec",
               "dom",
               "activ")

# secondary food outcomes
secFoodY = c("totalMeat",
                   "totalAnimProd",
                   meats,
                   animProds,
                   "totalGood")

fu.vars = c(foodVars, psychY, "aware" )

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
             "party")

effect.mods = c("female",
                "old",
                "collegeGrad",
                "cauc",  # probably will be the very dominant category
                "democrat")
# ~~need to add something about political affliation of their zip code

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 DESCRIPTIVE & TABLE 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Table 1 #####
CreateTableOne( vars = demo.raw, 
                strata = "treat", 
                data = d,
                includeNA = TRUE)  # last only works for NA

if(exists("t")) rm("t")



# stratify demographics by treatment group
t1.treat = make_table_one(.d = d %>% filter( treat == 1) )
t1.cntrl = make_table_one(.d = d %>% filter( treat == 0) )

t1 = data.frame( Characteristic = t1.treat$Characteristic,
                 Intervention = t1.treat$Summary,
                 Control = t1.cntrl$Summary )

# save it
if( overwrite.res == TRUE ){
  setwd(results.dir)
  write.csv(t1, "table1.csv")
}


##### Descriptive Look at Treatment Group Differences #####

# examine skewed outcome (immaterial given sample size)
hist(dcc$mainY)

# treatment group differences...!
ggplot( data = dcc, 
        aes( x = treat.pretty,
             y = mainY ) ) +
  geom_violin(draw_quantiles = c(.25, .5, .75)) + 
  theme_bw()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                    TABLE 2: MAIN ANALYSIS AND ALL SECONDARY FOOD OUTCOMES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# By default, we will conduct a 2-sample t-test of total consumption by treatment group,
# reporting the mean difference, a 95% confidence interval, and a p-value treated as a continuous
# measure. We expect that errors may be heteroskedastic given the potentially skewed outcome;
# if this appears to be the case based on the residuals, we will instead use a comparable
# generalized least-squares model with heteroskedasticity-consistent robust standard errors.
# We will not transform the outcome nor otherwise account for non-normal residuals given the
# large sample size (Stapleton, 2009).

# We will conduct a counterpart to the primary analysis for each
# secondary outcome, comprising the secondary consumption outcomes as well as the exploratory
# psychological outcomes. 

# We will report inference for all secondary outcomes both with and
# without Bonferroni correction, counting one test per secondary outcome. As an outcome-wide
# measure of the intervention’s effect on the secondary outcomes, we will report the number
# of secondary outcomes with a Bonferroni-corrected p < 0.05. This can be interpreted with
# 95% confidence as the number of secondary outcomes on which the intervention has an effect
# (VanderWeele & Mathur, 2019).

# # to pass fn as argument:
# fake = 'my_ttest(yName = "mainY", dat = .d)'
# mi.res = lapply( imps, function(.d) eval(parse(text = fake)) )



##### Analyze Each Outcome (Including Primary) #####

# for Bonferroni
n.secY = sum( length(secFoodY), length(psychY) )
( alpha2 = 0.05 / n.secY ) # Bonferroni-adjusted alpha

for ( i in c("mainY", secFoodY, psychY ) ) {
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


##### One-Off Stats for Paper: Main Estimates #####
update_result_csv( name = "mainY diff",
                   section = 0,
                   value = round( res.raw$est[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff lo",
                   section = 0,
                   value = round( res.raw$lo[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff hi",
                   section = 0,
                   value = round( res.raw$hi[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff pval",
                   section = 0,
                   value = format_pval( res.raw$pval[ res.raw$analysis == "mainY MI"], 2 ),
                   print = TRUE )

update_result_csv( name = "mainY diff g",
                   section = 0,
                   value = round( res.raw$g.est[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff g lo",
                   section = 0,
                   value = round( res.raw$g.lo[ res.raw$analysis == "mainY MI"], 2 ) )

update_result_csv( name = "mainY diff g hi",
                   section = 0,
                   value = round( res.raw$g.hi[ res.raw$analysis == "mainY MI"], 2 ) )


##### One-Off Stats for Paper: Various Multiple-Testing Metrics for Secondary Outcomes #####
update_result_csv( name = "Bonferroni alpha secY",
                   section = 0,
                   value = round( alpha2, 4 ),
                   print = TRUE )

update_result_csv( name = "Number secY pass Bonf",
                   section = 0,
                   value = sum( res.raw$pvalBonf[ res.raw$group == "secY" ] < 0.05 ),
                   print = TRUE )

# harmonic mean p-values by subsets of effect modifiers
update_result_csv( name = "HMP all secY",
                   section = 0,
                   value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group == "secY" ],
                                         L = sum(res.raw$group == "secY") ), 2 ),
                   print = TRUE )

update_result_csv( name = "HMP food secY",
                   section = 0,
                   value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY food" ],
                                               L = sum(res.raw$group.specific == "secY food") ), 2 ),
                   print = TRUE )

update_result_csv( name = "HMP psych secY",
                   section = 0,
                   value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY psych" ],
                                               L = sum(res.raw$group.specific == "secY psych") ), 2 ),
                   print = TRUE )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                               TABLE 3: EFFECT MODIFIERS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# We will fit a single ordinary least squares (OLS) model with two-way
# interactions of the intervention effect with each of the following variables from the baseline
# demographic data: sex, race/ethnicity, individual political affiliation, education level, and
# the ratio of self-identified liberals vs. conservatives in the subject’s zip code (e.g., Gallup
# (2019)).
# We will collapse all non-binary variables into binary variables for this analysis (e.g.,
# for education, greater than vs. less than college), with the categories determined by the
# distribution of responses. We will again report inference both with and without Bonferroni
# correction, counting one test per effect modifier regression coefficient.


##### Sanity Check #####
# look at effect modifiers as they'll be coded in analysis
CreateTableOne( vars = effect.mods, 
                data = dcc,
                includeNA = TRUE)  # last only works for NA

# for Bonferroni
n.mods = length(effect.mods)
( alpha3 = 0.05 / n.mods ) # Bonferroni-adjusted alpha

# names of coefficient estimates as they'll appear in the model
coefNames = c("treat:female",
              "treat:oldTRUE",
              "treat:collegeGradTRUE",
              "treat:cauc",
              "treat:democrat")


for ( i in coefNames ) {
  
  mi.res = lapply( imps, function(.d) {
              # fit one model with all effect modifiers
              string = paste( "mainY ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
              ols = lm( eval( parse( text = string ) ), data = .d )
              
              my_ols_hc0(coefName = i, dat = .d, ols = ols)
              }  )
  
  mi.res = do.call(what = rbind, mi.res)
  
  part1 = mi_pool(ests = mi.res$est, ses = mi.res$se)
  part2 = mi_pool(ests = mi.res$g, ses = mi.res$g.se)
  names(part2) = paste( "g.", names(part2), sep = "" )
  new.row = cbind(part1, part2)
  
  new.row$pvalBonf = min( 1, new.row$pval * n.mods )
  new.row$group = "mod"
  new.row$group.specific = "mod"

  # add name of this analysis
  string = paste(i, " MI", sep = "")
  new.row = add_column(new.row, analysis = string, .before = 1)
  
  if ( !exists("res.raw") ) res.raw = new.row else res.raw = rbind(res.raw, new.row)
}

res.raw


##### One-Off Stats for Paper #####
update_result_csv( name = "Bonferroni alpha mods",
                   section = 0,
                   value = round( alpha3, 4 ),
                   print = TRUE )

update_result_csv( name = "Number mods pass Bonf",
                   section = 0,
                   value = sum( res.raw$pvalBonf[ res.raw$group == "mod" ] < 0.05 ),
                   print = TRUE )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   COST-EFFECTIVENESS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# We will estimate the cost of disseminating the intervention using
# estimates from The Humane League's actual program that is currently doing so. We will
# use these cost figures to give cost-effectiveness estimates in the form of, for example, dollars spent per ounce of reduced meat and animal product consumption.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                         SUPPLEMENT: SENSITIVITY ANALYSES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

############################### SOCIAL DESIRABILITY BIAS ################################ 

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

# To supplement the primary analyses conducted by intention to treat, we will account for possible
# noncompliance with the intervention (i.e., not watching the entire documentary) by treating intervention
# assignment as an instrumental variable for intervention receipt (Angrist et al., 1996). We will define
# intervention receipt as whether the subject remained on the webpage containing the video for at least
# 20 minutes, the duration of the video. This analysis estimates a local average treatment effect
# (Angrist et al., 1996).

##### Look at CC Data #####
# look at relationship between instrument (treat) and X (video duration)
# in CC data
dcc %>% group_by(treat) %>%
  summarise( mean(video.time, na.rm = TRUE ),
             sum( finishedVid, na.rm = TRUE) )
table(dcc$treat, dcc$finishedVid)
# first-stage model (linear probability model; ignore the inference):
summary( lm( finishedVid ~ treat, data = dcc) )

##### IV for MI Datasets #####
mi.res = lapply( imps, function(.d) my_ivreg(dat = .d) )
mi.res = do.call(what = rbind, mi.res)
pooled = mi_pool(ests = mi.res$est, ses = mi.res$se) 

# ~~ look at this manually to make sure we don't have a weak instrument
#  (though that seems inconceivable in this case):
mi.res$stage1.pval
# why always the same??

# sanity check
# confirm the fact that the first-stage model has the same p-value for every imputation
# first-stage model (linear probability model; ignore the inference):
summary( lm( finishedVid ~ treat, data = imps[[3]]) )
# this seems to be where where the weak instruments p-value is coming from
summary( ivreg(mainY ~ finishedVid | treat, data = imps[[3]]), diagnostics = TRUE )



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

