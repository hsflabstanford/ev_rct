
rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      PRELIMINARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Packages #####
library(dplyr)
library(readr)
library(tableone)
library(ggplot2)

##### Working Directories #####
raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
imputed.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data/Saved fake imputations"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data/Results from R (fake)"

setwd(code.dir)
source("helper.R")

overwrite.res = TRUE

##### Dataset #####
setwd(prepped.data.dir)
d = read.csv("prepped_FAKE_data.csv")

# read in imputations
setwd(imputed.data.dir)
load("imputed_datasets.RData")  # load "imps", the mids object

# # if we need to pool manually
# setwd("Imputed datasets as csvs")
# imps = lapply( list.files(),
#                function(x) suppressMessages(read_csv(x)) )


##### Lists of Variables #####
meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)

foodVars = c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
              names(d)[ grepl(pattern = "Ounces", names(d) ) ] )

secondaryY = c("spec",
               "dom",
               "activ")

fu.vars = c(foodVars, secondaryY, "aware" )

demographics = c("sex",
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 DESCRIPTIVE & TABLE 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Table 1
CreateTableOne( vars = demographics, 
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
# psychological outcomes. We will report inference for all secondary outcomes both with and
# without Bonferroni correction, counting one test per secondary outcome. As an outcome-wide
# measure of the intervention’s effect on the secondary outcomes, we will report the number
# of secondary outcomes with a Bonferroni-corrected p < 0:05. This can be interpreted with
# 95% confidence as the number of secondary outcomes on which the intervention has an effect
# (VanderWeele & Mathur, 2019).


# go through each outcome, fit this model, and add its estimate, CI, and pval to a table
# also Bonferroni-correct the secondaries

# one-off stat: total number of secondaries passing Bonferroni



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                               TABLE 3: EFFECT MODIFIERS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# We will fit a single ordinary least squares (OLS) model with two-way
# interactions of the intervention effect with each of the following variables from the baseline
# demographic data: sex, race/ethnicity, individual political affiliation, education level, and
# the ratio of self-identified liberals vs. conservatives in the subject’s zip code (e.g., Gallup
# (2019)). We will collapse all non-binary variables into binary variables for this analysis (e.g.,
# for education, greater than vs. less than college), with the categories determined by the
# distribution of responses. We will again report inference both with and without Bonferroni
# correction, counting one test per effect modifier regression coefficient.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   COST-EFFECTIVENESS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# We will estimate the cost of disseminating the intervention using
# estimates from The Humane League’s actual program that is currently doing so. We will
# use these cost figures to give cost-effectiveness estimates in the form of, for example, dollars
# spent per ounce of reduced meat and animal product consumption.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                         SUPPLEMENT: SENSITIVITY ANALYSES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

############################### SOCIAL DESIRABILITY BIAS ################################ 

# To assess the possible effects of social desirability bias, interpreted
# as differential measurement error, we will conduct statistical sensitivity analyses that
# characterize how severe such bias would have to have been in order for the true intervention
# effect to have been zero (VanderWeele & Li, 2019). We will dichotomize the outcome at the
# baseline median to do so.


############################### SUBJECT AWARENESS ################################ 

# To assess the possible extent of subject awareness of the intervention,
# we will report the proportions of subjects answering the probe correctly within each treatment
# group. Because the awareness probe will be completed after outcome measurement, we
# will not condition on subject awareness in analysis (e.g., via subset analyses or covariate
# adjustment) to avoid inducing collider bias.


############################### NON-DIFFERENTIAL MEASUREMENT ERROR ################################ 

# Anticipating that there may be more non-differential
# measurement error in subjects’ reporting of serving sizes than in their reporting of consumption
# frequencies (e.g., because subjects have difficulty estimating volumes of food), we will
# repeat the primary analysis using only frequencies, rather than total amounts consumed, as
# the outcome.

############################### MISSING DATA METHODS ################################ 

# As a sensitivity analysis for the primary analyses using multiple
# imputation, we will conduct complete-case analyses. However, note that disagreements
# between this analysis and multiple imputation can occur if, for example, data are missing at
# random rather than completely at random, and such a discrepancy would not invalidate the
# multiple imputation approach.


############################### EFFECTS OF INTERVENTION NONCOMPLIANCE ################################ 

# To supplement the primary analyses conducted by intention to treat, we will account for possible
# noncompliance with the intervention (i.e., not watching the entire documentary) by treating intervention
# assignment as an instrumental variable for intervention receipt (Angrist et al., 1996). We will define
# intervention receipt as whether the subject remained on the webpage containing the video for at least
# 20 minutes, the duration of the video. This analysis estimates a local average treatment effect
# (Angrist et al., 1996).


