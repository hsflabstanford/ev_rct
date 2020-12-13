
rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      PRELIMINARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Packages #####
library(dplyr)
library(mice)
library(readr)
library(tableone)
library(testthat)
library(qdapTools)
library(Amelia)
library(tableone)
library(stringr)

# overwrite old results?
overwrite.res = TRUE


##### Working Directories ####
raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Raw/Study 2"


# this script will save some results of sanity checks
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Results from R/Study 2"

county.prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped"

code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"

setwd(code.dir)
source("helper_prep.R")

# should we overwrite previous prepped versions of the data?
overwrite.prepped.data = TRUE

##### Lists of Variables #####
demoVars = c( "sex",
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
              "pDem",
              "state",
              "covid" )

meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   PREP WAVE 1 (BASELINE) DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# @@try again with taskmaster?
# check if anyone spent too little time because of the qualtrics wrinkle

##### Read in Wave 1 Data #####
# these data have already been run through TaskMaster's Shiny App to parse the on-task 
#  time strings
# and I already manually removed 2 extra header rows from Qualtrics
setwd(raw.data.dir)
d1 = read.csv("study2_R1_from_qualtrics_noheader.csv", header = TRUE)
expect_equal( nrow(d1), 300 )


################################ RENAME AND RECODE VARIABLES ################################ 

# rename variables
d1 = d1 %>% rename( d1.ID = PROLIFIC_PID,
                    d1.date = StartDate,
                    d1.totalQuestionnaireMin = Duration..in.seconds./60,
                    d1.finishedQuestionnaire = Finished,
                    d1.IPlat = LocationLatitude,
                    d1.IPlong = LocationLongitude,
                    state = stateCounty_1,
                    county = stateCounty_2,
                    importAnimals = animals_Important,
                    importHealth = healthy_Important,
                    importEnviro = enviro_Important,
                    d1.problemsBin = problemsBin,
                    d1.problemsText = problemsText)

# recode checkbox variables as non-mutually-exclusive dummies
d1 = recode_checkboxes(.d = d1, var = "race")

# combined state-country variable for later merging joy
d1$stateCounty = paste( d1$state, d1$county, sep = " " )


##### Video Time Variables (TaskMaster and Qualtrics) #####
# combine video time variables (1 for each treatment)
d1$videoMinQualtrics = d1$videoTime_Page.Submit  # still in seconds
d1$videoMinQualtrics[ is.na(d1$videoTime_Page.Submit) ] = d1$videoTime_Page.Submit.1[ is.na(d1$videoTime_Page.Submit) ]
# convert to minutes
d1$videoMinQualtrics = d1$videoMinQualtrics / 60
# sanity check
expect_equal( any(is.na(d1$videoMinQualtrics)), FALSE )
expect_equal( min(d1$videoMinQualtrics) > 20, TRUE ) 

# @@NEEDS TASKMASTER VARIABLES:
# # TaskMaster data about on-task time 
# d1$onTaskMin = d1$Page_3_TimeOnPage/60
# d1$offTaskMin = d1$Page_3_TimeOffPage/60
# d1$totalTaskMin = d1$onTaskMin + d1$offTaskMin
# # sanity check: should generally be similar
# # but could potentially differ if, e.g., subject closes tab and starts it up again
# table( abs( d1$totalTaskMin - d1$videoMinQualtrics ) < .01 )
# d1$finishedVid = d1$onTaskMin >= 20

# # percentage of the 20 minutes for which people are on task
# 100 * round( summary( d1$onTaskMin / 20 ), 2 )
# # percentage of total time on the page for which people are on task
# 100 * round( summary( d1$onTaskMin / d1$totalTaskMin ), 2 )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      MAKE DERIVED VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# recode CC dataset
# bm
d2 = make_derived_vars(d1)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    PRETTIFY VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

d2$treat.pretty = NA
d2$treat.pretty[ d2$treat == 0 ] = "Control"
d2$treat.pretty[ d2$treat == 1 ] = "Documentary"

################################ MERGE IN COUNTY POLITICS DATA ################################ 

# merge in county-level politics data (already prepped by data_prep_counties.R)
#  this just adds the variable pDem to the dataset
setwd(county.prepped.data.dir)
cn = read.csv("counties_prepped.csv")
d1 = merge(d1, cn, by = "stateCounty")

expect_equal( nrow(d1), 300 )




################################ DROP AND REORGANIZE VARIABLES ################################ 


# d1 = d1 %>% select( # analysis variables
#   d1.ID,
#   d1.date,
#   treat,
#   sex,
#   age, 
#   educ,
#   cauc,
#   hisp,
#   black,
#   midEast,
#   #pacIsl,  # doesn't exist because no one used that option
#   natAm, 
#   SAsian,
#   EAsian,
#   SEAsian,
#   party,
#   pDem,
#   state,
#   county,
#   stateCounty,
#   
#   importAnimals,
#   importHealth,
#   importEnviro,
#   
#   # non-analysis meta-data
#   d1.totalQuestionnaireMin,
#   d1.finishedQuestionnaire,
#   d1.IPlat,
#   d1.IPlong,
#   # onTaskMin,
#   # offTaskMin,
#   d1.problemsBin,
#   d1.problemsText )


################################ EXCLUDE SUBJECTS IF NEEDED ################################

# review subjects' stated problems to see if any are serious enough to exclude
setwd(results.dir)
setwd("Sanity checks")
write.csv( d1 %>% select(d1.ID, d1.problemsText) %>%
             filter( d1.problemsText != ""),
           "d1_R1_problemsText_for_review.csv")
# nothing serious

# any repeated Prolific IDs?
t = d1 %>% group_by(d1.ID) %>%
  summarise(n())
table( t$`n()` )  # responses per pID
# no repeats



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    WRITE RESULTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

setwd(prepped.data.dir)
write.csv(d2, "prepped_data.csv")

