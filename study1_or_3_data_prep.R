
# note: when creating the time-on-task variable, need to use page 3 time on and off task
# from the TaskMaster Shiny parser, whose sum agrees with Qualtrics' own timer

rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      PRELIMINARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##### Set Parameters Here #####
# overwrite old results?
overwrite.res = TRUE

# should sanity checks be run?
run.sanity = FALSE

# should we impute from scratch or read in saved datasets?
# from scratch takes about an hour
impute.from.scratch = FALSE
# number of imputations
M = 10 

# which study's data to prep?
# must be 1 or 3
study = 3
# for making strings
if ( study %in% c(1,3) ) study.string = paste("Study", study, sep = " ") else stop("Invalid study spec.")


##### Packages #####
library(dplyr)
library(mice)
library(readr)
library(tableone)
library(testthat)
library(qdapTools)
library(Amelia)
library(tableone)
library(here)


##### Working Directories #####
imputed.data.dir = here( paste("Data/Prepped/", study.string, "/Saved imputations",
                               sep = "") )

raw.data.dir = here( paste("Data/Raw/", study.string, sep = "" ) )
prepped.data.dir = here( paste("Data/Prepped/", study.string, sep = "" ) )
# this script will save some results of sanity checks
results.dir = here( paste("Results from R/", study.string, sep = "" ) )

# county political affiliation data
county.prepped.data.dir = here("Data/Prepped")

code.dir = here("Code (git)")

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

# didn't have this variable in Study 3
if ( study == 3 ) demoVars = demoVars[ !demoVars == "covid" ]

meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   PREP WAVE 1 (BASELINE) DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Read in Wave 1 Data #####
# these data have already been run through TaskMaster's Shiny App to parse the on-task 
#  time strings
# and I already manually removed 2 extra header rows from Qualtrics
setwd(raw.data.dir)
if ( study == 1 ){
  w1 = read.csv("wave1_R1_noheader_taskmaster.csv", header = TRUE)
} else if ( study == 3 ) {
  w1 = read.csv("study3_wave1_R1_from_qualtrics_noheader.csv", header = TRUE)
}

# people who closed survey after starting
table(w1$Finished)

if ( study == 1 ) expect_equal( nrow(w1), 650 )
if ( study == 3 ) expect_equal( nrow(w1), 797 )

################################ RENAME AND RECODE VARIABLES ################################ 

# rename variables

if ( study == 1 ) w1 = w1 %>% rename( w1.ID = PROLIFIC_PID )
if ( study == 3 ) {
  w1 = w1 %>% rename( w1.ID = ID,
                      # order of foods here can be confirmed using the "withheader" raw dataset's
                      #  second header row:
                      "pledgeChicken" = pledge_1,
                      "pledgeFish" = pledge_2,
                      "pledgePork" = pledge_3,
                      "pledgeBeef"  = pledge_4,
                      "pledgeOtherMeat" = pledge_5,
                      "pledgeEggs" = pledge_6,
                      "pledgeDairy" = pledge_7,
                      "pledgeStrategiesFreeText" = pledgeStrategies_6_TEXT )
}

w1 = w1 %>% rename( w1.date = StartDate,
                    w1.totalQuestionnaireMin = Duration..in.seconds./60,
                    w1.finishedQuestionnaire = Finished,
                    w1.IPlat = LocationLatitude,
                    w1.IPlong = LocationLongitude,
                    state = stateCounty_1,
                    county = stateCounty_2,
                    videoContent = attention,
                    w1.problemsBin = problemsBin,
                    w1.problemsText = problemsText)

# recode checkbox variables as non-mutually-exclusive dummies
w1 = recode_checkboxes(.d = w1, var = "race")

# combined state-country variable for later merging joy
# in Study 3, it was possible for people to have a missing stateCounty (unintentionally on our part)
if ( study == 3 ){
  w1$state[ w1$state == "" ] = NA
  w1$county[ w1$county == "" ] = NA
}
w1$stateCounty = paste( w1$state, w1$county, sep = " " )
w1$stateCounty[ is.na(w1$state) | is.na(w1$county) ] = NA


# passing manipulation check
w1$passCheck = (w1$videoContent == "The ways we raise animals for human consumption causes the animals to suffer.")


##### Video Time Variables (TaskMaster and Qualtrics) #####

if ( study == 1 ) {
  # combine video time variables (1 for each treatment)
  w1$videoMinQualtrics = w1$videoTime_Page.Submit  # still in seconds
  w1$videoMinQualtrics[ is.na(w1$videoTime_Page.Submit) ] = w1$videoTime_Page.Submit.1[ is.na(w1$videoTime_Page.Submit) ]
  # convert to minutes
  w1$videoMinQualtrics = w1$videoMinQualtrics / 60
  # sanity check
  expect_equal( any(is.na(w1$videoMinQualtrics)), FALSE )
  
  # TaskMaster data about on-task time 
  w1$onTaskMin = w1$Page_3_TimeOnPage/60
  w1$offTaskMin = w1$Page_3_TimeOffPage/60
  w1$totalTaskMin = w1$onTaskMin + w1$offTaskMin
  # sanity check: should generally be similar
  # but could potentially differ if, e.g., subject closes tab and starts it up again
  table( abs( w1$totalTaskMin - w1$videoMinQualtrics ) < .01 )
  w1$finishedVid = w1$onTaskMin >= 20
  
  # percentage of the 20 minutes for which people are on task
  100 * round( summary( w1$onTaskMin / 20 ), 2 )
  # percentage of total time on the page for which people are on task
  100 * round( summary( w1$onTaskMin / w1$totalTaskMin ), 2 )
  
}


################################ MERGE IN COUNTY POLITICS DATA ################################ 

# merge in county-level politics data (already prepped by data_prep_counties.R)
#  this just adds the variable pDem to the dataset
setwd(county.prepped.data.dir)
cn = read.csv("counties_prepped.csv")

w1 = left_join( w1, cn, 
               by = "stateCounty" )

if ( study == 1 ) expect_equal( nrow(w1), 650 )
if ( study == 3 ) expect_equal( nrow(w1), 797 )



################################ DROP AND REORGANIZE VARIABLES ################################ 

if ( study == 1 ) {
  w1 = w1 %>% select( # analysis variables
    w1.ID,
    w1.date,
    treat,
    sex,
    age, 
    educ,
    cauc,
    hisp,
    black,
    midEast,
    pacIsl,
    natAm, 
    SAsian,
    EAsian,
    SEAsian,
    party,
    pDem,
    state,
    county,
    stateCounty,
    passCheck,
    
    # related to analysis variables but not directly used 
    #  in analysis
    videoContent,
    
    # non-analysis meta-data
    w1.totalQuestionnaireMin,
    w1.finishedQuestionnaire,
    w1.IPlat,
    w1.IPlong,
    onTaskMin,
    offTaskMin,
    w1.problemsBin,
    w1.problemsText )
}


if ( study == 3 ) {
  w1 = w1 %>% select( # analysis variables
    w1.ID,
    w1.date,
    treat,
    sex,
    age, 
    educ,
    highEduc,
    cauc,
    hisp,
    black,
    midEast,
    pacIsl,
    natAm, 
    SAsian,
    EAsian,
    SEAsian,
    party,
    pDem,
    state,
    county,
    stateCounty,
    targetDemographics,
    passCheck,
    
    # related to analysis variables but not directly used 
    #  in analysis
    videoContent,
    healthFreeText,
    enviroFreeText,
    animalFreeText,
    
    pledgeChicken,
    pledgeFish,
    pledgePork,
    pledgeBeef,
    pledgeOtherMeat,
    pledgeEggs,
    pledgeDairy,
    pledgeDateGoal,
    pledgeStrategies,
    pledgeStrategiesFreeText,
    madeReducePledge,
    madeEliminatePledge,
    
    # non-analysis meta-data
    w1.totalQuestionnaireMin,
    w1.finishedQuestionnaire,
    w1.IPlat,
    w1.IPlong,

    w1.problemsBin,
    w1.problemsText )
}



################################ EXCLUDE SUBJECTS IF NEEDED ################################

# review subjects' stated problems to see if any are serious enough to exclude
setwd(results.dir)
setwd("Sanity checks after W1")
write.csv( w1 %>% select(w1.ID, w1.problemsText) %>%
             filter( w1.problemsText != ""),
           "w1_R1_problemsText_for_review.csv")
# Study 1: nothing requiring exclusion
#  mostly just confusion from control subjects about why none of manipulation
# check items matched the control video content

# any repeated Prolific IDs?
t = w1 %>% group_by(w1.ID) %>%
  summarise(n())
table( t$`n()` )  # responses per pID

# **Study 1: one person somehow did it twice; note this in manuscript
# this is the only exclusion for W1
# Study 2: same; one person did it twice
dupID = w1$w1.ID[ duplicated(w1$w1.ID) ]

# keep only this person's first submission
w1 = w1 %>% filter( !duplicated(w1.ID) )

#@KEEP ONLY SUBJECTS WHO WERE RANDOMIZED
# added this AFTER study 1, so could affect its sample size
w1 = w1 %>% filter( !is.na(treat) )

if ( study == 1 ) expect_equal( nrow(w1), 649 )
if ( study == 3 ) expect_equal( nrow(w1), 665 )

################################ SAVE PREPPED W1 DATA ################################ 

if ( overwrite.prepped.data == TRUE ) {
  setwd(prepped.data.dir)
  write.csv(w1, "prepped_data_W1_R1.csv")
}


################################ SANITY CHECKS ################################ 

# read back in
setwd(prepped.data.dir)
w1 = read.csv("prepped_data_W1_R1.csv")


if ( run.sanity == TRUE ){
  # quick look at demographics
  if ( study == 1 ) {
    temp = w1 %>% select( c("treat",
                            demoVars,
                            "passCheck",
                            "onTaskMin") )
  }
  
  if ( study == 3 ) {
    temp = w1 %>% select( c("treat",
                            demoVars,
                            "passCheck") )
  }

  t = CreateTableOne(data = temp, strata = "treat")
  setwd(results.dir)
  setwd("Sanity checks after W1")
  write.csv( print(t, noSpaces = TRUE, printToggle = FALSE), 
             "ugly_table1_W1.csv")
  
  
  ##### Look for Expected Associations Among Variables #####
  
  # reported parties vs. counties' pDem
  # but exclude states with very few responses
  summary( glm( party == "Democrat" ~ pDem,
                data = w1,
                family = binomial( link = "log" ) ) )
  # makes sense :)
  
  # age and education
  summary( lm( age ~ educ, data = w1 ) )
  
  
  ##### Map of Subjects' Locations #####
  # just for fun
  # location map
  temp = w1 %>% group_by(state) %>%
    summarise( count = n(),
               pDem = mean(pDem),
               pDemReported = mean( (party == "Democrat")[ party %in% c("Democrat", "Republican") ] ) )
  dim(temp) # should be <= 52
  temp$region <- tolower(temp$state)
  library(ggplot2)
  library(maps)
  library(mapproj)
  states <- map_data("state")
  map.df <- merge(states,temp, by="region", all.x=T)
  map.df <- map.df[order(map.df$order),]
  
  ggplot(map.df, aes(x=long,y=lat, group=group))+
    geom_polygon(aes(fill=count))+
    geom_path()+ 
    scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
    coord_map()
  ggsave("states_map_W1.pdf",
         height = 4.52,
         width = 7.24,
         units = "in")
  
  # # pDem from MIT data
  # ggplot(map.df, aes(x=long,y=lat, group=group))+
  #   geom_polygon(aes(fill=pDem))+
  #   geom_path()+ 
  #   scale_fill_gradient(low = "red", high = "blue", na.value="grey90")+
  #   coord_map()
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              PREP AND MERGE WAVE 2 (FOLLOW-UP) DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##### Read in Wave 2 Data #####
# I already manually removed 2 extra header rows from Qualtrics
setwd(raw.data.dir)
if ( study == 1 ) w2 = read.csv( "wave2_R2_noheader.csv", header = TRUE)
if ( study == 3 ) w2 = read.csv( "study3_wave2_R1_from_qualtrics_noheader.csv", header = TRUE)

# a couple people started the survey, but then did not finish
table(w2$Finished)
#@ REVISIT THIS DISCREPANT CHOICE OF HANDLING MISSING DATA:
# FOR STUDY 3, WILL IMPUTE FOR PEOPLE WHO STARTED BUT DIDN'T FINISH
# PROBABLY BEST TO DO THIS FOR STUDY 1 AS WELL
if ( study == 1 ) w2 = w2[ w2$Finished == TRUE, ]

nrow(w2)  # number of completers
nrow(w2) / nrow(w1)  # completion rate vs. n from wave 1


# rename wave 2 variables
if ( study == 1 ) w2 = w2 %>% rename( w2.ID = PROLIFIC_PID )
if ( study == 3 ) w2 = w2 %>% rename( w2.ID = ID )

w2 = w2 %>% rename( w2.date = StartDate,
                    w2.totalQuestionnaireMin = Duration..in.seconds./60,
                    w2.finishedQuestionnaire = Finished,
                    w2.IPlat = LocationLatitude,
                    w2.IPlong = LocationLongitude,
                    w2.problemsBin = problemsBin,
                    w2.problemsText = problemsText,
                    importAnimals = animals_Important,
                    importHealth = healthy_Important,
                    importEnviro = enviro_Important)


# more variable lists (created here because they use the actual dataset)
foodVars = c( names(w2)[ grepl(pattern = "Freq", names(w2) ) ],
              names(w2)[ grepl(pattern = "Ounces", names(w2) ) ] )
expect_equal( 15*2, length(foodVars) )  # 15 food variables (in prereg) * 2 variables each

secondaryY = c("spec",
               "dom",
               "activ",
               "importHealth",
               "importEnviro",
               "importAnimals")



################################ EXCLUDE SUBJECTS IF NEEDED ################################

# review subjects' stated problems to see if any are serious enough to exclude
setwd(results.dir)
setwd("Sanity checks after W2")
write.csv( w2 %>% select(w2.ID, w2.problemsText) %>%
             filter( w2.problemsText != ""),
           "w2_R2_problemsText_for_review.csv")


# any repeated IDs?
t = w2 %>% group_by(w2.ID) %>%
  summarise(n())
table( t$`n()` )  # responses per pID

# if so, keep only each person's first submission
w2 = w2 %>% filter( !duplicated(w2.ID) )

# sanity check
# were all wave 2 subjects also in wave 1?
# in Study 1, it should be impossible to have an ID in W2 that doesn't
#  appear in W1...
if ( study == 1 ) expect_equal( all( w2$w2.ID %in% w1$w1.ID == TRUE ), TRUE )
# ...but in Study 3, they could have mis-entered their ID code
( nBadIDs = sum( !w2$w2.ID %in% w1$w1.ID ) )
# exclude these
w2 = w2[ w2$w2.ID %in% w1$w1.ID, ]


nrow(w2)


################################ MERGE WAVES ################################

# read wave 1 in again
setwd(prepped.data.dir)
w1 = read.csv( "prepped_data_W1_R1.csv", header = TRUE)

# merge waves
d = merge( w1, w2, by.x = "w1.ID", by.y = "w2.ID", all.x = TRUE)

if ( study == 1 ) expect_equal( nrow(d), 649 )
if ( study == 3 ) expect_equal( nrow(d), 665 )

# table(is.na(w2$beef_Freq))
# table(is.na(d$beef_Freq))



##### Recode Character Vars as Factors #####
# this is needed to avoid "constant" problem in mice's loggedEvents
sum(sapply(d, is.character))  # number of character vars
d = d %>% mutate_if(sapply(d, is.character), as.factor)
sum(sapply(d, is.character))  # check again; should be 0

# drop variables that aren't useful for analysis
#@RETURN TO THIS; I DON'T THINK WE ACTUALLY NEED TO DROP NON-IMPUTATION VARS HERE
#  BECAUSE WE SPECIFY LATER
d$ID = d$w1.ID  # have just one ID variable

if ( study == 1 ) {
  d = d %>% select( -c("X",
                       "w1.IPlat",
                       "w1.IPlong",
                       "EndDate",
                       "Progress",
                       "RecordedDate",
                       "ResponseId",
                       "RecipientLastName",
                       "RecipientFirstName",
                       "RecipientEmail",
                       "ExternalReference",
                       "Status",
                       "w2.IPlat",
                       "w2.IPlong",
                       "DistributionChannel",
                       "UserLanguage",
                       "IPAddress",
                       "pID",
                       "w1.problemsBin",
                       "w1.problemsText",
                       "w2.problemsBin",
                       "w2.problemsText",
                       "w1.finishedQuestionnaire",
                       "w2.finishedQuestionnaire",
                       "w1.ID") )
}


if ( study == 3 ) {
  d = d %>% select( -c("X",
                       "w1.IPlat",
                       "w1.IPlong",
                       "w1.date",
                       "w2.date",
                       "EndDate",
                       "Progress",
                       "RecordedDate",
                       "ResponseId",
                       "RecipientLastName",
                       "RecipientFirstName",
                       "RecipientEmail",
                       "ExternalReference",
                       "Status",
                       "w2.IPlat",
                       "w2.IPlong",
                       "DistributionChannel",
                       "UserLanguage",
                       "IPAddress",
                       "ID",
                       "w1.problemsBin",
                       "w1.problemsText",
                       "w2.problemsBin",
                       "w2.problemsText",
                       "w1.finishedQuestionnaire",
                       "w2.finishedQuestionnaire",
                       "w1.ID",
                       # "pledgeDateGoal",
                       # "pledgeStrategiesFreeText",
                       # "w1.totalQuestionnaireMin",
                       # "w2.totalQuestionnaireMin",
                       # "stateCounty",  # redundant with 
                       "wantsRaffle",  #@IULTIMATELY NEED TO DE-ID THE DATASET EARLIER IN WORKFLOW
                       "raffleEmail") )
}



# check the remaining variables
names(d)


# save intermediate dataset
write_interm(d, "d_intermediate_1.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    MULTIPLE IMPUTATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# All primary analyses will be conducted on an intention-to-treat basis, such that all enrolled
# subjects will be analyzed according to the intervention group they were assigned, regardless
# of their actual exposure to or engagement with the intervention. We will conduct all primary
# analyses using multiple imputation for all variables with missing data, including the outcome
# (using either a multilevel approach or imputation in wide format to account for correlation
#   within subjects). We will not exclude subjects with outlying data unless their data indicate
# an actual mistake in data collection, though we may conduct secondary analyses excluding
# outlying subjects.

# redo imps here
d = read_interm("d_intermediate_1.csv")

# look at missingness patterns
Pmiss = apply( d, 2, function(x) mean( is.na(x) ) )
sort(Pmiss)
# makes sense; for Study 1:
#  - all baseline variables have 0 missingness
#  - F/U variables usually have exactly 11.6% missingness because that's the attrition rate
#  - except for the "ounces" variables, which are additionally missing for subjects who said they 
#      never ate that particular food

##### Make Imputations #####

# variables to be imputed: those measured at follow-up
# these are the only vars that can have missing data
w2Vars = c(foodVars,
           names(w2)[ grepl(pattern = "spec", x = names(w2) ) ],
           names(w2)[ grepl(pattern = "dom", x = names(w2) ) ],
           names(w2)[ grepl(pattern = "activ", x = names(w2) ) ],
           names(w2)[ grepl(pattern = "guessPurpose", x = names(w2) ) ],
           names(w2)[ grepl(pattern = "import", x = names(w2) ) ],
           "covid" )

# variables to be used in imputation model:
# "real" variables measured at baseline and the F/U variables
w1Vars = c( "treat",
            demoVars )
# state has too many categories to work well as predictor
impModelVars = w1Vars[ !w1Vars == "state" ]

# # bm: give up 
# for helping to diagnose imp model problems:
# #  fit logit(P(missingness)) model
# # predict the most missing variable
# string = paste("is.na(leafyVeg_Freq ) ~ ", paste(impModelVars, collapse = " + " ))
# nullModel = glm( is.na(leafyVeg_Freq ) ~ 1, data = d )
# missModel = glm( eval(parse(text = string) ),data = d )
# summary(missModel)
# # entire missingness model not very predictive
# anova(missModel, nullModel, test = "Chisq" )
# 
# # look at inverse-prob of missingness weights (IPMW)
# d$Pmissing = predict(missModel, type = "response")
# hist(d$Pmissing)
# d %>% group_by(is.na(leafyVeg_Freq)) %>% summarise(mean(Pmissing))
# 
# # ggplot(subset(ecls_nomiss, catholic == 1), aes(x = pr_score, fill = factor(catholic))) +
# #   geom_histogram(aes(y = - ..density..)) + # note the negative sign here
# #   geom_histogram(data = subset(ecls_nomiss, catholic == 0),
# #                  aes(x = pr_score, y = ..density.., fill = factor(catholic))) +
# #   ylab("Density") + xlab("Probability of Catholic School Attendance") +
# #   ggtitle("Propensity Scores in Treated and Untreated\n(Density Histogram)") +
# #   scale_fill_discrete(name = "Catholic School")



if ( impute.from.scratch == TRUE ) {
  
  ##### Generate Imputations #####
  library(mice)
  
  # just to get the predictor matrix and method template
  ini = mice(d, m=1, maxit = 0 )

  # check default methods
  # all PMM, as desired
  ini$method
  
  # #@DEBUGGING ONLY
  # #w2Vars = foodVars # doesn't help
  # # doesn't even work with just these two
  # w2Vars = "beef_Ounces"
  # # try a less missing variable
  # w2Vars = "leafyVeg_Freq"
  # impModelVars = c("treat")
  
  # make own predictor matrix by modifying mice's own predictor matrix to keep structure the same
  #  from mice docs: "Each row corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)"
  myPred = ini$pred
  myPred[myPred == 1] = 0
  # impute all F/U variables using the sensible ones from baseline as well as all the other F/U vars
  myPred[ names(d) %in% w2Vars, # vars to be imputed
          names(d) %in% c(impModelVars, w2Vars) ] = 1  # ...and vars in the imputation model
  # set diagonals to 0 because a variable can't be used to impute itself
  diag(myPred) = 0
  
  # in order to NOT impute certain vars, also need to set method to ""
  # o.w. you get cryptic errors about collinearity
  myMethod = ini$method
  myMethod[ !names(myMethod) %in% w2Vars ] = ""
  
  
  # #DDEBUGGIG ONLY
  # myPred = ini$pred
  # myPred[myPred == 1] = 0
  # myPred[ "leafyVeg_Freq", "treat" ] = 1
  
  #BM: need to fix this...UGH!!!
  imps = mice( d,
               m=M,  
               predictorMatrix = myPred,
               method = myMethod,
               seed = 451)
  
  imps$loggedEvents$dep
  
  # # useful if you want to jump back to this point by 
  # #  reading in existing imputations as mids objects
  # setwd(imputed.data.dir)
  # load("imputed_datasets.RData")  # load "imps", the mids object
  
  # this is prone to crashing R:
  # plot comparing density of imputed data to observed datas 
  # https://stefvanbuuren.name/fimd/sec-diagnostics.html
  # red: imputations, blue: observed
  setwd(results.dir)
  pdf(file = "imputation_density_plot.pdf",
      width = 15,
      height = 10)
  densityplot(imps)
  dev.off()
  
  # any complaints?
  unique(imps$loggedEvents)
  if ( !is.null(imps$loggedEvents) ) warning("Imputation trouble: Imputations have logged events!")
  # all are about using ridge penalty for collinearity
  # not too worrisome
  
  # make sure there is no missing data in the imputations
  any.missing = apply( complete(imps,1), 2, function(x) any(is.na(x)) ) # should be FALSE
  if ( any(any.missing) == TRUE ) warning("Imputed datasets have missing data! Look at logged events.")
  
  ##### Save Imputations for Reproducibility #####
  if ( overwrite.res == TRUE ) {
    
    # save imputations for reproducibility
    setwd(imputed.data.dir)
    save( imps, file = "imputed_datasets.RData" )
    
    for (i in 1:M) {
      write.csv( complete(imps,i),
                 paste("imputed_dataset_", i, ".csv", sep="") )
    }
  }
  
} # end impute.from.scratch == TRUE


##### Read in Saved Imputations #####
# we're doing this even if impute.from.scratch=TRUE to have same data format
# i.e., a list of imputed datasets instead of a mids object
setwd(imputed.data.dir)

# avoid trying to recode other files in that directory
toRecode = paste("imputed_dataset_", 1:M, ".csv", sep="")

imps = lapply( toRecode,
               function(x) suppressMessages(read_csv(x)) )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      MAKE DERIVED VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


d = read_interm("d_intermediate_1.csv")


# recode CC dataset
d2 = make_derived_vars(d)

# sanity checks
# mainY should be missing whenever any frequency variable is missing
# but not necessarily when the ounces variables are missing, because those 
#  are also missing if the subject reported never eating that food
freqVars = c("chicken_Freq",
               "turkey_Freq",
               "fish_Freq",
               "pork_Freq",
               "beef_Freq",
               "otherMeat_Freq",
               "dairy_Freq",
               "eggs_Freq")

temp = d2[ ,freqVars ]
# Study 3: Equals 0 or 8, meaning that either someone dropped out of study entirely
#  or they answered all food freq questions, which is good
d2$numFoodFreqMissing = rowSums( is.na(temp) )
expect_equal( d2$numFoodFreqMissing > 0, is.na(d2$mainY) )

####@ ~~~~ EXPERIMENT WITH DOING MI AFTER MAKING DERIVED VARS
library(mice)
ini = mice(d2, m=1, maxit = 0 )


# check default methods
# all PMM, as desired
ini$method

# variables to be imputed: those measured at follow-up
# these are the only vars that can have missing data
# lists of variables from helper_analysis.R::prelims()
meats <<- c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds <<- c("dairy", "eggs")
decoy <<- c("refined", "beverages")
goodPlant <<- c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods <<- c(meats, animProds, decoy, goodPlant)

foodVars <<- c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
                names(d)[ grepl(pattern = "Ounces", names(d) ) ] )

# exploratory psych variables
psychY <<- c("importHealth",
             "importEnviro",
             "importAnimals",
             "activ",
             "spec",
             "dom")

# secondary food outcomes
secFoodY <<- c("totalMeat",
               "totalAnimProd",
               meats,
               animProds,
               "totalGood")
toAnalyze = c("mainY",
              secFoodY,
              psychY )




# variables to be used in imputation model:
# "real" variables measured at baseline and the F/U variables
w1Vars = c( "treat",
            demoVars )
# state has too many categories to work well as predictor
impModelVars = w1Vars[ !w1Vars == "state" ]

# w2Vars = "mainY"
# impModelVars = "treat"

# make own predictor matrix by modifying mice's own predictor matrix to keep structure the same
#  from mice docs: "Each row corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)"
myPred = ini$pred
myPred[myPred == 1] = 0
# impute all F/U variables using the sensible ones from baseline as well as all the other F/U vars
myPred[ names(d2) %in% toAnalyze, # vars to be imputed
        names(d2) %in% c(impModelVars, toAnalyze) ] = 1  # ...and vars in the imputation model
diag(myPred) = 0  # but a variable can't impute itself
sum(myPred)


myMethod = ini$method
myMethod[ !names(myMethod) %in% toAnalyze ] = ""
table(myMethod)

# imputing secfoodY variables seem to cause issues
imps = mice( d2,
             m=M,  
             predictorMatrix = myPred,
             method = myMethod,
             seed = 451)

imps$loggedEvents$dep
#bm

# make sure there is no missing data in the imputations
any.missing = apply( complete(imps,1)[ ,toAnalyze],
                     2,
                     function(x) any(is.na(x)) ) # should be FALSE
if ( any(any.missing) == TRUE ) warning("Imputed datasets have missing data! Look at logged events.")

cbind(d2$chicken, complete(imps,1)$chicken)


##### Save Imputations for Reproducibility #####
if ( overwrite.res == TRUE ) {
  
  # save imputations for reproducibility
  setwd(imputed.data.dir)
  save( imps, file = "imputed_datasets.RData" )
  
  for (i in 1:M) {
    write.csv( complete(imps,i),
               paste("imputed_dataset_", i, ".csv", sep="") )
    }
}

####@ ~~~~ END MICE EXPERIMENT


##### Recode the Imputations #####
# saves a new version of the imputation dataset (does not overwrite the old one)
setwd(imputed.data.dir)
for ( i in 1:M ) {
  imp = as.data.frame( imps[[i]] )
  
  imp = make_derived_vars(imp, printCorMat = FALSE)
  
  # overwrite the old one (prior to making derived variables)
  write.csv( imp, paste("imputed_dataset_prepped_", i, ".csv", sep="") )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    PRETTIFY VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

d2$treat.pretty = NA
d2$treat.pretty[ d2$treat == 0 ] = "Control"
d2$treat.pretty[ d2$treat == 1 ] = "Documentary"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    WRITE RESULTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

setwd(prepped.data.dir)
write.csv(d2, "prepped_merged_data.csv")

