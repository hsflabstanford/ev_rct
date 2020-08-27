# # check for people with problems or duplicated pIDs,
# #  and add N accordingly
# 
# # ~~~~ TEMP ONLY: LOOK AT PROLIFIC COMPLETION TIMES
# setwd("~/Desktop")
# p = read.csv("prolific_export_5f45868db809f51a582c456b.csv")
# t = p$time_taken[ p$status == "AWAITING REVIEW"]
# summary(t/60)
# median(t/60)
# # ~~~~~~
# 
# # note: when creating the time-on-task variable, need to use page 3 time on and off task
# #  from the TaskMaster Shiny parser, whose sum agrees with Qualtrics' own timer

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

# overwrite old results?
overwrite.res = TRUE

# should we impute from scratch or read in saved datasets?
impute.from.scratch = TRUE
M = 10


##### Working Directories #####
# for pilot or fake data
# raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Pilot data"
# prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Pilot data"
# imputed.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data/Saved fake imputations"

raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Raw"
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped"
# this script will save some results of sanity checks
results.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Results from R"

county.prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped"
                                
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"
                                 
setwd(code.dir)
source("helper_prep.R")

# read in wave 1 data
# these data have already been run through TaskMaster's Shiny App to parse the on-task 
#  time strings
# and I already manually removed 2 extra header rows from Qualtrics
setwd(raw.data.dir)
# d = read.csv("raw_FAKE_data.csv")
# w1 = read.csv( "wave1_P3.csv", header = TRUE )
w1 = read.csv("wave1_R1_noheader_taskmaster.csv", header = TRUE)
expect_equal( nrow(w1), 650 )

# lists of variables
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
              "state" )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   PREP WAVE 1 (BASELINE) DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

################################ RENAME AND RECODE VARIABLES ################################ 

# rename variables
w1 = w1 %>% rename( ID = PROLIFIC_PID,
                    date = StartDate,
                    totalQuestionnaireMin = Duration..in.seconds./60,
                    finishedQuestionnaire = Finished,
                    IPlat = LocationLatitude,
                    IPlong = LocationLongitude,
                    state = stateCounty_1,
                    county = stateCounty_2,
                    videoContent = attention )

# recode checkbox variables as non-mutually-exclusive dummies
w1 = recode_checkboxes(.d = w1, var = "race")

# combined state-country variable for later merging joy
w1$stateCounty = paste( w1$state, w1$county, sep = " " )

# passing manipulation check
w1$passCheck = (w1$videoContent == "The ways we raise animals for human consumption causes the animals to suffer.")


##### Video Time Variables (TaskMaster and Qualtrics) #####
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


################################ MERGE IN COUNTY POLITICS DATA ################################ 

# merge in county-level politics data (already prepped by data_prep_counties.R)
#  this just adds the variable pDem to the dataset
setwd(county.prepped.data.dir)
cn = read.csv("counties_prepped.csv")
w1 = merge(w1, cn, by = "stateCounty")

expect_equal( nrow(w1), 650 )


################################ DROP AND REORGANIZE VARIABLES ################################ 

w1 = w1 %>% select( # analysis variables
                    ID,
                    date,
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
                    totalQuestionnaireMin,
                    finishedQuestionnaire,
                    IPlat,
                    IPlong,
                    onTaskMin,
                    offTaskMin,
                    problemsBin,
                    problemsText )


################################ EXCLUDE SUBJECTS IF NEEDED ################################

# review subjects' stated problems to see if any are serious enough to exclude
setwd(results.dir)
setwd("Sanity checks after W1")
write.csv( w1 %>% select(ID, problemsText) %>%
             filter( problemsText != ""),
           "w1_R1_problemsText_for_review.csv")
# nothing requiring exclusion
#  mostly just confusion from control subjects about why none of manipulation
# check items matched the control video content

# any repeated Prolific IDs?
t = w1 %>% group_by(ID) %>%
  summarise(n())
table( t$`n()` )  # responses per pID
# **one person somehow did it twice: note this in manuscript
dupID = w1$ID[ duplicated(w1$ID) ]

# keep only this person's first submission
w1 = w1 %>% filter( !duplicated(ID) )
expect_equal( nrow(w1), 649 )


################################ aSAVE PREPPED W1 DATA ################################ 

setwd(prepped.data.dir)
write.csv(w1, "prepped_data_W1_R1.csv")

################################ SANITY CHECKS ################################ 

# quick look at demographics
temp = w1 %>% select( c("treat", demoVars, "passCheck", "onTaskMin") )
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






############# 
############# 
############# REST WAS WRITTEN EARLIER USING SIMULATED DATA:




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

fu.vars = c(foodVars,
            names(d)[ grepl(pattern = "spec", x = names(d) ) ],
            names(d)[ grepl(pattern = "dom", x = names(d) ) ],
            names(d)[ grepl(pattern = "activ", x = names(d) ) ],
            names(d)[ grepl(pattern = "guessPurpose", x = names(d) ) ] )


##### Recode Character Vars as Factors #####
# this is needed to avoid "constant" problem in mice's loggedEvents
sum(sapply(d, is.character))  # check number of character vars
d = d %>% mutate_if(sapply(d, is.character), as.factor)
sum(sapply(d, is.character))  # check again; should be 0


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

# also save the CC version of dataset for sensitivity analysis

# impute raw data before making derived variables


##### Make Imputations #####

if ( impute.from.scratch == TRUE ) {
  ##### Generate Imputations #####
  library(mice)
  ini = mice(d, m=1, maxit = 0 )
  ini$loggedEvents
  if ( !is.null(ini$loggedEvents) ) warning("Imputation trouble: Dry run has logged events!")
  
  # check default methods
  ini$method
  
  # make smart predictor matrix
  pred = quickpred(d)
  
  imps = mice( d,
               m=M,
               predictorMatrix = pred,
               #ridge = 1e-02,  # this can help with collinearity; not needed here
               method = "pmm")
  
  # any complaints?
  head(imps$loggedEvents)
  if ( !is.null(imps$loggedEvents) ) warning("Imputation trouble: Imputations have logged events!")
  
  
  # make sure there is no missing data in the imputations
  any.missing = apply( complete(imps,1), 2, function(x) any(is.na(x)) ) # should be FALSE
  if ( any(any.missing) == TRUE ) warning("Imputed datasets have missing data! Look at logged events.")
  
  # first imputed dataset
  head( complete(imps, 1) )
  # if this line returns an error about complete() not being applicable
  #  for a mids objects (which is a lie), restart R
  
  ##### Save Imputations for Reproducibility #####
  if ( overwrite.res == TRUE ) {
    
    # save imputations for reproducibility
    setwd(imputed.data.dir)
    save( imps, file = "imputed_datasets.RData" )
    
    # also save imputed datasets as csvs for Ying
    setwd("Imputed datasets as csvs")
    
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
setwd("Imputed datasets as csvs")

imps = lapply( list.files(),
               function(x) suppressMessages(read_csv(x)) )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      MAKE DERIVED VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# recode CC dataset
d2 = make_derived_vars(d)
CreateTableOne(data=d2,
               includeNA = TRUE) # second argument only works for categoricals


##### Recode the Imputations #####
# read in each relevant imputed dataset as csv and saved a prepped version
# currently this loop is not actually making any changes
for ( i in 1:M ) {
  imp = as.data.frame( imps[[i]] )
  
  imp = make_derived_vars(imp)
  
  # overwrite the old one
  write.csv( imp, paste("imputed_dataset_prepped_", i, ".csv", sep="") )
}

# look at the last imputation
CreateTableOne(data=imp,
               includeNA = TRUE)  # second argument only works for categoricals


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
write.csv(d2, "prepped_FAKE_data.csv")

