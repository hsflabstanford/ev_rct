
rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      PRELIMINARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Packages #####
library(dplyr)
library(mice)
library(readr)
library(tableone)

##### Working Directories #####
raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
county.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/County politics data"
imputed.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data/Saved fake imputations"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"

setwd(code.dir)
source("helper_prep.R")


setwd(raw.data.dir)
d = read.csv("raw_FAKE_data.csv")

# county-level politics data
setwd(county.data.dir)
zip = read.csv("countypres_2000-2016.csv")


# overwrite old results?
overwrite.res = TRUE

# should we impute from scratch or read in saved datasets?
impute.from.scratch = FALSE
M = 10


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

