
# ~~~NOTE: There is one small loose end here regarding counties with multiple entries. 
# Search "~~~" below. 

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

##### Working Directories #####
raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data"
prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped"
county.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/County politics data"
imputed.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data/Saved fake imputations"
code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"

setwd(code.dir)
source("helper_prep.R")


# county-level politics data
setwd(county.data.dir)
zip = read.csv("countypres_2000-2016.csv")


# overwrite old results?
overwrite.res = TRUE


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                         EXPLORE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# could summarize affiliations as the percentage of votes per county from 2000-2016 that went to a Democrat, excluding
#  votes to parties other than Democratic and Republican

table(zip$year)

zip %>% group_by(state) %>%
  summarise( length(unique(year)) )

# very small amount of missing data
table( is.na(zip$candidatevotes) )


# remove votes to parties other than Democrat/Republican
table(zip$party, useNA = "ifany")
zip2 = zip %>% filter( !is.na(party) & party %in% c("democrat", "republican") )

# sanity check: make sure each year-state-county triad has 2 rows
# (one for Democrat and one for Republican)
temp = zip2 %>% group_by(year, state, county) %>% 
  summarise( n = n() )
table(temp$n)

# ~~~ note that a very small number of counties (25) have multiple FIPS, one of which has way fewer votes than the # other
# this is why they have 4 total rows
# examples:
temp %>% filter(n==4)
zip2 %>% filter(year == 2004 & state == "Virginia" & county == "Richmond")
# ~~~ for now, aggregate over the multiple FIPS
# ~~~ come back to this later

# not sure why this is, since supposedly every state has a unique FIPS: 
# https://www.cms.gov/Regulations-and-Guidance/Legislation/EHRIncentivePrograms/Downloads/CMS_API_instructions.pdf


###### Sanity check: Sort states from Dem to Rep #####
temp = zip2 %>% group_by(state) %>%
  mutate( stateTot = sum(candidatevotes, na.rm = TRUE) )

temp$stateTot[1]
sum(temp$candidatevotes[temp$state == "Alabama"])
sum(temp$candidatevotes[temp$state == "Alabama" & temp$party == "democrat"])

temp2 = temp %>% filter(party == "democrat") %>%
  mutate( stateDemTot = sum(candidatevotes, na.rm = TRUE),
          statePDem = stateDemTot / stateTot )

temp2$stateTot[1]
temp2$stateDemTot[1]
temp2$statePDem

# sort the states
# seems to make sense :)
# c.f. proportion of self-ID'ed liberals and conservatives by state: https://news.gallup.com/poll/247016/conservatives-greatly-outnumber-liberals-states.aspx
data.frame( temp2 %>% group_by(state) %>%
        summarise( statePDem = mean(statePDem) ) %>%
        arrange(statePDem) )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 MAKE PROPORTION-DEMOCRATIC VARIABLE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# bm: was trying to straighten out this part...

##### Add Variable for Total Votes to Major-Party Candidate #####
# by state-county pair in case of duplicated county names
zip3 = zip2 %>% group_by(state, county) %>% 
  mutate( nTot = sum(candidatevotes, na.rm = TRUE) )

# sanity check: manually calculate nTot for the first state-county pair
expect_equal( zip3$nTot[1],
              sum( zip3$candidatevotes[ zip3$state == zip3$state[1] & zip3$county == zip3$county[1] ] ) )
# these are the votes that should contribute to the above count (5 election years * 2 candidates):
data.frame( zip3[ zip3$state == zip3$state[1] & zip3$county == zip3$county[1], ] )


# sanity check: every state-county pair should have exactly
#  one value of nTot
temp = zip3 %>% group_by(state, county) %>%
  summarise( nLevels = length(unique(nTot) ) )
expect_equal( as.numeric( unique(temp$nLevels) ), 1 )

##### Add Variable for Total Votes to the Democrat #####
# proportion of votes to Dem
# now we are aggregating into counties
counties = zip3 %>% filter(party == "democrat") %>%
  group_by(state, county) %>%
  summarise( nDem = sum(candidatevotes, na.rm = TRUE),
             pDem = nDem/nTot[1])  # use first value of nTot since static after this grouping

# should be no missing data
expect_equal( unique(is.na(counties$pDem)), FALSE )

# check number of counties
expect_equal( nrow(counties), length(unique( paste(zip$county, zip$state) ) ) )

# sanity check: manually calculate pDem for the first state-county pair
expect_equal( counties$pDem[1],
              sum( zip3$candidatevotes[ zip3$state == zip3$state[1] & zip3$county == zip3$county[1] & zip3$party == "democrat"] ) / sum( zip3$candidatevotes[ zip3$state == zip3$state[1] & zip3$county == zip3$county[1] ] ) )


##### Sanity checks #####
# sort states by pDem
# will be a little different from the previous state sorting because
# this is a mean of county means, not a marginal mean
# but still makes sense
data.frame( counties %>% group_by(state) %>%
              summarise(statePDem = mean(pDem, na.rm = TRUE)) %>%
              arrange(statePDem) )

# find the most liberal and most conservative counties
counties = counties %>% arrange(pDem)
counties[1:5,]  # most conservative
counties[ (nrow(counties) - 5) : nrow(counties), ]  # most liberal
# all makes sense :)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                 SIMPLIFY AND SAVE DATASET
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

counties$stateCounty = paste( counties$state, counties$county, sep = " ")

setwd(prepped.data.dir)

if ( overwrite.res == TRUE ){
  write.csv(counties %>% select(stateCounty, pDem), "counties_prepped.csv", row.names = FALSE)
} else {
  message("Not overwriting the old results because overwrite.res == FALSE, FYI")
}
