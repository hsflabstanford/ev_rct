

# SANITY CHECKS IN THE MIDDLE OF DATA COLLECTION --------------------------------------

library(here)
library(dplyr)
library(mice)
library(readr)
library(tableone)
library(testthat)
library(qdapTools)
library(Amelia)
library(tableone)
library(stringr)
library(data.table)
library(ggplot2)
library(scales)  # for POSIX dates


# waves 1 and 2 data
raw.data.dir = here("Data/Raw/Study 3")
setwd(raw.data.dir)
w1 = fread("study3_wave1_R1_from_qualtrics_noheader.csv")
nrow(w1)

w2 = fread("study3_wave2_R1_from_qualtrics_noheader.csv")
nrow(w2)

code.dir = here("Code (git)")
setwd( code.dir )
source("helper_prep.R")


# ~ Sample sizes and randomization --------------------------------------


# how many were randomized?
table(w1$treat, useNA = "ifany")

# total completed responses
sum( w1$problemsBin != "" )

# what proportion of randomized subjects completed the study through the final screen?
w1 %>% filter( !is.na(w1$treat) ) %>%
  group_by(treat) %>%
  summarise( completed = mean(problemsBin != "") )


# ~ Moderator --------------------------------------

# look at stratified randomization
table(w1$targetDemographics, useNA = "ifany")

# almost all highly educated, and very liberal,
#  but generally older than target demographics
table(w1$highEduc)
table(w1$party)
summary(w1$age)
# we will probably have to use this simplified moderator that omits age:
# works well because it's about 50-50
w1 %>% filter(!is.na(treat)) %>%
  summarise( mean(party == "Democrat" & highEduc == 1) )

# ~ Pledges --------------------------------------

# look at how many made pledges
w1 %>% filter(!is.na(treat)) %>%
  group_by(treat) %>%
  summarise( elim = mean(madeEliminatePledge, na.rm = TRUE),
             reduce = mean(madeReducePledge, na.rm = TRUE),
             either = mean(madeEliminatePledge | madeReducePledge, na.rm = TRUE) )
# makes sense

# ~ Misc --------------------------------------

# look for problems
w1$problemsText

# # did anyone from today get to complete the survey?
# w1$date = dateify(w1$StartDate)
#
# w1 %>% filter(w1$date == "2021-05-26") %>%
#   group_by(treat) %>%
#   summarise( n(),
#              completed = sum(problemsBin != "") )


# look at distribution of dates
w1$date2 <- as.POSIXct(w1$date)

ggplot(data = w1, aes(date2, ..count..)) +
  geom_histogram() +
  theme_bw() + xlab(NULL) +
  scale_x_datetime(breaks = date_breaks("1 week"),
                   #labels = date_format("%Y-%b"),
                   limits = c( min(w1$date2),
                               max(w1$date2) ) )

# ~ Practice linking the two waves --------------------------------------

# w2 had people enter their codes from w1, so some are blank or otherwise screwed up
# cast as numeric st anything character or "" will become NA
w2$ID = as.numeric( w2$ID )

#** there were 15 duplicated IDs entered in wave 2
# in these cases, keep only the first one
table(duplicated(w2$ID))
w2 = w2[ !duplicated(w2$ID), ]

# were there any IDs entered in w2 that weren't represented in w1?
# e.g., because they mis-entered
# only 3 bad IDs
( nBadIDs = sum( !w2$ID %in% w1$ID ) )

# keep all subjects in w1 who were RANDOMIZED
#  and merge in their response data
d = left_join( w1[ !is.na(w1$treat), ],
               w2,
               by = "ID" )

# total n to be analyzed: 666
nrow(d)
# ...and number with at least some outcome data: 221
sum( !is.na(d$StartDate.y) )
# ...and number with completely non-missing outcome data (reached last question): 208
sum( !is.na(d$problemsBin.y) & d$problemsBin.y != "")


expect_equal( is.na(d$treat), rep(FALSE, nrow(d)) )

# summary of some important variables
t = d %>%
  group_by(treat) %>%
  summarise( elim = mean(madeEliminatePledge),
             reduce = mean(madeReducePledge),
             either = mean(madeEliminatePledge | madeReducePledge),
             
             targetDemo = mean(targetDemographics),
             simplerModerator = mean(party == "Democrat" & highEduc == 1) ) %>%
  mutate_all( function(x) round(x, 2) )
as.data.frame(t)


# ~ Run raffle for first wave --------------------------------------

# SAVE THIS
w1.candidates = d$email[ !d$email == "" ]
length(w1.candidates)

# give
set.seed(451)
w1.winners = sample(w1.candidates, 10)

setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data")
write.csv(w1.winners, "study3_w1_raffle_winners_IDENTIFIABLE_INFO.csv")

# ~ Run raffle for second wave --------------------------------------

# second part of logic means they finished second wave
w2.candidates = d %>% filter( email != "" & problemsBin.y != "" & wantsRaffle == "Yes" ) %>%
  select(email, StartDate.x)

w2.candidates$StartDate.x

# use the last few chronologically to avoid issue with gift card amount mistake
w2.winners = w2.candidates[ (nrow(w2.candidates) - 4):nrow(w2.candidates), ]
dim(w2.winners)
w2.winners$StartDate.x


setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data")
write.csv(w2.winners$email, "study3_w2_raffle_winners_IDENTIFIABLE_INFO.csv")



