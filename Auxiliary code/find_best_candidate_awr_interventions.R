
library(dplyr)
library(MetaUtility)

data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction"

# prepped dataset
setwd(data.dir)
d = read.csv("prepped_data.csv")
d = d %>% filter( !is.na(authoryear) )  # because that table has blank rows for prettiness

# main-analysis dataset without SSWS
expect_equal( sum( is.na(d$exclude.main) ), 0 )  # indicator for being a high-bias challenge should never be NA
d = d[ d$use.rr.analysis == 1 & d$exclude.main == 0, ]  
d = droplevels(d)



##### Make High-Quality Variable #####
# how many meet bar of high quality?
d = d %>% mutate( hi.qual = (randomized == TRUE) & 
                    qual.exch %in% c("a.Low", "b.Medium") &
                    qual.sdb %in% c("a.Low", "b.Medium") &  # this is the killer
                    !is.na(qual.missing) & qual.missing < 15 )   # reducing this to 5 doesn't change number of studies
# which are high-quality?
table(d$hi.qual)
unique( d$authoryear[ d$hi.qual == TRUE ] )


d$ens = calib_ests(yi = d$logRR, 
               sei = sqrt(d$varlogRR))
d$ensRR = exp(d$ens)

d %>%
  filter(qual.y.prox != "c.Intended" &
           qual.exch %in% c("a.Low", "b.Medium") &
           qual.sdb %in% c("a.Low", "b.Medium") &  # this is the killer
           !is.na(qual.missing) & qual.missing < 15 ) %>%
  #filter(hi.qual == TRUE) %>%
  select(authoryear,
         substudy,
         logRR,
         ensRR,
         n.paper,
         qual.y.prox,
         qual.exch,
         qual.sdb,
         qual.gen,
         x.min.exposed,
         prose.x)


t = d %>%
  filter(qual.y.prox != "c.Intended" 
         # qual.exch %in% c("a.Low", "b.Medium") &
         # qual.sdb %in% c("a.Low", "b.Medium") &  # this is the killer
         # !is.na(qual.missing) & qual.missing < 15
  ) %>%
  #filter(hi.qual == TRUE) %>%
  select(authoryear,
         substudy,
         logRR,
         ensRR,
         n.paper,
         qual.y.prox,
         qual.exch,
         qual.sdb,
         qual.gen,
         qual.missing,
         x.min.exposed,
         prose.x) %>%
  filter(ensRR > 1.1) %>%
  arrange( desc(ensRR) )


View(t)

# how many studies used EIYLM?


# definitely Anderson 2016 because actual consumption and tractable intervention
# intervention is very short and available in paper
d$prose.x[ d$authoryear == "Anderson 2016" ]


# Norris 2016 EIYLM, but note that in 2/3 studies, its RR was close to 1:
d$ensRR[ grepl( x = d$prose.x, pattern = "Even If" ) ]

# "Speciesism" was in 2 studies:
d$ensRR[ grepl( x = d$prose.x, pattern = "Species" ) ]

d$ensRR[ grepl( x = d$prose.x, pattern = "Your Choice" ) ]

d$ensRR[ grepl( x = d$prose.x, pattern = "Compassionate" ) ]

d$ensRR[ grepl( x = d$prose.x, pattern = "Simple Way" ) ]

