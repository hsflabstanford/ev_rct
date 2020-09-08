

# complete-case prepped dataset
setwd(prepped.data.dir)
d = read.csv("prepped_FAKE_data.csv")

setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Fake simulated data")
w2 = read.csv("raw_simulated_wave2.csv", header = TRUE)

# to check: missing data should only happen when someone entirely did not do wave 2
# demographics should never be NA

####################### CHECK RECODING OF FOOD VARIABLES ####################### 

##### Reproduce One Food Variable #####
beef_Freq = recode( w2$beef_Freq,
                           a.Never = 0,
                           b.1Weekly = 1,
                           c.2Weekly = 2,
                           d.3to4Weekly = 3.5,
                           e.5to6Weekly = 5.5,
                           f.1Daily = 7,
                    # @ I removed the trailing space in latest Qualtrics version
                           "g.2PlusDaily " = 14 ) 

table(beef_Freq)

w2$beef = beef_Freq * w2$beef_Ounces

# randomly choose a subject from prepped data and check for agreement
id = sample(d$ID[ !is.na(d$beef) ], size = 1)
d$beef[ d$ID == id]
# @ insert the prolific ID variable here
s


##### Reproduce One Psych Variable #####