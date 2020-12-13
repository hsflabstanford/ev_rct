
# Makes comma-separated lists of subjects to block from Study 2
#  i.e., anyone who was a pilot before Study 1 or completed any part of the real Study 1

##### Pre-Study 1 Pilots #####

setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Pilot data/Prolific IDs for Blocklist")

block = ""

for (i in list.files()) {
  temp = read.csv(i)
  block = paste( block, paste( temp$participant_id, collapse = ","), collapse = "," )
}


##### Study 1 (both waves) #####

setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped")

temp = read.csv("prepped_merged_data.csv")

block = paste(block, paste( temp$ID, collapse = ","), collapse = ",")



##### Save the Lists #####

setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Materials/Qualtrics materials/Study 2/Blocklist")

write.table(block, "blocklist.txt")
