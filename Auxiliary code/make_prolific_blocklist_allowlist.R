
# Makes comma-separated lists of subjects to allow into wave 2 (i.e., they completed Wave 1)
#  and to block (i.e., they started or completed any pilot version)

##### Blocklist #####

setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Pilot data/Prolific IDs for Blocklist")

block = ""

for (i in list.files()) {
  temp = read.csv(i)
  block = paste( block, paste( temp$participant_id, collapse = ","), collapse = "," )
}


##### Allowlist #####

# only allow subjects who will actually be analyzed from Wave 1

setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped")

temp = read.csv("prepped_data_W1_R1.csv")

allow = paste( temp$ID, collapse = ",")


##### Save the Lists #####

setwd("~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Materials/Qualtrics materials/Wave 2 Blocklist and Allowlist")

write.table(block, "blocklist.txt")
write.table(allow, "allowlist.txt")