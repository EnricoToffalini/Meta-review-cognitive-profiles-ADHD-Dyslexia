
##############################

library(readxl)

##############################

#  SCREENING OF MAIN DATA

d = data.frame(read_excel("Data/screening-Data-main.xlsx"))
table(d$phase1_flag_inclusion)
table(d$phase2_flag_inclusion)
table(d$phase2_reasonExclusion)

##############################

#  SCREENING OF ADDITIONAL DATA 

da = data.frame(read_excel("Data/screening-additional.xlsx"))
table(da$flag1)
table(da$flag2)

##############################


      