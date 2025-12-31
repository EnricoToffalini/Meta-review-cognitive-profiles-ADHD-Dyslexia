library(readxl)

d = data.frame(read_excel("coded-Dataset.xlsx"))
for(i in 1:nrow(d)){
  if(d$Domain_L1_general[i]=="Intelligence / general cognition" & 
     is.na(d$Domain_L2_specific[i])){
    d$Domain_L2_specific[i] = "Intelligence / general cognition"
  }
}
write.csv(d,"dd.csv")
