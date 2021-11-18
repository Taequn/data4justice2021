datathon = read.csv("cleanedAndUpdated.csv")
View(datathon)

library(ez)
data_subsetted = dplyr::select(datathon, c(fullname, raceChar, timeDif, impose, rateSev, sex))
data_subsetted = na.omit(data_subsetted)

data_subsetted$fullname = as.factor(data_subsetted$fullname)
ezANOVA(data = data_subsetted, dv = timeDif, wid = .(fullname), within = .(raceChar))
table(data_subsetted$fullname, data_subsetted$raceChar)


data_subsetted = data_subsetted %>% filter(raceChar !="Unknown", raceChar!= "Other", impose !=1)
  
data_subsetted$raceChar = as.factor(data_subsetted$raceChar)
data_subsetted$raceChar = relevel(data_subsetted$raceChar, ref ="White")
table(data_subsetted$raceChar)
lm_race = lmer(timeDif ~ raceChar + (1 | fullname) +(1|rateSev), data = data_subsetted)
summary(lm_race)


