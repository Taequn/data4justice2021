library(tidyverse)
library(GGally)
library(ez)
library(VGAM)
library(lme4)
library(patchwork)


############################################################
# Ideas:
# separate positive/negative
# do absolute difference
############################################################



############################################################
# Reading data
############################################################
csv.1<-read_csv("countycodes.csv")
csv.2<-read_csv("grids.csv")
csv.3<-read_csv("allmn.csv")



############################################################
# Working with data
############################################################
csv.3.updated <- csv.3 %>%
  mutate(fullname = paste(jlname, jfname))%>%
  mutate(rateSev = case_when(severity <= 12 ~ "Standard",
                             (severity >= 13 & severity <= 20) ~ "Sexual assault",
                             (severity >= 51 & severity <= 59) ~ "Drugs",
                             TRUE ~ "Something's wrong"))%>%
  #Establish the postive/negative time difference
  
  
  #########
  # PLLEEEAAASE
  # TO DO:
  # double check the it works fine because I'm only 70% certain in this part
  #########
  mutate(timeDif = case_when(
      confine > Maxtime ~ (confine-Maxtime),
      confine < Mintime ~ -1*(Mintime-confine),
      TRUE ~ 0
   ))%>%
  mutate(timeDifTimes = case_when(
    confine > Maxtime ~ (confine/Maxtime),
    confine < Mintime ~ (confine/Mintime),
    TRUE ~ 0
  ))%>%
  #creating a column for neg/positive bias
  mutate(bias = case_when(
    timeDif < 0 ~ "NEG",
    timeDif > 0 ~ "POS",
    timeDif == 0 ~ "NO"
  ))%>%
  mutate(raceChar = case_when(
    race==1 ~ "White",
    race==2 ~ "Black",
    race==3 ~ "Native",
    race==4 ~ "Hispanic",
    race==5 ~ "Asian",
    race==6 ~ "Other",
    race==7 ~ "Unknown"
  ))

#
# Test run on the number of judges with bias
#
csv.3.updated %>%
  group_by(fullname, bias)%>%
  summarise(
    biased = n()
  )%>%
  filter(bias=="POS")%>%               #looking for positive bias
  arrange(-biased)

#
#Check a single judge
#
csv.3.updated %>%
  filter(fullname=="Quam Jay M.",          #Choose the name here
         bias=="POS")%>%
  group_by(raceChar)%>%
  summarise(
    time = mean(timeDif)
  )

#
#Checking mean bias over sample
#
csv.3.updated %>%
  filter(raceChar!="Unknown",
         raceChar!="Other",
         !(is.na(raceChar)),
         inctype!=2)%>%
  group_by(raceChar)%>%
  summarise(
    NumOfCases = n(),
    MeanTimeDif = mean(timeDif),
  )


########################################################################
#
# IMPORTANT OBSERVATION
# 1) Either double check how we count timeDif
# 2) Or everyone gets less than they deserve.
# Gotta figure it out, guys. 
#
########################################################################



#
#Let's plot the data
#

csv.3.updated %>%
  filter(bias!="NO",
         !(is.na(raceChar)),
         raceChar!="Unknown",
         raceChar!="Other")%>%
  ggplot(aes(x=raceChar, y=timeDif))+
  geom_violin(fill="dark red")
#
#We observe HUGE min/max for Black.
#Weird thing to explore: we might calculate timeDif wrongly:
#Most of the sentences are undersentenced
#Meaning that people got less time
#Weird.
#

########################################################################
#
# BASIC ANOVA TESTING (I suck at it, so please check, thanks)
#
########################################################################

#Research question:
#Is overall sentencing mean different (excluding unbiased observations)
#Different between races
#H0: mu1==mu2==mu3==mu4==mu5
#H1: population means mu_k are not equal

#ANOVA test
anova.data <- csv.3.updated%>%
  filter(!(is.na(raceChar)),
         raceChar!="Unknown",
         raceChar!="Other",
         inctype!=2)
anova.dat <- TukeyHSD(aov(timeDif ~ raceChar, data=anova.data))$raceChar
anova.dat
#We have enough evidence to reject null hypothesis in favor of the alternative

#VIZ
ggdat<-cbind(data.frame(difference=rownames(anova.dat)),
             anova.dat)


ggplot(data=ggdat,aes(x=difference,y=diff))+
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw()+
  xlab("Pairwise Difference")+
  ylab("Estimated Difference in prison sentences (Months)")






########################################################################
#
# Exploring if a judge is more biased for a race
# Way to do it:
# 1) Create a separate copy of csv.3.updated
# 2) Create a helper function to pull meanDifs for races per name
# 3) Loop through data frame and create 5 more columns based on diffs
########################################################################

#helper function assessing the bias towards races for a name
timeBias <- function(name, race){
  answer<-csv.3.updated %>%
    filter(!(is.na(raceChar)),
           raceChar!="Unknown",
           raceChar!="Other",
           inctype!=2)%>%
    filter(fullname==name)%>%
    group_by(raceChar)%>%
    summarise(
      meanDif = mean(timeDif)
    )%>%
    filter(raceChar==race)
  
  ifelse(nrow(answer)>0, as.double(answer[,2]), NA)
}
timeBias("Merkins Tammy L.","Black")


#Tomorrow:
#Build a dataframes that would have:
#1) Name of each judge+their respective biases against races.
#2) Compare different counties? Make a heatmap of the strictest/racist policies
#3) Compare gender punishements

judges.dat <- anova.data %>%
  mutate(asianBias = case_when(raceChar=="Asian" ~ timeDif,
                               TRUE ~ 0),
         asianBiasT = case_when(raceChar=="Asian" ~ timeDifTimes,
                               TRUE ~ 0),
         blackBias = case_when(raceChar=="Black" ~ timeDif,
                               TRUE ~ 0),
         blackBiasT = case_when(raceChar=="Black" ~ timeDifTimes,
                               TRUE ~ 0),
         hispanicBias = case_when(raceChar == "Hispanic" ~ timeDif,
                                  TRUE ~ 0),
         hispanicBiasT = case_when(raceChar == "Hispanic" ~ timeDifTimes,
                                  TRUE ~ 0),
         nativeBias = case_when(raceChar == "Native"~ timeDif,
                                TRUE ~ 0),
         nativeBiasT = case_when(raceChar == "Native"~ timeDifTimes,
                                TRUE ~ 0),
         whiteBias = case_when(raceChar == "White" ~ timeDif,
                               TRUE ~ 0),
         whiteBiasT = case_when(raceChar == "White" ~ timeDifTimes,
                               TRUE ~ 0))



################################################
# Differences between sexes
###############################################
anova.data %>% 
  group_by(sex)%>%
  summarise(
    meanTime = mean(timeDif)
  )

#H0: u1=u2
#H1: u1!=u2
t.test(x=anova.data$timeDif, y=anova.data$sex, alternative="two.sided",
       paired=T)
#Enough evidence to support H0




########################################################################
# Chisq.test
########################################################################
csv.3.plea <- csv.3.updated %>%
  mutate(pleaSuccess = case_when(stayexec==1~ 1,
                                 impose==1 ~ 1,
                                 TRUE ~ 0))%>%
  filter(!(is.na(raceChar)),
         raceChar!="Unknown",
         raceChar!="Other")%>%
  mutate(sexChar = case_when(
    sex==1 ~ "Male",
    sex==2 ~ "Female"
  ))

csv.3.pleaWvNW <- csv.3.plea %>%
  mutate(raceChar=case_when(
    raceChar=="White" ~ "White",
    TRUE ~ "Non-white"
  ))%>%
  group_by(fullname, raceChar, pleaSuccess)%>%
  summarise(
    n=n()
  )%>%
  mutate(prop = prop.table(n))

csv.3.plea.plot <- csv.3.plea %>%
  mutate(pleaSuccess = case_when(
    pleaSuccess == 1 ~ "Yes",
    TRUE ~ "No"
  ))%>%
  group_by(sexChar, pleaSuccess)%>%
  summarise(
    n = n()
  )

p1<-ggplot(csv.3.plea.plot, aes(x=sexChar, y=n, fill=pleaSuccess))+
  geom_bar(position="fill", stat="identity")+
  labs(x="Gender",
       y="Proportion",
       fill="Plea success",
       title="Proportion of successful pleas per race")


csv.3.plea.plot1 <- csv.3.plea %>%
  mutate(pleaSuccess = case_when(
    pleaSuccess == 1 ~ "Yes",
    TRUE ~ "No"
  ))%>%
  mutate(pleaPoss = case_when(severity <= 7 ~ "Yes",
                            (severity >= 13 & severity <= 16) ~ "Yes",
                            (severity >= 51 & severity <= 57) ~ "Yes",
                            TRUE ~ "No"))%>%
  filter(pleaPoss=="No")%>%
  group_by(sexChar, pleaSuccess)%>%
  summarise(
    n = n()
  )

p2<-ggplot(csv.3.plea.plot1, aes(x=sexChar, y=n, fill=pleaSuccess))+
  geom_bar(position="fill", stat="identity")+
  labs(x="Gender",
       y="Proportion",
       fill="Plea success",
       title="Proportion of successful pleas per race",
       subtitle="For cases with no suggested probation")

(p1|p2)

###########################################################################
# County data
###########################################################################
csv.3.county <- csv.3.plea %>%
  mutate(raceChar=case_when(
    raceChar=="White" ~ "White",
    TRUE ~ "Non-white"
  ))%>%
  group_by(county, raceChar, pleaSuccess)%>%
  summarise(
    n=n()
  )%>%
  mutate(prop = prop.table(n))%>%
  filter(pleaSuccess==1)

csv.3.county.w <- csv.3.county %>%
  filter(raceChar=="White") %>%
  rename(prop_white=prop)

csv.3.county.nw <- csv.3.county %>%
  filter(raceChar=="Non-white")

csv.3.county.w <- bind_cols(csv.3.county.w, csv.3.county.nw$prop)
  
county.df <- csv.3.county.w %>% 
  rename(prop_nwhite = ...6)%>%
  mutate(div_prop = prop_white/prop_nwhite,
         effect = prop_white-prop_nwhite)

write_csv(county.df, "countyData.csv")

####
# Analysis for the model.
####
even_nw <- csv.3.pleaWvNW %>% filter(pleaSuccess==1,
                                        raceChar=="Non-white")

even_w <- csv.3.pleaWvNW %>% filter(pleaSuccess==1,
                                        raceChar=="White")%>%
  filter(fullname %in% even_nw$fullname)%>%
  mutate(prop_white = prop,
         prop=NULL)

even_nw <- even_nw %>%
  filter(fullname %in% even_w$fullname)%>%
  mutate(prop_nwhite = prop,
         prop=NULL)

final.df <- bind_cols(even_nw, even_w$prop_white)%>%
  rename(prop_white=...6)%>%
  mutate(prop.final = prop_white/prop_nwhite,
         proportions = prop_white-prop_nwhite)

#H0: mu=1
#H1: mu!=1
t.test(final.df$prop.final, mu=1)
#We have strong evidence to decline H0.
final.df

##################################################################
#FITTING THE MODELS
##################################################################
library(EnvStats)
pareto.df <- final.df%>%filter(proportions>0)

epareto(pareto.df$proportions)
pareto.MLE <- function(X){
  n <- length(X)
  m <- min(X)
  a <- n/sum(log(X)-log(m))
  return( c(m,a) ) 
}
pareto.MLE(pareto.df$proportions)

LLpareto <- function(par, data){
  answer <- sum(dpareto(x=data, scale=par[1], shape=par[2], log=TRUE))
  -answer
}

optim(par = c(2,3),
      fn = LLpareto,
      data=pareto.df$proportions)

LLnormal <- function(par, data){
  answer <- sum(dnorm(x=data, mean=par[1], sd=par[2], log=TRUE))
  -answer
}

optim(par = c(1,1),
      fn = LLnormal,
        data=final.df$proportions)

ggplot(final.df, aes(x=proportions))+
  geom_histogram(aes(y=..density..), color="white", fill="dark red")+
  geom_function(fun=dnorm, args=list(mean=0.07507912, sd=0.11360153),
                color="black", width=2)+
  labs(x="Overall effect",
       y="Density",
       title="Histogram of overall effect of pleas",
       subtitle= "Mean: 0.075, SD: 0.113")

########

write_csv(csv.3.plea[1:2000, ], "pleaData.csv")
####
#Standard
###
standPlea <- csv.3.plea %>%
  filter(rateSev=="Standard")


####
#Sexual Assault
###
saPlea <- csv.3.plea %>%
  filter(rateSev=="Sexual assault")
table(saPlea$pleaSuccess, saPlea$raceChar)
chisq.test(table(saPlea$pleaSuccess, saPlea$raceChar))


####
#Drugs
###
drugPlea <- csv.3.plea %>%
  filter(rateSev=="Drugs")
table(drugPlea$pleaSuccess, drugPlea$raceChar)
chisq.test(table(drugPlea$pleaSuccess, drugPlea$raceChar))

#
#White/non-white
#
w.nonwhite <- csv.3.plea %>%
  mutate(raceChar = case_when(
    raceChar=="White" ~ "White",
    TRUE ~ "Non-white"
  ))%>%
  group_by(raceChar, pleaSuccess)%>%
  summarise(
    n=n()
  )

w.nonwhite


table(w.nonwhite$pleaSuccess, w.nonwhite$raceChar)
chisq.test(table(w.nonwhite$pleaSuccess, w.nonwhite$raceChar))

##
#SEVERITY THING
##
#1-7 can be non-severe
#51-57
#13-16

test <- csv.3.plea %>%
  mutate(raceChar = case_when(
    raceChar=="White" ~ "White",
    TRUE ~ "Non-white"
  ))%>%
  mutate(pleaPoss = case_when(severity <= 7 ~ "Yes",
                             (severity >= 13 & severity <= 16) ~ "Yes",
                             (severity >= 51 & severity <= 57) ~ "Yes",
                             TRUE ~ "No"))%>%
  filter(pleaPoss=="No")%>%
  group_by(raceChar, pleaSuccess)%>%
  summarise(
    n = n()
  )

test

#Focus on categories that by definition can't have a plea possibility
#VIZ
ggplot(test, aes(y=n, x=pleaSuccess, fill=raceChar))+
  geom_col(position="dodge")+
  theme_bw()+
  labs(x="Getting a probation",
       y="Counts",
       fill="Races",
       title="Success of a plea to get a probation",
       subtitle="Controling for the crimes with no recommended probation")


#H0: p1-p2==0
#Ha: p1-p2!=0
#non-white
x1<-5346
n1<-(10077+5346)
#white
x2<-8597
n2<-(8597+9090)
prop.test(x=c(x1, x2),
          n=c(n1, n2))

#p-value <- 2.2* 10^-16
#        <- We have enough evidence to reject H0 in favor of Ha



#Judges:
#Give the assessment of each judge via a function: 
#number: total biased cases*mean(bias) of that judge


############################################################
#
# TIME SERIES PLOT FOR PROBATION CASES
# 
############################################################

time.series <- csv.3.plea %>% 
  group_by(sentyear, raceChar, pleaSuccess)%>%
  summarise(
    n=n()
  )%>%
  mutate(prop = prop.table(n))%>%
  filter(pleaSuccess==1)

p4<-ggplot(time.series, aes(x=sentyear, y=prop, color=raceChar))+
  geom_line(size=2)+
  theme_bw()+
  labs(x="Year",
       y="Proportion",
       color="Races",
       title="Time graph of changes among approved probation cases",
       subtitle="All the cases")


time.series.corrected <- csv.3.plea %>%
  mutate(pleaPoss = case_when(severity <= 7 ~ "Yes",
                              (severity >= 13 & severity <= 16) ~ "Yes",
                              (severity >= 51 & severity <= 57) ~ "Yes",
                              TRUE ~ "No"))%>%
  filter(pleaPoss=="No")%>%
  group_by(sentyear, raceChar, pleaSuccess)%>%
  summarise(
    n=n()
  )%>%
  mutate(prop = prop.table(n))%>%
  filter(pleaSuccess==1)


p5<-ggplot(time.series.corrected, aes(x=sentyear, y=prop, color=raceChar))+
  geom_line(size=2)+
  theme_bw()+
  labs(x="Year",
       y="Proportion",
       color="Races",
       title="Time char of changes among approved probation cases",
       subtitle="Cases without suggested probation")

(p4/p5)

########################################################################
#
# Judges
#
########################################################################

ratingBvW <- judges.dat %>% group_by(fullname)%>%
  summarise(
    meanBB = mean(blackBias),
    meanBBt = mean(blackBiasT),
    meanWB = mean(whiteBias),
    meanWBt = mean(whiteBiasT),
    numCases = n(),
  )%>%
  filter(numCases>10)%>%
  mutate(ratingBvW=abs(meanWB)-abs(meanBB),
         ratingBvWt=abs(meanWBt)-abs(meanBBt))%>%
  arrange(-ratingBvW)
nrow(ratingBvW)

meanTimes <- mean(ratingBvW$ratingBvWt)
sdTimes <- sd(ratingBvW$ratingBvWt)


#QUANTITATIVE GROUPING
ratingBvWgroup <- ratingBvW %>%
  mutate(groups = case_when(
    ratingBvWt < (meanTimes-sdTimes) ~ "For black (-1 sd)",
    ratingBvWt > (meanTimes+sdTimes) ~ "For white (+1 sd)",
    (ratingBvWt >= (meanTimes-sdTimes)) & (ratingBvWt <= (meanTimes+sdTimes)) ~ "Middle"
  ))%>%
  group_by(groups)%>%
  filter(groups!="Middle")%>%
  summarise(
    nums = n(),
    meanTimes = mean(ratingBvW)
  )

p6<-ggplot(ratingBvWgroup, aes(x=groups, y=nums, fill=groups))+
  geom_col()+
  geom_hline(yintercept = 0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(fill="Grouping",
       y="Number of judges",
       title="Number of judges with Black vs White bias",
       subtitle="Out of 562 biased judges")

###AVERAGE EFFECT
ratingBvWeffect <- ratingBvW %>%
  mutate(groups = case_when(
    ratingBvWt < (meanTimes-sdTimes) ~ "For black",
    ratingBvWt > (meanTimes+sdTimes) ~ "For white",
    (ratingBvWt >= (meanTimes-sdTimes)) & (ratingBvWt <= (meanTimes+sdTimes)) ~ "Middle"
  ))%>%
  mutate(effect = abs(ratingBvWt)) %>%
  group_by(groups)%>%
  filter(groups!="Middle")%>%
  summarise(
    meanEff = mean(effect)
  )%>%
  mutate(meanEff = as.double(100*meanEff))

p7<-ggplot(ratingBvWeffect, aes(x=groups, y=meanEff, fill=groups))+
  geom_col()+
  geom_hline(yintercept = 0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(fill="Grouping",
       y="Mean difference (%)",
       title="Effect of biased judges")

(p6/p7)
