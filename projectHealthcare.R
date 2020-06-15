mydata= (HospitalCosts)
install.packages('Rmisc')
install.packages('dplyr')

library('Rmisc')
library('ggplot2')
library('dplyr')

head(mydata)

#-1###############################################################
#To record the patient statistics, the agency wants to find the age category
#of people who frequent the hospital and has the maximum expenditure.
##################################################################
summary(as.factor(mydata$AGE))
hist(mydata$AGE)
summarySE(mydata, 'TOTCHG', groupvars= 'AGE',na.rm = TRUE)
aggregate(mydata$TOTCHG, by=list(age=mydata$AGE), sum)

#the age rage most frequently treated is the 0years old, with 307 cases
#an average cost of 2208.85 and a total cost of 678.118



#-2################################################################
##In order of severity of the diagnosis and treatments and to find out the
##expensive treatments, the agency wants to find the diagnosis related group
##that has maximum hospitalization and expenditure
#################################################################
summarySE(mydata, 'TOTCHG','APRDRG')

freq.dia= summary(as.factor(mydata$APRDRG))
max(freq.dia)
sort(freq.dia, decreasing= TRUE)

totcost= aggregate(mydata$TOTCHG, by=list(diagnosis=mydata$APRDRG), sum)
print(totcost)
max(totcost$x)

#the most frequent treatment is the num 640 with 267 cases and the highest total cost of 437.978
#average cost of 1640.37 and 2.44 days of hospedalization

meancost= aggregate(mydata$TOTCHG, by=list(diagnosis=mydata$APRDRG), mean)
sort(meancost$x, decreasing=TRUE)

#on average, the 911 is the group more expensive with 48.388 mean cost per treatment

hospitalization= aggregate(mydata$LOS, by=list(diagnosis=mydata$APRDRG),mean)
print(hospitalization)

#on average, the 602 group is the one with longer permanence, 41 days on average
#with an average and total cost of 29.188 (only one case)

#-3###############################################
#To make sure that there is no malpractice, the agency needs to analyze if
#the race of the patient is related to the hospitalization costs.
#################################################
str(mydata)
racecost= summarySE(mydata,'TOTCHG','RACE')
print(racecost)
racecost = racecost[-which(is.na(racecost$RACE)), ]
barplot(racecost$TOTCHG, racecost$RACE)

g = ggplot(racecost, aes(RACE, TOTCHG))
g + geom_col(colour = "red", fill = "red") +
  theme_bw(base_size = 12)

help("geom_col")
# g = ggplot(mydata, aes(TOTCHG, RACE))
# g + geom_bar(colour = "red")

aggregate(mydata$TOTCHG, by=list(age=mydata$RACE), sum)
 # race N1 is the most hospitalised with 484 cases, a total cost of 1341972,
# an average cost of 2772.669
  