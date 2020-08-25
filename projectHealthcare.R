setwd('C:/Users/ilaria/Desktop/Projects/healthcare')
mydata= read.csv('HospitalCosts.csv')
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
aggregate(mydata$TOTCHG, by=list(age=mydata$AGE), sum)
aggregate(mydata$TOTCHG, by=list(age=mydata$AGE), mean)

#the age range most frequently treated is the 0years old, with 307 cases
#an average cost of 2208.85 and a total cost of 678.118



#-2################################################################
##In order of severity of the diagnosis and treatments and to find out the
##expensive treatments, the agency wants to find the diagnosis related group
##that has maximum hospitalization and expenditure
#################################################################

hist(mydata$APRDRG)

freq.dia= summary(as.factor(mydata$APRDRG))
max(freq.dia)
sort(freq.dia, decreasing= TRUE)

totcost= aggregate(mydata$TOTCHG, by=list(diagnosis=mydata$APRDRG), sum)
totcost[order(totcost$x , decreasing= TRUE),]

mydataAPRDRG=as.factor(mydata$APRDRG)
meancost= aggregate(mydata$TOTCHG, by=list(diagnosis=mydata$APRDRG), mean)
meancost[order(meancost$x, decreasing=TRUE),]

hospitalization= aggregate(mydata$LOS, by=list(diagnosis=mydata$APRDRG),mean)
hospitalization[order(hospitalization$x, decreasing = TRUE),]



#-3###############################################
#To make sure that there is no malpractice, the agency needs to analyze if
#the race of the patient is related to the hospitalization costs.
#################################################

str(mydata)
mydata$RACE=as.factor(mydata$RACE) #transform the variable

racecost= summarySE(mydata,'TOTCHG','RACE')
racecost = racecost[-which(is.na(racecost$RACE)), ] #remove NAs
print(racecost) #calculate the average cost

g = ggplot(racecost, aes(RACE, TOTCHG))
g + geom_col(colour = "red", fill = "red") +
  theme_bw(base_size = 12)

aggregate(mydata$TOTCHG, by=list(race=mydata$RACE), sum) #calculate the total cost



racelm = lm(TOTCHG~RACE, mydata, na.action=na.omit) #inferential
summary(racelm)






# race N1 is the most hospitalised with 484 cases, a total cost of 1341972,
# an average cost of 2772.669


#-4####################################################################
###To properly utilize the costs, the agency has to analyze the severity of the
#hospital costs by age and gender for proper allocation of resources
#################################################################

mydata$AGE=as.factor(mydata$AGE)

hosdemo= aggregate(mydata$TOTCHG, by=list(age=mydata$AGE),sum)#total cost by age
hosdemo[order(hosdemo$x, decreasing= TRUE),]


hosage= summarySE(mydata,'TOTCHG','AGE')#average cost by age
hosage[order(hosage$TOTCHG, decreasing = TRUE),]


#the most expensive age is 9, with an average cost of 10.573,500
mydata$FEMALE= as.factor(mydata$FEMALE)

bar <- ggplot(mydata, aes(x = FEMALE, y = TOTCHG))

bar+stat_summary(fun= mean, geom = "bar", fill = "lightgreen", colour = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width=0.3)

gndcost= summarySE(mydata,'TOTCHG','FEMALE')
print(gndcost)

totgndcost=aggregate(mydata$TOTCHG, by=list(gender=mydata$FEMALE), sum)
print(totgndcost)

# On average, the cost for men is higher than woman (3.013, 89 vs 2.546,1)



#-5#######################################
#Since the length of stay is the crucial factor for inpatients, the agency wants
#to find if the length of stay can be predicted from age, gender, and race.
###########################################
str(mydata)
mydata$RACE = as.factor(mydata$RACE)
mydata$FEMALE= as.factor(mydata$FEMALE)
mydata$AGE=as.numeric(mydata$AGE)

fit <- lm(LOS ~ AGE + FEMALE + RACE, data=mydata, na.action = na.omit)
summary(fit) # show results
print(fit)

unique(mydata$AGE)

agelos= aggregate(mydata$LOS,by=list(age=mydata$AGE),mean)
agelos[order(agelos$x, decreasing= TRUE),]

#none of the variable statistically predict the LOS, although age approximates statistical significance
# with the age of 4 being hospitalised for longer

#-6##########################################
##### To perform a complete analysis, the agency wants to find the variable that
#mainly affects the hospital costs.
#############################################

fit2 <- lm(TOTCHG ~ AGE + FEMALE + RACE + LOS + APRDRG, data=mydata)

#or

fit2 <- lm(TOTCHG ~ ., data=mydata)

summary(fit2) # show results

#LOS, diagnosis and age significantly affect the costs.
