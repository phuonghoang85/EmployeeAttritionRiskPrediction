#packages preparation
library(readr)
library(ggplot2)
library(scales)
library(lubridate) 
library(gridExtra) 
library(plyr)
library(ggthemes)
library(xtable)
library(stats)
#Setup all scatterplot

rm(list = ls())
graphics.off()
setwd("/Users/phhoang/Edge/PropensityToLeave/PostHirePTL")
ibm.attrition.data <- read_tsv("./data/WA_Fn-UseC_-HR-Employee-Attrition.tsv")
attach(ibm.attrition.data)

line1 = ggplot(ibm.attrition.data,aes(WorkLifeBalance,DistanceFromHome,color=factor(Attrition)))
line1 = line1 +geom_jitter()+ggtitle("DistanceFromHome Based on Work Life Balance")+
  theme_economist()

print(line1)

line2 = ggplot(ibm.attrition.data,aes(WorkLifeBalance,MaritalStatus,color=factor(Attrition)))
line2 = line2 +geom_jitter()+ggtitle("Marital status Based on Work Life Balance")+
  theme_economist()
print(line2)


line3 = ggplot(ibm.attrition.data,aes(WorkLifeBalance,MonthlyIncome,color=factor(Attrition)))
line3 = line3 +geom_jitter()+ggtitle("Monthly income Based on Work Life Balance")+
  theme_economist()

print(line3)

line4 = ggplot(ibm.attrition.data,aes(JobSatisfaction,MonthlyIncome,color=factor(Attrition)))
line4 = line4 +geom_jitter()+ggtitle("Monthly income Based on Job satisfaction")+
  theme_economist()

print(line4)

line5 = ggplot(ibm.attrition.data,aes(JobSatisfaction,TotalWorkingYears,color=factor(Attrition)))
line5 = line5 +geom_jitter()+ggtitle("Total working years Based on Job satisfaction")+
  theme_economist()

print(line5)

line6 = ggplot(ibm.attrition.data,aes(JobSatisfaction,YearsSinceLastPromotion,color=factor(Attrition)))
line6 = line6 +geom_jitter()+ggtitle("Years since last promotion Based on Job satisfaction")+
  theme_economist()

print(line6)

line7 = ggplot(ibm.attrition.data,aes(JobSatisfaction,YearsAtCompany,color=factor(Attrition)))
line7 = line7 +geom_jitter()+ggtitle("Years at company Based on Job satisfaction")+
  theme_economist()

print(line7)

line8 = ggplot(ibm.attrition.data,aes(JobSatisfaction,YearsInCurrentRole,color=factor(Attrition)))
line8 = line8 +geom_jitter()+ggtitle("Years in current role Based on Job satisfaction")+
  theme_economist()

print(line8)

line9 = ggplot(ibm.attrition.data,aes(JobSatisfaction,PercentSalaryHike,color=factor(Attrition)))
line9 = line9 +geom_jitter()+ggtitle("Salary hike percent Based on Job satisfaction")+
  theme_economist()

print(line9)

line10 = ggplot(ibm.attrition.data,aes(x=JobSatisfaction,color=factor(Attrition),y=OverTime))
line10 = line10 +geom_jitter()+ggtitle("Overtime Based on Job satisfaction")+
  theme_economist()

print(line10)

line11 = ggplot(ibm.attrition.data,aes(x=JobSatisfaction,y=JobInvolvement,color=factor(Attrition)))
line11 = line11 +geom_jitter()+ggtitle("Job involvement Based on Job satisfaction")+
  theme_economist()

print(line11)

line12 = ggplot(ibm.attrition.data,aes(x=JobRole,y=Department,color=factor(Attrition)))
line12 = line12 +geom_jitter()+ggtitle("Job involvement Based on Job satisfaction")+
  theme_economist()

print(line12)

line13 = ggplot(ibm.attrition.data,aes(x=Age,y=MonthlyIncome,color=factor(Attrition)))
line13 = line13 +geom_jitter()+ggtitle("Monthly income Based on age")+
  theme_economist()

print(line13)


line14 = ggplot(ibm.attrition.data, aes(NumCompaniesWorked,fill = factor(Attrition)))
line14 = line14 + geom_histogram(stat="count")
print(line14)
tapply(as.numeric(ibm.attrition.data$Attrition) - 1 ,ibm.attrition.data$NumCompaniesWorked,mean)

line15 = ggplot(ibm.attrition.data, aes(TotalWorkingYears,fill = factor(Attrition)))
line15 = line15 + geom_histogram(stat="count")
print(line15)
tapply(as.numeric(ibm.attrition.data$Attrition) - 1 ,ibm.attrition.data$TotalWorkingYears,mean)

#Plot correlation 
install.packages("corrplot")
library(corrplot)
mine = IBM1[2]
mydata = IBM1[c(1,4,5,7,8,10,12:17,19:25)]
mydata = cbind(mydata,mine)
Cor = cor(mydata)
corrplot(Cor,method = "shade")

#Three dimension
ggplot(ibm.attrition.data, aes(OverTime, Age)) +  
  facet_grid(.~MaritalStatus) +
  geom_jitter(aes(color = Attrition),alpha = 0.4) +  
  ggtitle("x=Overtime, y= Age, z = MaritalStatus , t = Attrition") +  
  theme_light()
#Table making
require(MASS)
require(dplyr)
StatusCount <- as.data.frame.matrix(ibm.attrition.data %>%
                                      group_by(MaritalStatus) %>%
                                      select(Attrition) %>%
                                      table())
StatusCount$TOTAL <-StatusCount$No+ StatusCount$Yes
StatusCount$PercentNo <-StatusCount$No/(StatusCount$TOTAL)*100
StatusCount$PercentYes <-StatusCount$Yes/(StatusCount$TOTAL)*100                   
StatusCount
#summary
attrition = ibm.attrition.data$Attrition 
environmentsatisfaction = ibm.attrition.data$EnvironmentSatisfaction
attrition[which(attrition=="Yes")] = 1
attrition[which(attrition=="No")] = 0 
performancerating = ibm.attrition.data$PerformanceRating
md2 = glm(factor(attrition) ~ performancerating, data=ibm.attrition.data, family=binomial(link="logit")) 
summary(md2)

totalworking = ibm.attrition.data$TotalWorkingYears
md3 = glm(factor(attrition) ~ totalworking, data=ibm.attrition.data, family=binomial(link="logit"))
summary(md3)
