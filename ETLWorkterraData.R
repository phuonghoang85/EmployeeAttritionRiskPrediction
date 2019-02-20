library(data.table)
library(dplyr)
library(anytime)
rm(list = ls())
# setwd("/Users/phhoang/Edge/PropensityToLeave/PostHirePTL")


### Read data ###
DT <- fread("./data/EmployeeAnonymousData.csv")
colnames(DT) <- c(
  "ClientName",
  "FirstName",
  "LastName",
  "DOB",
  "CurrentJobTitle",
  "StreetAddress",
  "StreetAddress2",
  "City",
  "State",
  "County",
  "EmploymentDate",
  "EmploymentStatus"
)


### Feature Engineering ###
today <- as.POSIXct("2017-07-01 00:00:00 EDT")
DT$DOB <- anydate(DT$DOB)
DT$EmploymentDate <- anydate(DT$EmploymentDate)
DT$EmploymentYear <- substring(DT$EmploymentDate,1,4)
DT$Age <- difftime(today, DT$DOB, units = "days") / 365
DT$Age <- round(DT$Age, digits = 1)
DT$Tenure <-
  difftime(today, DT$EmploymentDate, units = "days") / 365
DT$Tenure <- round(DT$Tenure, digits = 1)
DT <- DT[,-c("StreetAddress2", "County")]

### Job Title Attribute ###
DT<- DT[order(DT$CurrentJobTitle, decreasing = FALSE),]
DT$CurrentJobTitle <- gsub("\"", "", DT$CurrentJobTitle)
DT$CurrentJobTitle <- gsub(".*:", "", DT$CurrentJobTitle)
DT$CurrentJobTitle <- gsub("Sr ", "Senior ", DT$CurrentJobTitle)
DT$CurrentJobTitle <- gsub("Dr.", "Director", DT$CurrentJobTitle)
# DT$CurrentJobTitle<-gsub("Dir","Director",DT$CurrentJobTitle)
DT$EmploymentStatus <- as.factor(DT$EmploymentStatus)
DT$CurrentJobTitle <- as.factor(DT$CurrentJobTitle)
DT$EmploymentYear <- as.factor(DT$EmploymentYear)
# summary(DT$EmploymentStatus)
# summary(DT$CurrentJobTitle)

DT$Attrition <- NA
DT$Attrition[DT$EmploymentStatus == "Active"] <- 0
DT$Attrition[DT$EmploymentStatus == "COBRA"] <- 1
DT$Attrition[DT$EmploymentStatus == "Deceased"] <- 1
DT$Attrition[DT$EmploymentStatus == "FMLA"] <- 0
DT$Attrition[DT$EmploymentStatus == "Inactive"] <- 1
DT$Attrition[DT$EmploymentStatus == "LOA"] <- 1
DT$Attrition[DT$EmploymentStatus == "Rehired"] <- 0
DT$Attrition[DT$EmploymentStatus == "Retired"] <- 1
DT$Attrition[DT$EmploymentStatus == "Terminated"] <- 1

save(DT, file = "WorkterraEmployeeAnonymousData_clean.Rda")

### Remove noise
DT<-DT[DT$EmploymentStatus!="Retired"]
DT<-DT[DT$EmploymentStatus!="Inactive"]
DT<-DT[DT$EmploymentStatus!="FMLA"]
DT<-DT[DT$EmploymentStatus!="COBRA"]
DT<-DT[DT$EmploymentStatus!="Deceased"]

idx_terminate <- which(DT$EmploymentStatus=="Terminated")
set.seed(1234)
idx_terminate_select <- sample(idx_terminate, size = 20000)
DT_rebalance<- DT[-idx_terminate_select,]

save(DT_rebalance, file = "WorkterraEmployeeAnonymousData_clean_rebalance.Rda")






# DTa <- DT
# setDT(DTa)[, JobTitleFrequency := .N, by=CurrentJobTitle]
# 
# setDT(DTa)[, EmploymentYearFrequency := .N, by=EmploymentYear]
# DTa<-DTa[order(DTa$EmploymentYearFrequency, decreasing = TRUE),]
# 
# summary(DTa$EmploymentYear)
# 
# DTUniqueJobTille <-
#   DT[!duplicated(as.character(DT$CurrentJobTitle)), 
#      c("CurrentJobTitle", "EmploymentStatus", "Attrition")]
