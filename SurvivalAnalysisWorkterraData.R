library(ggplot2)
library(survival)
require(survminer)
# install.packages("survminer")
library(dplyr)
# install.packages("OIsurv")
require(OIsurv) # Aumatically loads KMsurv
library(KMsurv)
install.packages("ranger")
library(ranger)
library(data.table)
graphics.off()

library(readr)
library(scales)
library(lubridate) 
library(gridExtra) 
library(plyr)
library(ggthemes)
# install.packages("ggthemes")
library(xtable)
library(stats)


rm(list = ls())
# setwd("/Users/phhoang/Edge/PropensityToLeave/PostHirePTL")
#------------
load("./data/WorkterraEmployeeAnonymousData_clean.Rda")
joblevel<-fread("joblevel_results.csv", header = FALSE)
names(joblevel)<-c("CurrentJobTitle","JobLevel")
DTJoinJobLevel<- inner_join(DT, joblevel, by = c("CurrentJobTitle"))
DT <- DTJoinJobLevel 
# save(DT, file = "./data/WorkterraEmployeeAnonymousData_clean_JobLevel.Rda" )
# load("./data/WorkterraEmployeeAnonymousData_clean_rebalance.Rda")
# sapply(DT, class)

# ibmData<-fread("WA_Fn-UseC_-HR-Employee-Attrition.csv")
# DT<-DT_rebalance
# write.table(DT$CurrentJobTitle, file = "./data/Workterra_JobTitle.csv", sep = ",", row.names = FALSE, col.names = TRUE)


DT$Attrition<-as.numeric(DT$Attrition)
DT$Tenure<-as.numeric(DT$Tenure)
DT$Age<-as.numeric(DT$Age)
DT<-DT[!is.na(DT$Age),]

# DT_keep<-DT
DT<-DT[DT$Tenure<=6,]

### Prepare data for modeling
DT2 <- select(DT, -c(2,3,4,6,9,10))
# sapply(DT2, class)
DT2 <- mutate(DT2,
              ClientName = as.factor(ClientName),
              City = as.factor(City),
              State = as.factor(State),
              Attrition = as.factor(Attrition),
              JobLevel = as.factor(JobLevel))
###
# Kaplan Meier Survival Curve
y_DT <- Surv(DT$Tenure, DT$Attrition)
# y_KM <- Surv(DT$Age, DT$Attrition)
# y_KM

fit_KM <- survfit(y_DT ~ 1)
summary(fit_KM)
cb <- confBands(y_DT, type = "hall")
plot(fit_KM,
     main = 'Kaplan–Meier Plot with confidence bands')
lines(cb, col = "red",lty = 3)
legend(1000, 0.99, legend = c('Kaplan–Meier survival estimate',
                              'pointwise intervals', 'Hall-Werner conf bands'), lty = 1:3)

# Fit Cox Model
form <- formula(y_DT ~ ClientName + State + Age + Tenure + JobLevel) #CurrentJobTitle
cox_model <- coxph(form, data = DT2)
# summary(cox_model)
cox_fit <- survfit(cox_model)
plot(cox_fit)

# ranger model
# r_fit <- ranger(form,
#                 data = DT2,
#                 importance = "permutation",
#                 seed = 1234)

# save(r_fit, file = "RFfit.Rda")
load("./data/RFfit.Rda")


# vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
# names(vi) <- "importance"
# head(vi)
cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)
## Prediction Error = 1 - Harrell's c-index =  0.1872401

# Average the survival models
death_times <- r_fit$unique.death.times
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)


# Plot the survival models for each employee
r_fit_data<-data.frame(x=r_fit$unique.death.times, y=r_fit$survival[1,], group=1)
set.seed(1234)
idx_all<-2:nrow(DT2)
idx<-sample(idx_all, 499)
for(n in idx){
  r_fit_n<-data.frame(x=r_fit$unique.death.times, y=r_fit$survival[n,], group=n)
  r_fit_data<-rbind(r_fit_data, r_fit_n)
}
avg_curve<-data.frame(x=death_times, y=avg_prob, group='ave')

ggplot(data=r_fit_data, aes(x=x,y=y,group=group)) + 
geom_line(alpha=I(1/5),size=0.5,color="Red") +
geom_line(data=avg_curve,aes(x=x,y=y,group=group),color ="Black") +
  ylab("Survival Probability") +
  xlab("Tenure (Years)") + 
  xlim(0,6) +
  theme(legend.title = element_blank(), text = element_text(size = 12))


# Plot the survival models for each employee with xlab = Turnover Probability
r_fit_data<-data.frame(x=r_fit$unique.death.times, y=1-r_fit$survival[1,], group=1)
set.seed(1234)
idx_all<-2:nrow(DT2)
idx<-sample(idx_all, 499)
for(n in idx){
  r_fit_n<-data.frame(x=r_fit$unique.death.times, y=1-r_fit$survival[n,], group=n)
  r_fit_data<-rbind(r_fit_data, r_fit_n)
}
avg_curve<-data.frame(x=death_times, y=1-avg_prob, group='ave')
ggplot(data=r_fit_data, aes(x=x,y=y,group=group)) + 
  geom_line(alpha=I(1/5),size=0.5,color="Blue") +
  geom_line(data=avg_curve,aes(x=x,y=y,group=group),color ="Black") +
  ylab("Turnover Probability") +
  xlab("Tenure (Years)") +
  xlim(0,6) +
  theme(legend.title = element_blank(), text = element_text(size = 12))

ggplot() + 
  geom_line(data=avg_curve,aes(x=x,y=y,group=group),color ="Black") +
  geom_line(data=r_fit_data, aes(x=x,y=y,group=group), alpha=I(1/5),size=0.5,color="Blue") +
  ylab("Turnover Probability") +
  xlab("Tenure (Years)") + 
  theme(legend.title = element_blank(), text = element_text(size = 12))


# Set up for ggplot all methods
km <- rep("Kaplan Meier", length(fit_KM$time))
km_df <- data.frame(fit_KM$time,fit_KM$surv,km)
names(km_df) <- c("Time","Surv","Model")

cox <- rep("Cox Proportional Hazard",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,cox)
names(cox_df) <- c("Time","Surv","Model")
# 
rf <- rep("Random Forests",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rf)
names(rf_df) <- c("Time","Surv","Model")
plot_df <- rbind(km_df, cox_df, rf_df)


### Kaplan Meier Survival Curve (single line)
ggplot(km_df, aes(x = Time, y = Surv, color = Model)) + 
  geom_line(size=1) +
  ggtitle("Kaplan Meier Survival Curve") + 
  ylab("Survival Probability") +
  xlab("Tenure (Years)") + 
  xlim(0,6) +
  # ylim(.20,1.0)+
  theme(legend.title = element_blank(), text = element_text(size = 12))

ggplot(plot_df, aes(x = Time, y = Surv, color = Model)) + 
  geom_line(size=1) +
  ggtitle("Survival Analysis Comparison") + 
  ylab("Turnover Probability") +
  xlab("Tenure (Years)") + 
  xlim(0,6) +
  # ylim(.20,1.0)+
  theme(legend.title = element_blank(), text = element_text(size = 12))

ggplot(plot_df, aes(x = Time, y = Surv, group="Kaplan Meier", color = Model)) +
  geom_line(size=1) +
  ggtitle("Survival Curves") +
  ylab("Survival Probability") +
  xlab("Tenure (Years)") +
  xlim(0,6) +
  # ylim(.20,1.0)+
  theme(legend.title = element_blank(), text = element_text(size = 12))

ggplot(plot_df, aes(x = Time, y = 1 - Surv, color = Model)) + 
  geom_line(size=1) +
  ylab("Turnover Probability") +
  xlab("Tenure (Years)") + 
  xlim(0,6) +
  # ylim(.20,1.0)+
  theme(legend.title = element_blank(), text = element_text(size = 12))

### 1- Kaplan Meier Curve (Turnover Probablity)
ggplot(km_df, aes(x = Time, y = 1- Surv, color = Model)) + 
  geom_line(size=1) +
  # ggtitle("Survival Curves") + 
  ylab("Turnover Probability") +
  xlab("Tenure (Years)") + 
  xlim(0,6) +
  # ylim(.20,1.0)+
  theme(legend.title = element_blank(), text = element_text(size = 12))


######## Compare by State ##########

DT_State<- DT[DT$State%in%c("GA","CA","FL","TX"),]
fit <- survfit(Surv(Tenure, Attrition) ~ State,
               data = DT_State)
# ggsurvplot(fit, data = DT_State, risk.table = TRUE)

g1<-ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = DT_State,  # data used to fit survival curves. 
  risk.table = FALSE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,5),        # present narrower X axis, but not affect
  xlab = "Tenure (Years)",
  title = "Comparison by State",
  # ylab = "Survival Probability",
  # survival estimates.
  break.time.by = 1,     # break X axis in time intervals by 500.
  # ncensor.plot = TRUE,
  # risk.table.y.text.col = T, # colour risk table text annotations.
  # risk.table.y.text = TRUE, # show bars instead of names in text annotations
  # in legend of risk table
  ggtheme = theme_grey()
)


######## Compare by Job Title ##########
DT_JobTitle <- DT[DT$CurrentJobTitle %in% c("SCAFFOLD BUILDER", "ELECTRICIAN", "MECHANIC"),]
colnames(DT_JobTitle)[which(names(DT_JobTitle) == "CurrentJobTitle")] <- "Role"
fit <- survfit(Surv(Tenure, Attrition) ~ Role,
               data = DT_JobTitle)
# ggsurvplot(fit, data = DT_State, risk.table = TRUE)

g2<-ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = DT_JobTitle,  # data used to fit survival curves. 
  risk.table = FALSE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,5),        # present narrower X axis, but not affect
  xlab = "Tenure (Years)",
  # ylab = "Survival Probability",
  title = "Comparison by Job Role",
  # survival estimates.
  break.time.by = 1,     # break X axis in time intervals by 500.
  # ncensor.plot = TRUE,
  # risk.table.y.text.col = T, # colour risk table text annotations.
  # risk.table.y.text = TRUE, # show bars instead of names in text annotations
  # in legend of risk table
  ggtheme = theme_grey()
)


######## Compare by Job Level ##########
DT$Level<- NA
DT$Level[DT$JobLevel==1]<- "Internship"
DT$Level[DT$JobLevel==2]<- "Entry Level"
DT$Level[DT$JobLevel==3]<- "Experienced"
DT$Level[DT$JobLevel==4]<- "Manager"
DT$Level[DT$JobLevel==5]<- "Executive"
DT$Level<-as.factor(DT$JobLvl)


fit <- survfit(Surv(Tenure, Attrition) ~ Level,
               data = DT)
# ggsurvplot(fit, data = DT_State, risk.table = TRUE)

g3<-ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  data = DT,  # data used to fit survival curves. 
  risk.table = FALSE,       # show risk table.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  xlim = c(0,5),        # present narrower X axis, but not affect
  xlab = "Tenure (Years)",
  title = "Comparison by Job Level",
  # ylab = "Survival Probability",
  # survival estimates.
  break.time.by = 1,     # break X axis in time intervals by 500.
  # ncensor.plot = TRUE,
  # risk.table.y.text.col = T, # colour risk table text annotations.
  # risk.table.y.text = TRUE, # show bars instead of names in text annotations
  # in legend of risk table
  ggtheme = theme_grey()
)


splots <- list()

splots[[1]]<-g1
splots[[2]]<-g2
splots[[3]]<-g3
arrange_ggsurvplots(splots, print = TRUE, surv.plot.height = 0.5,
                    ncol = 2, nrow = 2)
g1
g2
g3



# ######## Compare by Employment Year ##########
# DT_EmploymentYear <- DT[DT$EmploymentYear %in% c(2012, 2013, 2014)]
# colnames(DT_EmploymentYear)[which(names(DT_EmploymentYear) == "EmploymentYear")] <- "StartYear"
# 
# fit <- survfit(Surv(Tenure, Attrition) ~ StartYear,
#                data = DT_EmploymentYear)
# # ggsurvplot(fit, data = DT_State, risk.table = TRUE)
# 
# ggsurvplot(
#   fit,                     # survfit object with calculated statistics.
#   data = DT_EmploymentYear,  # data used to fit survival curves. 
#   risk.table = TRUE,       # show risk table.
#   pval = TRUE,             # show p-value of log-rank test.
#   conf.int = TRUE,         # show confidence intervals for 
#   # point estimaes of survival curves.
#   xlim = c(0,5),        # present narrower X axis, but not affect
#   xlab = "Tenure (Years)",
#   ylab = "Survival Probability",
#   # survival estimates.
#   break.time.by = 1,     # break X axis in time intervals by 500.
#   # ncensor.plot = TRUE,
#   # risk.table.y.text.col = T, # colour risk table text annotations.
#   # risk.table.y.text = TRUE, # show bars instead of names in text annotations
#   # in legend of risk table
#   ggtheme = theme_grey()
# )
