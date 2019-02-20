library(survival)
library(dplyr)
# install.packages("OIsurv")
library(OIsurv) # Aumatically loads KMsurv
# install.packages("ranger")
library(ranger)
library(ggplot2)
library(KMsurv)
rm(list = ls())
setwd("/Users/phhoang/Edge/PropensityToLeave/PostHirePTL")


#------------
data(bmt)
sapply(bmt,class)

load("WorkterraEmployeeAnonymousData_clean.Rda")
sapply(DT, class)

DT2 <- select(DT, -c(2,3,4,6,9,10))
sapply(DT2, class)
DT3 <- mutate(DT2,
              ClientName = as.factor(ClientName),
              City = as.factor(City),
              State = as.factor(State),
              Age = as.double(Age),
              Tenure = as.double(Tenure),
              Attrition = as.factor(Attrition))
head(DT3)


# Prepare new data frame for modeling
bmt2 <- select(bmt,-c(t2,d2,d3))
bmt2  <- mutate(bmt2,
                group = as.factor(group),
                d1 = as.factor(d1),
                da = as.factor(da),
                dc = as.factor(dc),
                dp = as.factor(dp),
                z3 = as.factor(z3),
                z4 = as.factor(z4),
                z5 = as.factor(z5),
                z6 = as.factor(z6),
                z8 = as.factor(z8),
                z9 = as.factor(z9),
                z10 = as.factor(z10)
)
head(bmt2)

# Kaplan Meier Survival Curve
y_bmt <- Surv(bmt$t1, bmt$d1)
y_bmt

fit1_bmt <- survfit(y_bmt ~ 1)
summary(fit1_bmt)



cb <- confBands(y_bmt, type = "hall")
plot(fit1_bmt,
     main = 'Kaplan–Meier Plot with confidence bands')
lines(cb, col = "red",lty = 3)
legend(1000, 0.99, legend = c('Kaplan–Meier survival estimate',
                              'pointwise intervals', 'Hall-Werner conf bands'), lty = 1:3)


# Fit Cox Model
form <- formula(y_bmt ~ group + ta + tc + tp + dc + dp + 
                  z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z10)

cox_bmt <- coxph(form,data = bmt2)
summary(cox_bmt)


cox_fit_bmt <- survfit(cox_bmt)
plot(cox_fit_bmt)

# ranger model

# ranger model
r_fit_bmt <- ranger(form,
                    data = bmt2,
                    importance = "permutation",
                    seed = 1234)

# Average the survival models
death_times <- r_fit_bmt$unique.death.times
surv_prob <- data.frame(r_fit_bmt$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit_bmt$unique.death.times,r_fit_bmt$survival[1,], type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "death times",
     ylab = "survival",
     main = "Patient Survival Curves")

for(n in c(2:137)){
  lines(r_fit_bmt$unique.death.times, r_fit_bmt$survival[n,], type = "l", col = "red")
}
lines(death_times, avg_prob, lwd = 2)
legend(100, 0.2, legend = c('Averages - black'))


vi <- data.frame(sort(round(r_fit_bmt$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

cat("Prediction Error = 1 - Harrell's c-index = ", r_fit_bmt$prediction.error)


# Set up for ggplot
km <- rep("KM", length(fit1_bmt$time))
km_df <- data.frame(fit1_bmt$time,fit1_bmt$surv,km)
names(km_df) <- c("Time","Surv","Model")

cox <- rep("Cox",length(cox_fit_bmt$time))
cox_df <- data.frame(cox_fit_bmt$time,cox_fit_bmt$surv,cox)
names(cox_df) <- c("Time","Surv","Model")

rf <- rep("RF",length(r_fit_bmt$unique.death.times))
rf_df <- data.frame(r_fit_bmt$unique.death.times,avg_prob,rf)
names(rf_df) <- c("Time","Surv","Model")
plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line() + ggtitle("Comparison of Survival Curves") 
