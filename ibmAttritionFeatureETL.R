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
require(caret)
# install.packages("caret")

suppressMessages(library(plyr))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(gbm))
suppressMessages(library(survival))
suppressMessages(library(pROC))
# suppressMessages(library(DMwR))
suppressMessages(library(scales))

rm(list = ls())
graphics.off()
setwd("/Users/phhoang/Edge/PropensityToLeave/PostHirePTL")
ibm <- read_tsv("./data/WA_Fn-UseC_-HR-Employee-Attrition.tsv")

g1 <- ggplot(ibm, 
             aes(x = MonthlyIncome, fill = Attrition)) + 
  geom_density(alpha = 0.7) 
# +    scale_fill_manual(values = c("#386cb0","#fdb462"))

g2 <- ggplot(ibm, 
             aes(x = HourlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

g3 <- ggplot(ibm, 
             aes(x = DailyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

g4 <- ggplot(ibm, 
             aes(x = MonthlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#386cb0","#fdb462"))

grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

ggplot(ibm, 
       aes(y = YearsSinceLastPromotion, x = YearsAtCompany, colour = OverTime)) + 
  geom_jitter(size = 1, alpha = 0.7) + 
  geom_smooth(method = "gam") + 
  facet_wrap(~ Attrition) + 
  ggtitle("Attrition") + 
  scale_colour_manual(values = c("#386cb0","#fdb462")) + 
  theme(plot.title = element_text(hjust = 0.5))

g1a<-ggplot(ibm, 
       aes(x = OverTime, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "OverTime") +
  facet_grid(~Attrition) +
  scale_fill_manual(values = c("#386cb0","#fdb462")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

g1b<- ggplot(ibm, 
       aes(x= WorkLifeBalance, y=DistanceFromHome, group = WorkLifeBalance, fill = WorkLifeBalance)) + 
  geom_boxplot(alpha=0.7) + 
  theme(legend.position="none") + 
  facet_wrap(~ Attrition) + 
  ggtitle("Attrition") + 
  theme(plot.title = element_text(hjust = 0.5))

g1c<-ggplot(ibm, 
       aes(x= BusinessTravel,  group=Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill="Business Travel") +
  facet_grid(~Attrition) +
  scale_y_continuous(labels=percent) + 
  scale_fill_manual(values = c("#386cb0","#ef3b2c", "#fdb462")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

grid.arrange(g1, g1a, g1b, g1c, ncol = 2, nrow = 2)
