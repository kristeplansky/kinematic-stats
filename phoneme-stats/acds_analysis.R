library(ggpubr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(caret)
library(dplyr)
library(jtools)
library(ggplot2)
library(broom)
library(AICcmodavg)
library(lsr)
library(gplots)


# Select data --------------

setwd("C:/Users/krist/Box/WangLab/Journals/JSLHR2022 - articulatory consonant space/results")
my_data <- read.csv(file.choose(), header = TRUE)

# Preparing the data --------------
my_data_als <- my_data[my_data$group == "ALS", ] # subset data
# my_data_control <- my_data[my_data$group == "Control", ] # subset data

sample_n(my_data_als, 5) # inspect data

# Visualize data --------------
p1 <- ggscatter(my_data_als, x = "area", y = "ISR",
          shape = 1,
          size = 4,
          add = "reg.line", 
          conf.int = TRUE,
          add.params = list(color = "black", fill = "lightgray", size = 1), # customize regression line
          cor.coef = TRUE,
          cor.coeff.args = list(label.x = .01, label.y = 240),
          cor.method = "pearson",
          xlab = "ACDS Area", ylab = "Intelligible Speaking Rate (IWPM)")

p1 +
  font("ylab", size = 14, family = "serif")+
  font("xlab", size = 14, family = "serif")+
  ylim(0, 250)+
  # stat_cor(method = "pearson", label.x = 0.01, label.y = 240)+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 14, angle = 0),
        axis.text.y = element_text(family = "serif", color = "black", size = 14, angle = 0))


p2 <- ggscatter(my_data_als, x = "volume", y = "ISR",
                shape = 2,
                size = 4,
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "black", fill = "lightgray", size = 1), # customize regression line
                cor.coef = TRUE,
                cor.coeff.args = list(label.x = .001, label.y = 240),
                cor.method = "pearson",
                xlab = "ACDS Volume", ylab = "Intelligible Speaking Rate (IWPM)")



p2 +
  font("ylab", size = 14, family = "serif")+
  font("xlab", size = 14, family = "serif")+
  ylim(0, 250)+
  # stat_cor(method = "pearson", label.x = 0.001, label.y = 250)+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 14, angle = 0),
        axis.text.y = element_text(family = "serif", color = "black", size = 14, angle = 0))

p3 <- ggscatter(my_data_als, x = "area", y = "bulbar.subscore",
                 shape = 1,
                 size = 4,
                 add = "reg.line", 
                 conf.int = TRUE,
                 add.params = list(color = "black", fill = "lightgray", size = 1), # customize regression line
                 cor.coef = TRUE,
                 cor.coeff.args = list(label.x = .01, label.y = 15),
                 cor.method = "pearson",
                 xlab = "ACDS Area", ylab = "ALSFRS-R Bulbar Subscore")

p3 +
  font("ylab", size = 14, family = "serif")+
  font("xlab", size = 14, family = "serif")+
  ylim(0, 15)+
  # stat_cor(method = "pearson", label.x = 0.01, label.y = 240)+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 14, angle = 0),
        axis.text.y = element_text(family = "serif", color = "black", size = 14, angle = 0))

p4 <- ggscatter(my_data_als, x = "volume", y = "bulbar.subscore",
                shape = 2,
                size = 4,
                add = "reg.line", 
                conf.int = TRUE,
                add.params = list(color = "black", fill = "lightgray", size = 1), # customize regression line
                cor.coef = TRUE,
                cor.coeff.args = list(label.x = .001, label.y = 15),
                cor.method = "pearson",
                xlab = "ACDS Volume", ylab = "ALSFRS-R Bulbar Subscore")

p4 +
  font("ylab", size = 14, family = "serif")+
  font("xlab", size = 14, family = "serif")+
  xlim(0, .004)+
  # stat_cor(method = "pearson", label.x = 0.001, label.y = 250)+
  theme(axis.text.x = element_text(family = "serif", color = "black", size = 14, angle = 0),
        axis.text.y = element_text(family = "serif", color = "black", size = 14, angle = 0))


# Pearson's Correlation results --------------

res1 <- cor.test(my_data_als$area, my_data_als$ISR, 
                 method = "pearson")
res1

res_vol <- cor.test(my_data_als$volume, my_data_als$ISR, 
                 method = "pearson")
res_vol

res_area_bulbar <- cor.test(my_data_als$area, my_data_als$bulbar.subscore, 
                    method = "pearson")
res_area_bulbar

res_volume_bulbar <- cor.test(my_data_als$volume, my_data_als$bulbar.subscore, 
                            method = "pearson")
res_volume_bulbar


res2 <- cor.test(my_data$area, my_data$svm_c, 
                 method = "pearson")
res2

res2_als <- cor.test(my_data_als$area, my_data_als$svm_c, 
                 method = "pearson")
res2_als

cohensD(my_data_als$area, my_data_als$svm_c) # Cohen's D, pooled SD.

res3 <- cor.test(my_data$area, my_data$pm_c, 
                     method = "pearson")
res3

res3_als <- cor.test(my_data_als$area, my_data_als$pm_c, 
                 method = "pearson")
res3_als
cohensD(my_data_als$area, my_data_als$pm_c) # Cohen's D, pooled SD.

res4 <- cor.test(my_data$volume, my_data$svm_c, 
                 method = "pearson")
res4

res4_als <- cor.test(my_data_als$volume, my_data_als$svm_c, 
                 method = "pearson")
res4_als

cohensD(my_data_als$volume, my_data_als$svm_c) # Cohen's D, pooled SD.

res5 <- cor.test(my_data$volume, my_data$pm_c, 
                 method = "pearson")
res5

res5_als <- cor.test(my_data_als$volume, my_data_als$pm_c, 
                 method = "pearson")
res5_als
cohensD(my_data_als$volume, my_data_als$pm_c) # Cohen's D, pooled SD.

# Regression models --------------

model1 <- lm(area ~ speaking.rate + speech.intelligibility, data = my_data_als)
summary(model1)
summ(model1)

model2 <- lm(area ~ speaking.rate*speech.intelligibility, data = my_data_als)
summary(model2)
summ(model2)

model3 <- lm(area ~ speech.intelligibility, data = my_data_als)
summary(model3)

model4 <- lm(area ~ ISR, data = my_data_als)
summary(model4)


# ANOVA model --------------

group.data <- read.csv("C:/Users/krist/Box/WangLab/Journals/JSLHR2022 - articulatory consonant space/results/acds_data_crosschecked-anova.csv", header = TRUE, colClasses = c("factor", "numeric", "numeric", "numeric", "numeric"))
summary(group.data)

# ACDS Area (2D)
log_area <- log10(group.data$area)
one.way1 <- aov(log_area ~ severity.group, data = group.data)
summary(one.way1)
TukeyHSD(one.way1)
etaSquared(one.way1)# Calculate Effect Size (partial eta squared)
plot(TukeyHSD(one.way1, conf.level=.95), las = 2)

ggplot(one.way1, aes(severity.group, log_area))+
  geom_boxplot(aes(col = severity.group)) +
  labs(title="Boxplot Speaker Groups")

plotmeans(log_area ~ severity.group, main = "Fig.1: Mean Plot with 95% Confidence Interval", ylab = "SVM Classification Accuracy", xlab = "Speaker Group")

# ACDS Volume (3D)
log_volume <- log10(group.data$volume)
one.way2 <- aov(log_volume ~ severity.group, data = group.data)
summary(one.way2)
TukeyHSD(one.way2)
etaSquared(one.way2)# Calculate Effect Size (partial eta squared)
plot(TukeyHSD(one.way2, conf.level=.95), las = 2)

# SVM Classification Accuracy
one.way3 <- aov(svm_c ~ severity.group, data = group.data)
summary(one.way3)
TukeyHSD(one.way3)
etaSquared(one.way3)# Calculate Effect Size (partial eta squared)
plot(TukeyHSD(one.way3, conf.level=.95), las = 2)

# PM Classification Accuracy
one.way4 <- aov(pm_c ~ severity.group, data = group.data)
summary(one.way4)
TukeyHSD(one.way4)
etaSquared(one.way4)# Calculate Effect Size (partial eta squared)
plot(TukeyHSD(one.way4, conf.level=.95), las = 2)

# Create APA Tables --------------
library(apaTables)

group.data_n <- cbind(group.data, log_area, log_volume)

# Descriptive statistics for area as a function of severity group
apa.1way.table(iv = severity.group, dv = area, data = group.data_n)

# Visualize data  --------------

# area <- group.data_n %>% select(severity.group, area)
area_group.data <- read.csv("C:/Users/krist/Box/WangLab/Journals/JSLHR2022 - articulatory consonant space/results/acds_group_area.csv", header = TRUE, colClasses = c("factor", "numeric", "numeric", "numeric", "numeric"))
summary(area_group.data)

area_group.data$Group <- factor(area_group.data$Group, levels=c("Healthy", "Asymptomatic", "Mild-Moderate", "Severe"))

p_area <- ggplot(area_group.data) +
  aes(x = Group, y = Area, color = Group)+
  geom_boxplot()+
  geom_jitter(width = .1) +
  theme_classic()+
  font("ylab", size = 18, family = "serif")+
  font("xlab", size = 18, family = "serif")+
  theme(legend.position = "none", axis.text.y = element_text(family = "serif", color = "black", size = 14, angle = 0),
        axis.text.x = element_text(family = "serif", color = "black", size = 14, angle = 0))

# Volume and Log(Volume)
volume_group.data <- read.csv("C:/Users/krist/Box/WangLab/Journals/JSLHR2022 - articulatory consonant space/results/acds_group_volume.csv", header = TRUE, colClasses = c("factor", "numeric", "numeric", "numeric", "numeric"))
summary(volume_group.data)

volume_group.data$Group <- factor(volume_group.data$Group, levels=c("Healthy", "Asymptomatic", "Mild-Moderate", "Severe"))

ggplot(volume_group.data) +
  aes(x = Group, y = Volume, color = Group)+
  geom_boxplot()+
  geom_jitter(width = .1) +
  theme_classic()+
  font("ylab", size = 18, family = "serif")+
  font("xlab", size = 18, family = "serif")+
  theme(legend.position = "none", axis.text.y = element_text(family = "serif", color = "black", size = 14, angle = 0),
        axis.text.x = element_text(family = "serif", color = "black", size = 14, angle = 0))
