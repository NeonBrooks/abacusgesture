# Analyses for US Audio Data
# SAME AS INDIA BUT FOR US KIDS
rm(list=ls())
library(plotrix)
library(lme4)

##READ IN DATA
setwd("~/Desktop/Abacus_gesture/*first_paper/abacus_gesture_analyses/Study2/novices/finaldata") # SET WORKING DIRECTORY TO FOLDER CONTAINING FINAL DATA (OUTPUTTED FROM PREPROCESSING SCRIPT)
data.bysub <- read.csv("MAstudy2_novices_bysub.csv")
data.bytrial <- read.csv("MAstudy2_novices_bytrial.csv")

##reformat factors
# structure condition levels so that baseline is the reference level and the others apper in a sensible order on graphs
data.bysub$condition <- relevel(data.bysub$condition, "stillhands")
data.bysub$condition <- relevel(data.bysub$condition, "blindfold")
data.bysub$condition <- relevel(data.bysub$condition, "baseline")
names(data.bysub$condition) <- c("Baseline", "Blindfold","Sit-on-Hands", "Motor Interference")
# make subnum a factor
data.bysub$subnum <- factor(data.bysub$subnum)

##GRAPH DATA (graphs 1 and 2 not included in paper)
# Make bar graph showing performance on each task, with error bars
# 1: by addend size
threshBar <- tapply(data.bysub$max, data.bysub$condition, FUN=mean, na.rm=T)
ses <- tapply(data.bysub$max, data.bysub$condition, FUN=std.error)
plot <- barplot(threshBar, ylim=c(0,25), main=c("Control Subjects: Mean Addend Size by Condition"), ylab=c("Maximum Addend Size"))
plotCI(plot, threshBar, ses, add=T)

#2: by threshold
threshBar <- tapply(data.bysub$threshold, data.bysub$condition, FUN=mean, na.rm=T)
ses <- tapply(data.bysub$threshold, data.bysub$condition, FUN=std.error)
plot <- barplot(threshBar, ylim=c(0,25), main=c("Control Subjects: Mean Addend Size by Condition"), ylab=c("Maximum Addend Size"))
plotCI(plot, threshBar, ses, add=T)

#3: overall threshold graph
par(cex=1.5, lwd=2)
means <- tapply(data.bytrial$threshold, list(data.bytrial $condition, data.bytrial $trial),mean,na.rm=T)
lengths <- tapply(data.bytrial $threshold, list(data.bytrial $condition, data.bytrial $trial),length)
plot(means[1,1:20],type="l",col="blue", xlab=c("Trial"), ylab=c("Level"),main=c("Control Subjects:\nAverage Performance Over 20 Trials"), lty=1, ylim=c(2,18))
legend("topleft",c("Baseline","Blindfold", "No Hands","Motor Interference"),lty=c(1,1,1,1), col=c("blue","green","orange","red"))
points(means[2,1:20],type="l", col="green", lty=2)
points(means[4,1:20],type="l", col="orange", lty=3)
points(means[3,1:20],type="l", col="red", lty=4)


## ANALYSES: CONDUCT MIXED EFFECTS REGRESSION MODEL OF PERFORMANCE
# For analyses, consider all data with random intercept & slopes for subject
lm <- lmer(threshold ~ condition+ (1+ condition|subnum), data=data.bytrial)
# TO GET SIMPLE EFFECTS: USE t-as-z heuristic
 2*(1-pnorm(abs((fixef(lm)/sqrt(diag(vcov(lm)))))))

# check about other factors to possibly include in model
lm.1 <- lmer(threshold ~ condition + age + (1  +condition |subnum) + (1+condition|age), data=data.bytrial) #t=0.73
pval <- 2*(1-pnorm(abs((fixef(lm.1)/sqrt(diag(vcov(lm.1))))))) #p=.47, ns
lm.1 <- lmer(threshold ~ condition + gender + (1+condition|subnum) + (1+condition|gender), data=data.bytrial) #t=.09
pval <- 2*(1-pnorm(abs((fixef(lm.1)/sqrt(diag(vcov(lm.1))))))) #p=.93, ns
lm.1 <- lmer(threshold ~ condition + order + (1+condition|subnum)+(1+condition|order), data=data.bytrial) #t=0.72
pval <- 2*(1-pnorm(abs((fixef(lm.1)/sqrt(diag(vcov(lm.1))))))) #p=.47, ns
