######################################################################################################
#
# Script with BLINDED analyses to reconstruct the analyses reported in the Experiment 1 
# in the Manuscript "Infants’ preference for social interactions increases from 7 to 13 months of age"
#
# April 2021 
#
######################################### TABLE OF CONTENTS ##########################################
#
# (PART 1) EYE TRACKING
# (PART 2) FREE PLAY
# (PART 3) CORRELATION BETWEEN FREEPLAY & EYE TRACKING
# (PART 4) PLOTTING
#
######################################################################################################

# Clear workspace.
rm(list = ls(all = TRUE)) 
graphics.off()            

# Set the working directory
setwd("")

#load packages
library("sciplot")
library("exactRankTests")
library("ggpubr")
library("psych")
library("lsr")
library("lme4")
library("plyr")
library("ggplot2")
library("pwr")
library("jtools")

# read data
Data <- read.table(file = 'Data_Freeplay&EyeTracking.csv', header = TRUE, dec = ".", sep = ";") # Eye tracking and free play data from N=40 children who provided data in both measures
AdditionalData <- read.table(file = 'Data_EyeTrackingTrialwise.txt', header = TRUE, dec = ".", sep = "") # Only Eye tracking data with performance on individual trials (12 trials) for each child.
First2sec <- read.table(file = 'Data_EyeTracking_first2.txt', header = TRUE, dec = ".", sep = "") # Only Eye tracking data pre-processed for only the first 2 seconds of the video presentation (while actors facing forward)
Last10sec <- read.table(file = 'Data_EyeTracking_last10.txt', header = TRUE, dec = ".", sep = "") # Only Eye tracking data pre-processed for only the last 10 seconds of the video presentation (after actors had started interacting/not interacting)
OnlyFreeplay <- read.table(file = 'Data_Freeplay.csv', header = TRUE, dec = ",", sep = ";")  # Only Freeplay data for all N=47 children who have provided valid data, regardless of whether or not they provided eye tracking data
Data_Pref <- read.table(file = 'Data_EyeTracking_without1or0.txt', header = TRUE, dec = ".", sep = "") # Pre-processed eye tracking data after excluding trials during which children had looked at only one of the two videos during the preferential looking trials.

# create subgroups for age groups
youngerData <- subset(Data, Data$AgeGroup=="1") # "younger" sample: 7-8.5 months
olderData <- subset(Data, Data$AgeGroup=="2") # "older" sample: 9.5-11 months
# create subgroups for age groups (for additional analyses trial exclusion)
youngerData_pref <- subset(Data_Pref, Data_Pref$AgeGroup=="1") # "younger" sample: 7-8.5 months
olderData_pref <- subset(Data_Pref, Data_Pref$AgeGroup=="2") # "older" sample: 9.5-11 months
# create subgroups for age groups (for additional analyses first 2 sec)
youngerData_first2 <- subset(First2sec, First2sec$AgeGroup=="1") # "younger" sample: 7-8.5 months
olderData_first2 <- subset(First2sec, First2sec$AgeGroup=="2") # "older" sample: 9.5-11 months
# create subgroups for age groups (for additional analyses last 10 sec)
youngerData_last10 <- subset(Last10sec, Last10sec$AgeGroup=="1") # "younger" sample: 7-8.5 months
olderData_last10 <- subset(Last10sec, Last10sec$AgeGroup=="2") # "older" sample: 9.5-11 months
# create subgroups for age groups (for free play)
youngerData2 <- subset(OnlyFreeplay, OnlyFreeplay$Age=="1") # "younger" sample: 7-8.5 months
olderData2 <- subset(OnlyFreeplay, OnlyFreeplay$Age=="2") # "older" sample: 9.5-11 months

###########################################################################
# Sociodemographics
###########################################################################

# younger sample
min(youngerData$AgeMonthsDays) # 7 months, 2 days 
max(youngerData$AgeMonthsDays) # 8 months, 14 days
mean(youngerData$AgeDays) # mean age 240.4 days
sd(youngerData$AgeDays) # SD age 13.24 days
table(youngerData$Sex) # 10 M, 10 F
mean(Data$PregnancyWeek) # 40.53 weeks
sd(Data$PregnancyWeek) # 1.26 weeks
table(Data$ParentFreeplay) # 5 fathers and 35 mothers participated in the free play phase of the study
table(youngerData$ParentFreeplay) # 2 fathers, 18 mothers in younger sample

# older sample
min(olderData$AgeMonthsDays) # 9 months, 15 days 
max(olderData$AgeMonthsDays) # 10 months, 25 days
mean(olderData$AgeDays) # mean age 313.6 days
sd(olderData$AgeDays) # SD age 11.64 days

###########################################################################
# PART 1: Eye tracking
###########################################################################

# Checking assumptions
hist(Data$RelFixSocialMean)
qqnorm(Data$RelFixSocialMean)
qqline(Data$RelFixSocialMean)
shapiro.test(Data$RelFixSocialMean)

hist(youngerData$RelFixSocialMean)
qqnorm(youngerData$RelFixSocialMean)
qqline(youngerData$RelFixSocialMean)
shapiro.test(youngerData$RelFixSocialMean)

hist(olderData$RelFixSocialMean)
qqnorm(olderData$RelFixSocialMean)
qqline(olderData$RelFixSocialMean)
shapiro.test(olderData$RelFixSocialMean)

###########################################################################
# (A) Eye tracking MAIN ANALYSES 
###########################################################################

# Descriptive Stats: Mean and SD of Relative Looking time at social AOI (Averaged over all valid trials)
mean(olderData$RelFixSocialMean) # [1] 0.5385405
sd(olderData$RelFixSocialMean) # [1] 0.07235637
mean(youngerData$RelFixSocialMean) # [1] 0.4707975
sd(youngerData$RelFixSocialMean) # [1] 0.08370386

# (A1) One sample t-tests against chance for both age groups
t.test(olderData$RelFixSocialMean, mu=0.50, alternative="two.sided") # older sample: t(19) = 2.38, p = .03, d = 0.53
d.onesample.olderData <- (mean(olderData$RelFixSocialMean) - 0.50) / sd(olderData$RelFixSocialMean) #Effect size cohen's d
d.onesample.olderData
# post-hoc power analysis (reported in Table S3 in the Supplements)
pwr.p.test(h = .53, n = 20, sig.level = 0.05, power = NULL,alternative = "two.sided")

t.test(youngerData$RelFixSocialMean, mu=0.50, alternative="two.sided") # younger sample: t(19) = –1.56, p = .13, d = 0.35
d.onesample.youngerData <- (mean(youngerData$RelFixSocialMean) - 0.50) / sd(youngerData$RelFixSocialMean) #Effect size cohen's d
d.onesample.youngerData
# post-hoc power analysis (reported in Table S3 in the Supplements)
pwr.p.test(h = .35, n = 20, sig.level = 0.05, power = NULL,alternative = "two.sided")

# (A2) Comparison between Age Groups: 
## "We found no effects for gender, neither as main effect (F(1,36) = 0.05, p = .83) nor in interaction with age group (F(1,36) = 2.40, p = .13), 
## and thus excluded gender from the following analyses."  
anova1 <- aov(Data$RelFixSocialMean ~ Data$AgeGroup + Data$Sex + Data$AgeGroup*Data$Sex)
summary(anova1)
etaSquared(anova1)

## "The mean proportion of looking time at social stimuli was significantly greater in the older compared to the younger sample (F(1,38) = 7.50, p = .009, η² = .16)."
anova2 <- aov(Data$RelFixSocialMean ~ Data$AgeGroup)
summary(anova2)
etaSquared(anova2)
# post-hoc power analysis (reported in Table S3 in the Supplements)
pwr.f2.test(u = 1, v = 38, f2 = .16, sig.level = 0.05, power = NULL)

# (A3) Exclusion 
## "We repeated our main analysis after excluding trials in which infants exclusively looked at one stimulus, revealing the same pattern with even stronger 
## effects (older sample: M = .57, SD = .06, t(19) = 5.32, p < .001, d = 1.20; younger sample: M = .47, SD = .07, t(19) = –1.63, p = .12, d = 0.36; 
## difference between age groups: F(1,38) = 21.11, p < .001, η² = .36)."

# Descriptive Stats: Mean and SD of Relative Looking time at social AOI (Averaged over all valid trials)
mean(olderData_pref$RelFixSocialMean) # [1] 0.5685598
sd(olderData_pref$RelFixSocialMean) # [1] 0.5685598
mean(youngerData_pref$RelFixSocialMean) # [1] 0.4737324
sd(youngerData_pref$RelFixSocialMean) # [1] 0.07209258

t.test(olderData_pref$RelFixSocialMean, mu=0.50, alternative="two.sided") 
d.onesample.olderData <- (mean(olderData_pref$RelFixSocialMean) - 0.50) / sd(olderData_pref$RelFixSocialMean) #Effect size cohen's d
d.onesample.olderData

t.test(youngerData_pref$RelFixSocialMean, mu=0.50, alternative="two.sided") 
d.onesample.youngerData <- (mean(youngerData_pref$RelFixSocialMean) - 0.50) / sd(youngerData_pref$RelFixSocialMean) #Effect size cohen's d
d.onesample.youngerData

# "difference between age groups: F(1,38) = 21.11, p < .001, η² = .36"
anova3 <- aov(Data_Pref$RelFixSocialMean ~ Data_Pref$AgeGroup)
summary(anova3)
etaSquared(anova3, anova = FALSE)

# The average number of trials discarded in this way per infant was 1.4 (SD = 1.79, total = 27) 
## for the younger age group and 1.60 (SD = 1.64, total = 32) for the older age group.
min(youngerData$relSocial.oneStimulus.count) # 0
max(youngerData$relSocial.oneStimulus.count) #6
mean(youngerData$relSocial.oneStimulus.count) #1.35
sd(youngerData$relSocial.oneStimulus.count) #1.785173
sum(youngerData$relSocial.oneStimulus.count) #27

min(olderData$relSocial.oneStimulus.count) #0
max(olderData$relSocial.oneStimulus.count) #5
mean(olderData$relSocial.oneStimulus.count) #1.6
sd(olderData$relSocial.oneStimulus.count) #1.63514
sum(olderData$relSocial.oneStimulus.count) #32

# (A4) Trial Effect Overall trials? - reported in main manuscript
## "We did not find any effect of trial on infants’ preference score, neither in interaction with age group 
## (χ2 (1) = 1.12, p = .29, estimate = −0.03, SE = 0.03), nor as overall main effect (χ2 (1) = 2.11, p = .15, 
## estimate = 0.01, SE = 0.01, see supplementary materials for additional analyses). 
contr=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)) 
AdditionalData$z.Trial <- as.vector(scale(AdditionalData$Trial))

res1 = lmer(relSocialFixTime ~ AgeGroup*z.Trial + (1|ID) + (0+z.Trial|ID), data=AdditionalData, control = contr) 
drop1(res1, test="Chisq") 
summary(res1) #Model with interaction

contr=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)) 
res2 = lmer(relSocialFixTime ~ AgeGroup + z.Trial + (1|ID) + (0+z.Trial|ID), data=AdditionalData, control = contr) 
drop1(res2, test="Chisq") #Model with main effects
summary(res2)

###########################################################################
# (B) Eye tracking ADDITIONAL ANALYSES (Supplementary Material)
###########################################################################
# Age as factor
AdditionalData$AgeGroup <- as.factor(AdditionalData$AgeGroup)
# z-standardisierung
Data$z.AgeDays <- as.vector(scale(Data$AgeDays))
# Condition.code
AdditionalData$Stimulus.code <- as.numeric(factor(AdditionalData$Stimulus))

# (B1) Age continuously
##"First, to allow for a more direct comparison with the main analysis of Experiment 2, we conducted a model for the mean proportional looking time to the 
## social interaction stimuli, including age as continuous rather than categorical predictor. The mean proportional looking time increased with age 
## (Beta = .04 +/-  SE = .01, t(1,38) = 2.87, p < .01, η² = .19).
model1 <- lm(RelFixSocialMean ~ z.AgeDays, data = Data) 
summ(model1)
summary(model1)
etaSquared(model1)

# (B2) Effect of Timing within the stimuli? (comparing looking pattern in first 2 seconds with remaining 10 seconds)
## "We did not find any effects of condition during the first two seconds— neither in the older group (M = .52, SD = .11; t(19) = 0.65, p = .52, d = 0.15), 
## nor in the younger group (M = .51, SD = .09; t(19) = 0.76, p = .45, d = 0.17), with no difference between age groups (F(1,38) = 0.004, p = .95, η² = .0001). 

mean(olderData_first2$RelFixSocialMean) # [1] 0.5167081
sd(olderData_first2$RelFixSocialMean) # [1] 0.1144823
mean(youngerData_first2$RelFixSocialMean) # [1]  0.5146241
sd(youngerData_first2$RelFixSocialMean) # [1] 0.08505703

t.test(olderData_first2$RelFixSocialMean, mu=0.50, alternative="two.sided") 
d.onesample.olderData <- (mean(olderData_first2$RelFixSocialMean) - 0.50) / sd(olderData_first2$RelFixSocialMean) #Effect size cohen's d
d.onesample.olderData

t.test(youngerData_first2$RelFixSocialMean, mu=0.50, alternative="two.sided") 
d.onesample.youngerData <- (mean(youngerData_first2$RelFixSocialMean) - 0.50) / sd(youngerData_first2$RelFixSocialMean) #Effect size cohen's d
d.onesample.youngerData

anova3 <- aov(First2sec$RelFixSocialMean ~ First2sec$AgeGroup)
summary(anova3)
etaSquared(anova3, anova = FALSE)

## During the last ten seconds, in contrast, the mean proportional looking time at the social stimuli was significantly greater in the older as compared to the 
## younger sample (F(1,38) = 9.16, p = .004, η² = .19), with a looking preference for the social stimuli in only the older (M = .56, SD = .09; t(19) = 3.103, p = .006, d = 0.69), 
##not the younger sample (M = .47, SD = .10; t(19) = 0.77, p = .45, d = 0.29).

mean(olderData_last10$RelFixSocialMean) # [1] 0.5596642
sd(olderData_last10$RelFixSocialMean) # [1] 0.08596719
mean(youngerData_last10$RelFixSocialMean) # [1] 0.4708186
sd(youngerData_last10$RelFixSocialMean) # [1] 0.0992516

t.test(olderData_last10$RelFixSocialMean, mu=0.50, alternative="two.sided") 
d.onesample.olderData <- (mean(olderData_last10$RelFixSocialMean) - 0.50) / sd(olderData_last10$RelFixSocialMean) #Effect size cohen's d
d.onesample.olderData

t.test(youngerData_first2$RelFixSocialMean, mu=0.50, alternative="two.sided") 
d.onesample.youngerData <- (mean(youngerData_last10$RelFixSocialMean) - 0.50) / sd(youngerData_last10$RelFixSocialMean) #Effect size cohen's d
d.onesample.youngerData

anova3 <- aov(Last10sec$RelFixSocialMean ~ Last10sec$AgeGroup)
summary(anova3)
etaSquared(anova3, anova = FALSE)

# (B3) Overall analysis including both stimulus type as well as kind of interaction - reported in supplemental materials
## Infants’ looking preference did not differ between the different kinds of interactions, neither did it change over trials. We did not find any effect of 
## interaction type or trial, neither in interaction with age group (age × trial: χ2(1) = 1.73, p = .19, estimate = −0.01, SE = 0.009; age × type of interaction: 
## χ2 (1) = 0.63, p = .43, estimate = −0.007, SE = 0.009), nor as overall main effects (trial: χ2 (1) = 0.75, p = .39, estimate = 0.004, SE = 0.005;
## type of interaction: χ2 (1) = 0.61, p = .43, estimate = −0.004, SE = 0.005). In line with our main analysis reported in the main manuscript, the model revealed 
## a significant effect of age group on infants’ looking preference (χ2 (1) = 5.97, p = .01, estimate = 0.07, SE = 0.03). Further in line with our main analysis, 
## gender had no significant effect on infants’ looking preference (χ2(1) = 0.03, p = .86, estimate = −0.005, SE = 0.43). 
contr=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)) 
model1 = lmer(relSocialFixTime ~ Stimulus.code*AgeGroup + Trials*AgeGroup + Sex +(1|ID) + (1|Sex) + (0+Trials|ID) + (0+Stimulus.code|ID) + (0+AgeGroup|ID) + (0+AgeGroup:Stimulus.code|ID) + (0+AgeGroup:Trials|ID), data=AdditionalData, control = contr) 
drop1(model1, test="Chisq")
summary(model1)

model2 = lmer(relSocialFixTime ~ Stimulus.code + AgeGroup + Trials + Sex +(1|ID) + (1|Sex) + (0+Trials|ID) + (0+Stimulus.code|ID) + (0+AgeGroup|ID), data=AdditionalData, control = contr) 
drop1(model2, test="Chisq")
summary(model2)

# (B4) Effect of Stimulus? (Type of interaction) - not longer reported as single analysis in the supplements (instead overall Model described in B3)
## "Infants’ looking preference did not differ between the different kinds of interactions, neither in interaction with age group (χ2(1) = 0.023, p = .89, estimate = −0.001, SE = 0.008), 
## nor as overall main effect (χ2(1) = 1.95, p = .16, estimate = −0.005, SE = 0.004)."

contr=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)) 
res3 = lmer(relSocialFixTime ~ AgeGroup*Stimulus.code + (1|ID) + (0+Stimulus.code|ID), data=AdditionalData, control = contr) 
drop1(res3, test="Chisq") #Model with main effects
summary(res3)

contr=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)) 
res4 = lmer(relSocialFixTime ~ AgeGroup + Stimulus.code + (1|ID) + (0+Stimulus.code|ID), data=AdditionalData, control = contr) 
drop1(res4, test="Chisq")
summary(res4)

###########################################################################
# PART 2: Free Play
###########################################################################

# Checking assumptions
hist(Data$OverallScore)
qqnorm(Data$OverallScore)
qqline(Data$OverallScore)
shapiro.test(Data$OverallScore)

###########################################################################
# (A) Free Play MAIN ANALYSES 
###########################################################################

# Descriptive stats: Means and SDs OverallScore = (behaviors 2+3+4) / (behaviors 1+2+3+4)
mean(youngerData2$OverallScore) #[1] 0.2362366
sd(youngerData2$OverallScore) #[1] 0.1982765
mean(olderData2$OverallScore) #[1] 0.3637029
sd(olderData2$OverallScore) #[1] 0.1832779

# (A) Comparison between Age Groups:
##"We found no effects of gender, neither as main effect (F(1,43) = 1.71, p = .20), nor in interaction with age group (F(1,43) = 1.02, p = .32), 
## and thus dropped gender from the model." 
res5 <- aov(OnlyFreeplay$OverallScore ~ OnlyFreeplay$Age + OnlyFreeplay$Sex + OnlyFreeplay$Age*OnlyFreeplay$Sex)
summary(res5) #Model with interaction
etaSquared(res5, anova = FALSE)

## "Social behavior scores were significantly higher in the older age group (M = .36, SD = .18) compared to the younger age group (M = .24, SD = .20; F(1,45) = 5.06, p = .03, η² = .10, Figure 2b)". 
res6 <- aov(OnlyFreeplay$OverallScore ~ OnlyFreeplay$Age)
summary(res6) #Model with Main effects
etaSquared(res6)
#post-hoc power analysis (reported in Table S3 in the Supplements)
pwr.f2.test(u = 1, v = 45, f2 = .10, sig.level = 0.05, power = NULL)

# (B) Exploratory Man-Whitney U tests for individual behaviors (see Tables in Main Document)
# Note: Bonferroni-corrected alpha = .0125

# Behavior 1
wilcox.exact(youngerData2$behav1, olderData2$behav1, paired=F) 
mean(youngerData2$behav1) 
sd(youngerData2$behav1) 
mean(olderData2$behav1) 
sd(olderData2$behav1) 
count(olderData2$behav1)
count(youngerData2$behav1) 
### Behavior 2
wilcox.exact(youngerData2$behav2, olderData2$behav2, paired=F) 
mean(youngerData2$behav2) 
sd(youngerData2$behav2) 
mean(olderData2$behav2)
sd(olderData2$behav2) 
count(olderData2$behav2)
count(youngerData2$behav2) 
### Behavior 3
wilcox.exact(youngerData2$behav3, olderData2$behav3, paired=F) 
mean(youngerData2$behav3) 
sd(youngerData2$behav3) 
mean(olderData2$behav3) 
sd(olderData2$behav3) 
count(olderData2$behav3) 
count(youngerData2$behav3) 
### Behavior 4
wilcox.exact(youngerData2$behav4, olderData2$behav4, paired=F) 
mean(youngerData2$behav4) 
sd(youngerData2$behav4) 
mean(olderData2$behav4) 
sd(olderData2$behav4) 
count(olderData2$behav4) 
count(youngerData2$behav4) 

###########################################################################
# (B) Free Play ADDITIONAL ANALYSES (Supplementary Material)
###########################################################################

# (B2) Alternative approach to the proportion score of social engagement (sum of raw frequencies of behaviors 2+3+4)
## "Analogous to our findings based on the proportion score, this sum score was significantly higher in the older age group 
## (M = 11.05, SD = 6.78) compared to the younger age group (M = 6.40, SD = .6.28; F(1,45) = 5.87, p = .02, η² = .12, see Figure S4).
mean(olderData2$SummeOhne1) # 11.05
sd(olderData2$SummeOhne1) # 6.778255
mean(youngerData2$SummeOhne1) # 6.407407
sd(youngerData2$SummeOhne1) # 6.778255

anova <- aov(OnlyFreeplay$SummeOhne1 ~ OnlyFreeplay$Age)
summary(anova)
etaSquared(anova, anova = FALSE)

# (B2) Exploring differential behavior scores before and after the first 90 minutes during Free play
## "We did not find difference in infants’ social engagement score before and after 90 seconds. Neither overall infants 
## (before: M = .32, SD = .26; after: M = .26, SD = .21; t(46) = 1.78, p = .08), nor for the separate younger sample 
## (before: M = .28, SD = .28; after: M = .21, SD = .20; t(26) = 1.70, p = .10) or 
## older sample (before: M = .38, SD = .23; after: M = .34, SD = .19; t(19) = 0.76, p = .45)."
mean(OnlyFreeplay$OverallScore_before)
sd(OnlyFreeplay$OverallScore_before)
mean(OnlyFreeplay$OverallScore_after)
sd(OnlyFreeplay$OverallScore_after)
t.test(OnlyFreeplay$OverallScore_before, OnlyFreeplay$OverallScore_after, paired = TRUE, alternative = "two.sided")
d.onesample.Overall <- (mean(OnlyFreeplay$OverallScore_before) - 0.50) / sd(OnlyFreeplay$OverallScore_after) #Effect size cohen's d
d.onesample.Overall
# compare proportion Score before and after in younger sample
mean(youngerData2$OverallScore_before)
sd(youngerData2$OverallScore_before)
mean(youngerData2$OverallScore_after)
sd(youngerData2$OverallScore_after)
t.test(youngerData2$OverallScore_before, youngerData2$OverallScore_after, paired = TRUE, alternative = "two.sided")
# compare proportion Score before and after in older sample
mean(olderData2$OverallScore_before)
sd(olderData2$OverallScore_before)
mean(olderData2$OverallScore_after)
sd(olderData2$OverallScore_after)
t.test(olderData2$OverallScore_before, olderData2$OverallScore_after, paired = TRUE, alternative = "two.sided")

###########################################################################
# PART 3: Correlation between Eye Tracking and Free Play
###########################################################################

# Pearsons R correlation ET Data and Freeplay Data
## "The proportional looking time at social interactions did not correlate with the social behavior scores—neither in the total sample (N = 40; r(38) = .15, p = .36), nor in both age groups separately (younger sample: r(18) = .30, p = .19; older sample: r(18) = –.34, p = .15)."
cor.test(Data$OverallScore, Data$RelFixSocialMean, method = "pearson") #overall
cor.test(youngerData$OverallScore, youngerData$RelFixSocialMean, method = "pearson") #only younger
cor.test(olderData$RelFixSocialMean, olderData$OverallScore, method = "pearson") #only older

###########################################################################
# PART 4: PLOTTING 
## note that plots in the manuscript have been further processed in illustrator
###########################################################################
# Preference for others' interactions (eye tracking)
Data$AgeGroup <- as.character(Data$AgeGroup)

Boxplot_3 <- ggplot(Data, aes(AgeGroup, RelFixSocialMean))
Boxplot_3 + geom_boxplot(fill=c("white", "darkgrey")) + scale_y_continuous(limits=c(0.0,1.0), name="Mean proportion of looking time") + labs(x="Stimulus") + scale_fill_manual(values=c("blue", "darkgrey")) + geom_dotplot(binaxis="y",stackdir="center",stackratio=0.7,dotsize=0.5, fill=NA)+
  theme(axis.title.x = element_text(face="bold", size=10, vjust=-0.4, hjust=0.54),
        axis.text.x  = element_text(size=10,face="bold",vjust=0.7, color = "black"),
        axis.text.y = element_text(size=35),
        axis.title.y = element_text(face="bold", size=10, angle=90, vjust=5.0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
  ) + geom_hline(aes(yintercept=0.5), linetype="dotted", size = 1)

# Social Engagement Score
Data$AgeGroup <- as.character(Data$AgeGroup)

Boxplot_3 <- ggplot(Data, aes(AgeGroup, OverallScore))
Boxplot_3 + geom_boxplot(fill=c("white", "darkgrey")) + scale_y_continuous(limits=c(0.0,1.0), name="Mean prop. social engagement") + labs(x="Stimulus") + scale_fill_manual(values=c("blue", "darkgrey")) + geom_dotplot(binaxis="y",stackdir="center",stackratio=0.7,dotsize=0.5, fill=NA)+
  theme(axis.title.x = element_text(face="bold", size=10, vjust=-0.4, hjust=0.54), 
        axis.text.x  = element_text(size=10,face="bold",vjust=0.7, color = "black"),
        axis.text.y = element_text(size=35),
        axis.title.y = element_text(face="bold", size=10, angle=90, vjust=5.0), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()
  ) 

