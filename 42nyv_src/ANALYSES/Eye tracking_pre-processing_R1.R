##############################################################################################################
#
# R-Script for pre-processing the Eye Tracking Data in “recs.new” folder. 
# Creates the tables for statistical analyses (output tables and Script for analyses are provided on the OSF)
#
# April 2021 
#
#############################################################################################################

# Clear workspace.
rm(list = ls(all = TRUE)) # clears everything
graphics.off()            # close all open graphics

# Set the working directory
setwd("")

# Set directories
recs.dir = "./recs.new/" 
plots.dir = "./plots/"

##load packages
#install.packages("sciplot")
#install.packages("exactRankTests")
#install.packages("ggpubr")
#install.packages("psych")
#install.packages("lsr")
library("sciplot")
library("exactRankTests")
library("ggpubr")
library("psych")
library("lsr")
library("lme4")
#install.packages("devtools")
#library("devtools")
#install.packages("lmerTest")
#library("lmerTest")

# reads all txt files in recs folder (40 textfiles, changed complete File name of ID 255394 and 257211)
flist = list.files(recs.dir)

###########################################################################
# preparing data for analysis
###########################################################################

# create empty final data frame "summaryDF" ###, "relSocial.oneStimulus.count"
summaryDF = data.frame(matrix(vector(), 0, 10, dimnames=list(c(), c("ID", "AgeGroup", "Sex", "RelFixSocialMean", "RelFixControlMean", "RelFixCountSocialMean", "RelFixCountControlMean", "RelFixTimeDiff", "RelSocialTrials", "TrialsNoData.count"))), stringsAsFactors=F)
# summaryDF = data.frame(matrix(vector(), 0, 11, dimnames=list(c(), c("ID", "AgeGroup", "Sex", "RelFixSocialMean", "RelFixControlMean", "RelFixCountSocialMean", "RelFixCountControlMean", "RelFixTimeDiff", "RelSocialTrials", "TrialsNoData.count", "relSocial.oneStimulus.count"))), stringsAsFactors=F)

# create empty data frame for the review part
trialSummaryDF = data.frame(matrix(vector(), 0, 9, dimnames=list(c(), c("Stimulus", "AOI.Name", "Fixation.Time..ms.", "relSocialFixTime", "ParentIndex", "AgeGroup", "Sex", "ID", "Trials"))), stringsAsFactors=F)

# create empty data frames for plotting
Plot.data.social = data.frame(matrix(vector(), 0, 5, dimnames=list(c(), c("ID", "AgeGroup", "Sex", "RelFixTimeSocialMean", "RelFixCountSocialMean"))), stringsAsFactors=F)
Plot.data.control = data.frame(matrix(vector(), 0, 5, dimnames=list(c(), c("ID", "AgeGroup", "Sex", "RelFixTimeControlMean", "RelFixCountControlMean"))), stringsAsFactors=F)

# Get subject info.
s.info = strsplit(flist, split="_", fixed=T) # remove underscore in filename
s.info = unlist(lapply(s.info, function(x){
  x = unlist(strsplit(x, split="_"))
  c(x[1], x[2], x[3], x[4])
}))
s.info = matrix(s.info, ncol=40)
s.info = data.frame(s.info)

# transpose data frame
s.info = t(s.info)

#i=1 # wenn man sich einzelnen Probanden spezifisch anschauen möchte

for(i in 1:length(flist)){
#  for(i in 1:1){

  # reading all column headers
  s.data=read.table(file=paste(recs.dir,flist[i], collapse="",sep = ""), header=T, sep="\t")

  #delete last half of the trials # 8 trials if you want to look at the first trials only
  #s.data <- s.data[-(25:48), ]

  # define all relevant column headers
  vars = c("Trial", "Stimulus","Participant","Eye.L.R","AOI.Name","Entry.Time..ms.","Sequence", "Fixation.Count", "Fixation.Time..ms.","Average.Fixation.Duration..ms.")

  # Remove unnecessary columns
  s.data=s.data[, vars]

  ############################## Fixation Time ###############################
  # aggregating the mean
  #meanSubset = aggregate(Fixation.Time..ms.~Stimulus+AOI.Name, data=s.data, FUN=mean)

  # custom mean function: mean of fixationtime left and right eye (if only ONE eye contains 0.0 --> no averaging)
  # https://stackoverflow.com/questions/45998419/aggregate-group-means-while-ignoring-zeros-unless-0-is-the-only-value
  meanSubset.FixTime = aggregate(Fixation.Time..ms.~Stimulus+AOI.Name, data=s.data,  FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))

  # filter out all Social AOIs (12 Trials)
  meanSubSet.FixTime.Social = meanSubset.FixTime[meanSubset.FixTime$AOI.Name == "Social Interaction",]

  # filter out all Control AOIs (12 Trials)
  meanSubSet.FixTime.Control = meanSubset.FixTime[meanSubset.FixTime$AOI.Name == "Control",]


  ###############################################Proportion Scores ##########################################################
  # fixation time social AOI + fixation time control AOI (12 Trials)
  meanSubset.FixTime.Sum = aggregate(Fixation.Time..ms.~Stimulus, data=meanSubset.FixTime, FUN=sum)

  #### Anzahl an Trials die aus der Analyse ausgeschlossen wurden weil keine Daten vorhanden waren
  TrialsNoData <- subset(meanSubset.FixTime.Sum, meanSubset.FixTime.Sum$Fixation.Time..ms.==0.0)
  TrialsNoData.count <- nrow(TrialsNoData) #Anzahl der Zeilen der Tabelle

  meanSubset.FixTime.Sum[meanSubset.FixTime.Sum==0.00] <- NA   #replace 0 with NAs

  # relative looking time social (Vektor: 12 Trials)
  relSocial.FixTime = meanSubSet.FixTime.Social$Fixation.Time..ms. / meanSubset.FixTime.Sum$Fixation.Time..ms.
  ##relSocial.FixTime[ is.na(relSocial.FixTime) ] <- 0  #replace Nas with 0 for average (war in originalskript mit drin wie auch Zeile 105, dafür ist jetzt bei mean unten na.rm = TRUE)

  # relative looking time control (Vektor: 12 Trials)
  relControl.FixTime = meanSubSet.FixTime.Control$Fixation.Time..ms. / meanSubset.FixTime.Sum$Fixation.Time..ms.
  ##relControl.FixTime[ is.na(relControl.FixTime) ] <- 0  #replace Nas with 0 for average

  # Delete values with 1 oder 0 (as in these cases only one stimulus has been attended)
  # relSocial.FixTime[relSocial.FixTime == 1 | relSocial.FixTime == 0] <- NA
  
  ## If values with 1 and 0 have been deleted: for how many was this the case? (choose one of the following, but activate the code above to make sure that 1 and 0 are still in the data set. In addition, include .count coumn above when creating the empty summaryDF and below when filling the table with values. 
  # relSocial.oneStimulus <- subset(relSocial.FixTime, relSocial.FixTime==0.0 | relSocial.FixTime==1.0) #ein Vektor mit den Werten, die 0 oder 1 sind
  # relSocial.oneStimulus.count <- length(relSocial.oneStimulus) #Anzahl der Zahlen in dem Vektor

  ### mean over all social (or control) trials (1 Wert pro Proband)
  meanRelSocial.FixTime = mean(relSocial.FixTime, na.rm = TRUE)

  # mean over all control trials (1 Wert pro Proband)
  meanRelControl.FixTime = mean(relControl.FixTime, na.rm = TRUE)


  #########
  # fixation time social AOI - fixation time control AOI (12 Trials)
  meanSubset.FixTime.diff = (meanSubSet.FixTime.Social$Fixation.Time..ms. - meanSubSet.FixTime.Control$Fixation.Time..ms.)
  meanSubset.FixTime.diff[meanSubset.FixTime.diff==0.00] <- NA   #replace 0 with NAs

  # relative looking time (social-control/social+control) (Vektor: 12 Trials)
  rel.FixTime.diff = meanSubset.FixTime.diff/ meanSubset.FixTime.Sum$Fixation.Time..ms.
  rel.FixTime.diff[ is.na(rel.FixTime.diff) ] <- 0  #replace Nas with 0 for average

  # mean over all social trials (1 Wert pro Proband)
  mean.rel.FixTime.diff = mean(rel.FixTime.diff)

  ########### Relative Anzahl der Trials bei denen social preference vorliegt
  ####In dem Vektor die Anzahl der Werte, in denen >.50 geteilt durch Anzahl aller Trials
  relSocial.FixTime.Trials <- subset(relSocial.FixTime, relSocial.FixTime > 0.5) #ein Vektor mit den Werten, die >.50 sind
  relSocial.FixTime.Trials.Number <- length(relSocial.FixTime.Trials) #Anzahl der Zahlen in dem Vektor

  relSocial.FixTime.TrialsTotal <- subset(relSocial.FixTime, relSocial.FixTime > 0.0) # die gesamten Trials bei denen auf die Stimuli geschaut wurde (Ausschlus von Trials mit looking Time von 0)
  relSocial.FixTime.TrialsTotal.Number <- length(relSocial.FixTime.TrialsTotal) # Anzahl Total Trials

  relSocialTrials = relSocial.FixTime.Trials.Number / relSocial.FixTime.TrialsTotal.Number #Proportion Score: Relative Anzahl der Trials mit social preference
  #relSocialTrials = relSocial.FixTime.Trials.Number / 12 ###Proportion Score: Anzahl mit Social Preference / total Anzahl der Trials --> Selbes Ergebnismuster wie wenn an der Anzahl der Trials relativiert, in denen sozialer Stimulus angeschaut wurde.
  
  
  ############################## Enable trial-by-trial analysis ###############################
  
  # meanSubSet.FixTime.Social serves as the initial working data frame
  trialSummary <- meanSubSet.FixTime.Social
  
  # Add relSocial.FixTime
  trialSummary$relSocialFixTime <- c(relSocial.FixTime)
  
  # Add new ParentIndex column in trialSummary and fill with NA
  trialSummary[,"ParentIndex"] <- NA
  
  # Lookup the parent row index of the stimulus string in s.data and insert it for social
  for(k in 1:length(trialSummary$Stimulus)){
    trialSummary$ParentIndex[k] <- which(s.data$Stimulus == trialSummary$Stimulus[k])[1]
  }
  
  trialSummary$AgeGroup <- s.info[i,3]
  trialSummary$Sex <- s.info[i,4]
  
  trialSummary$ID <- i
  
  ### Sorting based on parent index (i.e., trial) must be at the end
  # sort by parent index
  trialSummary <- trialSummary[order(trialSummary$ParentIndex),]
  
  # create a new trials columns and fill it for social
  trialSummary[,"Trials"] <- NA
  trialSummary$Trials <- c(1:length(trialSummary$Trials))
  
  
  trialSummaryDF <- rbind(trialSummaryDF, trialSummary)
  
  #############################################################################
  
  
  ############################## Fixation Count ###############################

  # custom mean function: mean of fixation count left and right eye (if only ONE eye contains 0.0 --> no averaging)
  # https://stackoverflow.com/questions/45998419/aggregate-group-means-while-ignoring-zeros-unless-0-is-the-only-value
  meanSubset.FixCount = aggregate(Fixation.Count~Stimulus+AOI.Name, data=s.data,  FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))

  # filter out all Social AOIs
  meanSubSet.FixCount.Social = meanSubset.FixCount[meanSubset.FixCount$AOI.Name == "Social Interaction",]

  # filter out all Control AOIs
  meanSubSet.FixCount.Control = meanSubset.FixCount[meanSubset.FixCount$AOI.Name == "Control",]

  # fixation time social AOI + fixation time control AOI
  meanSubset.FixCount.Sum = aggregate(Fixation.Count~Stimulus, data=meanSubset.FixCount, FUN=sum)
  meanSubset.FixCount.Sum[meanSubset.FixCount.Sum==0.00] <- NA   #replace 0 with NAs

  # relative number of fixations social
  relSocial.FixCount = meanSubSet.FixCount.Social$Fixation.Count / meanSubset.FixCount.Sum$Fixation.Count
  relSocial.FixCount[ is.na(relSocial.FixCount) ] <- 0  #replace Nas with 0 for average

  # relative number of fixations control
  relControl.FixCount = meanSubSet.FixCount.Control$Fixation.Count / meanSubset.FixCount.Sum$Fixation.Count
  relControl.FixCount[ is.na(relControl.FixCount) ] <- 0  #replace Nas with 0 for average

  # mean over all social trials
  meanRelSocial.FixCount = mean(relSocial.FixCount)

  # mean over all control trials
  meanRelControl.FixCount = mean(relControl.FixCount)


  ############################## Fill Data in Dataframe ##########################

  # Fill data in final data frame "summaryDF"
  summaryDF[i,1] = rbind(s.info[i,2])
  summaryDF[i,2] = rbind(s.info[i,3])
  summaryDF[i,3] = rbind(s.info[i,4])
  summaryDF[i,4] = rbind(meanRelSocial.FixTime)
  summaryDF[i,5] = rbind(meanRelControl.FixTime)
  summaryDF[i,6] = rbind(meanRelSocial.FixCount)
  summaryDF[i,7] = rbind(meanRelControl.FixCount)
  summaryDF[i,8] = rbind(mean.rel.FixTime.diff)
  summaryDF[i,9] = rbind(relSocialTrials)
  summaryDF[i,10] = rbind(TrialsNoData.count)
  # summaryDF[i,11] = rbind(relSocial.oneStimulus.count)

  # Fill data in final plot data frame "Plot.data.social"
  Plot.data.social[i,1] = rbind(s.info[i,2])
  Plot.data.social[i,2] = rbind(s.info[i,3])
  Plot.data.social[i,3] = rbind(s.info[i,4])
  Plot.data.social[i,4] = rbind(meanRelSocial.FixTime)
  Plot.data.social[i,5] = rbind(meanRelSocial.FixCount)
  #Plot.data.social[i,6] = rbind(mean.rel.FixTime.diff)

  # Fill data in final plot data frame "Plot.data.control"
  Plot.data.control[i,1] = rbind(s.info[i,2])
  Plot.data.control[i,2] = rbind(s.info[i,3])
  Plot.data.control[i,3] = rbind(s.info[i,4])
  Plot.data.control[i,4] = rbind(meanRelControl.FixTime)
  Plot.data.control[i,5] = rbind(meanRelControl.FixCount)

}


