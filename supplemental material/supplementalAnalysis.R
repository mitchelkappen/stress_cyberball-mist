##############################
#                            #
#    supplemental Analysis   #
#        Self-reports        #
#       Cyberball-MIST       #
#                            #
#############################
# The first part of this code follows the exact same structure as allAnalysis.R
# Only the following parts are different:
#     Behavioral: PAA
#     Behavioral: PSA
#     Physiological: HRV - RMSSD
# Author: Mitchel Kappen 
# 27-3-2023

library(arrow) # Parquets
library(lme4)
library(car)
library(emmeans)
library(ggplot2)
library(dplyr)
library(effects)
library(ggpubr)
library(psych) # Cohen.d

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console # # Or ctrl + l in VSCode
dev.off() # Clear plot window

# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
plotDirectory = "C:/Users/mitch/OneDrive - UGent/UGent/Documents/GitHub/stress_cyberball-mist/figures/recovery" # This is super sloppy, but there are errors with relative plotting
# plotDirectory = "C:/Users/mitch/OneDrive - UGent/Documents/GitHub/stress_cyberball-mist/figures/withBaselines"
plotDirectory = dirname(rstudioapi::getActiveDocumentContext()$path)
source("../scripts/functions.R") # Load document where functions are stored
options(contrasts = c("contr.sum","contr.poly")) #use this for the p value of the t test

includeBaseline = 2 # 0 if not included, 2 if it should be included
nAGQ = 1
plotPrefix <- "/../figures/"
pvalues = c() # Create a variable to store all p-values to correct later

# Create empty dataframe for forestplot
forestdf <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Outcome" = character(0), "D" = numeric(0), "Lower" = numeric(0), "Upper" = numeric(0), "Group" = character(0)))
forestdf = data.frame(Outcome=character(0), Group=character(0),  effectsize=numeric(0), Lower=numeric(0), Upper=numeric(0), Beta=numeric(0), SE=numeric(0), t=numeric(0))
tasks = c("Cyberball", "MIST")

##### Loading data ##### 
# Audio Data
audioData <- as.data.frame(read_parquet("../loc_data/df_gemaps_func_16khz_noisy.parquet"))

# Limesurvey Data
questionData <- as.data.frame(read.csv("../loc_data/QuestionnaireResults.csv")) 
questionData$participantNum = questionData$Participant.Number # Change name for merge

colnames(questionData)[which(names(questionData) == "DASS.SCORES")] <- "DASS.Depression"
colnames(questionData)[which(names(questionData) == "X")] <- "DASS.Anxiety"
colnames(questionData)[which(names(questionData) == "X.1")] <- "DASS.Stress"

colnames(questionData)[which(names(questionData) == "RRS.SCORES")] <- "RRS.SCORE"
colnames(questionData)[which(names(questionData) == "X.2")] <- "RRS.Reflection"
colnames(questionData)[which(names(questionData) == "X.3")] <- "RRS.Brooding"

audioData = merge(audioData, questionData, by = "participantNum") # Merge audioData with trait questionnaires

# Behavioral Data
behavioralData <-
  as.data.frame(read_parquet("../loc_data/behavioralData_CYBB_MIST.parquet"))
behavioralData$participantNum = behavioralData$participant_ID # Change name for merge
behavioralData <- behavioralData[c("participantNum", # Create smaller dataframe with only relevant variables
                                   "Mean_SCRS_Baseline", "Mean_SCRS_Control", "Mean_SCRS_Stress", 
                                   "Mean_VAS_NA_Baseline", "Mean_VAS_PAA_Baseline", "Mean_VAS_PSA_Baseline", "VAS_Stress_Baseline", 
                                   "Mean_VAS_NA_Control_Post_Rest", "Mean_VAS_PAA_Control_Post_Rest", "Mean_VAS_PSA_Control_Post_Rest", "VAS_Stress_Control_Post_Rest",
                                   "Mean_VAS_NA_Control", "Mean_VAS_PAA_Control", "Mean_VAS_PSA_Control", "VAS_Stress_Control",
                                   "Mean_VAS_NA_Stress_Post_Rest", "Mean_VAS_PAA_Stress_Post_Rest", "Mean_VAS_PSA_Stress_Post_Rest", "VAS_Stress_Stress_Post_Rest", 
                                   "Mean_VAS_NA_Stress", "Mean_VAS_PAA_Stress", "Mean_VAS_PSA_Stress", "VAS_Stress_Stress",
                                   "taskType")]

behavioralDataLong <- reshape(behavioralData, direction='long', # Transform dataframe to long format
                              varying=list(c("Mean_VAS_NA_Baseline", "Mean_VAS_NA_Control_Post_Rest", "Mean_VAS_NA_Control", "Mean_VAS_NA_Stress_Post_Rest", "Mean_VAS_NA_Stress"),
                                           c("Mean_VAS_PAA_Baseline", "Mean_VAS_PAA_Control_Post_Rest", "Mean_VAS_PAA_Control", "Mean_VAS_PAA_Stress_Post_Rest", "Mean_VAS_PAA_Stress"),
                                           c("Mean_VAS_PSA_Baseline", "Mean_VAS_PSA_Control_Post_Rest", "Mean_VAS_PSA_Control", "Mean_VAS_PSA_Stress_Post_Rest", "Mean_VAS_PSA_Stress"),
                                           c("VAS_Stress_Baseline", "VAS_Stress_Control_Post_Rest", "VAS_Stress_Control", "VAS_Stress_Stress_Post_Rest", "VAS_Stress_Stress")),
                              timevar='fileNum',
                              times=c('baseline', 'control rest', 'control', 'stress rest', 'stress'),
                              v.names=c('VAS_NA', 'VAS_PAA', 'VAS_PSA', 'VAS_Stress'),
                              idvar= c('participantNum', 'taskType'))


# This is easier to work with to plot over time and needed to merge with behavioralData
audioData$fileNum[audioData$fileName == "audio_picture_baseline.wav"] = "baseline"
audioData$fileNum[audioData$fileName == "audio_referential_control.wav"] = "control"
audioData$fileNum[audioData$fileName == "audio_picture_control.wav"] = "control rest"
audioData$fileNum[audioData$fileName == "audio_referential_stress.wav"] = "stress"
audioData$fileNum[audioData$fileName == "audio_picture_stress.wav"] = "stress rest"

audioData = merge(audioData, behavioralDataLong, by = c("participantNum","taskType", "fileNum"))
audioData$paradigm[grepl("referential", audioData$fileName)] = "Referential"
audioData$paradigm[grepl("picture", audioData$fileName)] = "Pic Describe"

audioData <- audioData %>% # Factorize relevant variables
  transform(participantNum = as.factor(participantNum),
            taskType = as.factor(taskType),
            descriptionType = as.factor(descriptionType),
            experimentPhase = as.factor(experimentPhase),
            Sex = as.factor(Sex),
            Age = as.double(Age),
            paradigm = as.factor(paradigm),
            DASS.Depression = as.double(DASS.Depression),
            DASS.Anxiety = as.double(DASS.Anxiety),
            DASS.Stress = as.double(DASS.Stress),
            RRS.SCORE = as.double(RRS.SCORE),
            RRS.Reflection = as.double(RRS.Reflection),
            RRS.Brooding = as.double(RRS.Brooding))

audioData$fileNum <- ordered(audioData$fileNum, levels = c('baseline', 'control', 'control rest', 'stress', 'stress rest')) # Factorize (ordered) moment
levels(audioData$fileNum) <- list("Baseline"  = "baseline", "Control Task" = "control", "Control Rest" = "control rest", "Stress Task" = "stress", "Stress Rest" = "stress rest")
levels(audioData$taskType) <- list(Cyberball = "cybb", MIST = "mist")

# Create a dataframe omitting all other time moments
if(includeBaseline == 0){
  audioData = filter(audioData, fileNum == "Control Task" | fileNum == "Stress Task")
}else if(includeBaseline > 0){
  audioData = filter(audioData, fileNum == "Baseline" | fileNum == "Control Task" | fileNum == "Stress Task")
  print('Baseline data is included')
}

# Physiological Data (couldn't do that earlier, because different time moments)
physiologicalData <- as.data.frame(read_parquet("../loc_data/df_feat_tot.parquet"))
physiologicalData$taskType[physiologicalData$condition == "mist"] = "MIST"
physiologicalData$taskType[physiologicalData$condition == "cybb"] = "Cyberball"

physiologicalData$participantNum = physiologicalData$patient_id

physiologicalData$fileNum[physiologicalData$trigger == "Start rest baseline (eyes open)"] = "Baseline"
physiologicalData$fileNum[physiologicalData$trigger == "Start MIST control"] = "Control Task"
physiologicalData$fileNum[physiologicalData$trigger == "Start MIST stress"] = "Stress Task"
physiologicalData$fileNum[physiologicalData$trigger == "Start cyberball control"] = "Control Task"
physiologicalData$fileNum[physiologicalData$trigger == "Start cyberball stress"] = "Stress Task"

physiologicalData <- physiologicalData[c("participantNum", "taskType", "fileNum", "duration_s", "rmssd", "sdnn", # nolint
                                         "mean_hr", "max_hr", "min_hr", "std_hr", "lf", "hf", "lf_hf_ratio", "total_power", # nolint
                                         "SCRR", "phasic_area", "tonic_mean", "phasic_area_normalized", "mean_EDA_SQI")] # nolint

if(includeBaseline == 0){
  physiologicalData = filter(physiologicalData, fileNum == "Control Task" | fileNum == "Stress Task")
}else if(includeBaseline > 0){
  physiologicalData = filter(physiologicalData, fileNum == "Baseline" | fileNum == "Control Task" | fileNum == "Stress Task")
  print('Baseline data is included')
}

#  Present in physiological, but not in rest
physiologicalData$participantNum[!physiologicalData$participantNum %in% audioData$participantNum]

# Present in rest, but not in physiological
audioData$participantNum[!audioData$participantNum %in% physiologicalData$participantNum]

# Merge to final dataframe
allData = merge(audioData, physiologicalData, by = c("participantNum","taskType", "fileNum"))

# Do baseline correct if includeBaseline == 2
if(includeBaseline == 2){
  allDataBackup = allData # Backup full dataframe
  # Compute baseline corrected delta scores
  allData <- ddply(allData,.(participantNum, taskType),transform,
                   F0semitoneFrom27.5Hz_sma3nz_amean = F0semitoneFrom27.5Hz_sma3nz_amean - F0semitoneFrom27.5Hz_sma3nz_amean[1],
                   jitterLocal_sma3nz_amean = jitterLocal_sma3nz_amean - jitterLocal_sma3nz_amean[1],
                   shimmerLocaldB_sma3nz_amean = shimmerLocaldB_sma3nz_amean - shimmerLocaldB_sma3nz_amean[1],
                   HNRdBACF_sma3nz_amean = HNRdBACF_sma3nz_amean - HNRdBACF_sma3nz_amean[1],
                   MeanVoicedSegmentLengthSec = MeanVoicedSegmentLengthSec - MeanVoicedSegmentLengthSec[1],
                   VoicedSegmentsPerSec = VoicedSegmentsPerSec - VoicedSegmentsPerSec[1],
                   VAS_NA = VAS_NA - VAS_NA[1],
                   VAS_PAA = VAS_PAA - VAS_PAA[1],
                   VAS_PSA = VAS_PSA - VAS_PSA[1],
                   VAS_Stress = VAS_Stress - VAS_Stress[1],
                   SCRR = SCRR - SCRR[1], 
                   rmssd = rmssd - rmssd[1])
  # Remove baseline rows
  allData = allData[allData$fileNum != "Baseline", ]
}

allData$fileNum = droplevels(allData$fileNum)
summary(allData)

# Behavioral: PAA ######
formula <- 'VAS_PAA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")

# Plot
figure = behaviorplot(emm0.1, fileNum, taskType, "Postive Activating Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "PostiveActivatingAffect") # Display and save plot
figureAA = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'Positive Activating Affect'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Behavioral: PSA ######
formula <- 'VAS_PSA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")

# Plot
figure = behaviorplot(emm0.1, fileNum, taskType, "Positive Soothing Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "PositiveSoothingAffect") # Display and save plot
figureSA = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'Positive Soothing Affect'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Physiological: HRV - RMSSD ######
formula <- 'rmssd ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")

# Plot
figure = behaviorplot(emm0.1, fileNum, taskType, "HRV - RMSSD") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "HRV_RMSSD") # Display and save plot
figureHRV = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'RMSSD'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}
# Physiological: HRV - HF ######
formula <- 'hf ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")

# Plot
figure = behaviorplot(emm0.1, fileNum, taskType, "HRV - HF") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "HRV_HF") # Display and save plot
figureHRVHF = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'HF'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Physiological: HRV - LF ######
formula <- 'lf ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")

# Plot
figure = behaviorplot(emm0.1, fileNum, taskType, "HRV - LF") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "HRV_LF") # Display and save plot
figureHRVHF = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'LF'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}