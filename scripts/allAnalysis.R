##############################
#                            #
#     All Analysis   V1      #
#    Audio and Physiology    #
#       Cyberball-MIST       #
#                            #
#############################
# 
# Author: Mitchel Kappen 
# 15-6-2022

library(arrow) # Parquets
library(lme4)
library(car)
library(emmeans)
library(ggplot2)
library(dplyr)
library(effects)

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console # # Or ctrl + l in VSCode
dev.off() # Clear plot window

# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
# plotDirectory = "C:/Users/mitch/OneDrive - UGent/UGent/Workshops & Lectures/11_SPR_2022/Poster" # This is super sloppy, but there are errors with relative plotting
plotDirectory = "C:/Users/mitch/OneDrive - UGent/Documents/GitHub/stress_cyberball-mist/figures/withBaselines"
source("functions.R") # Load document where functions are stored

includeBaseline = 1
nAGQ = 1
plotPrefix <- "/figures/"

##### Loading data ##### 
# Audio Data
audioData <-
  as.data.frame(read_parquet("../loc_data/df_gemaps_func.parquet"))

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
  
summary(allData)

####### Speech features #######
# Speech features: F0 ######
formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

datatemp <- audioData[c('F0semitoneFrom27.5Hz_sma3nz_amean', 'fileNum', 'taskType', 'Sex', 'paradigm', 'participantNum')] # Clean dataframe to check with Jens

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

if(includeBaseline == 2){
  Anova(d0.1, type = 'III')
  plot(effect("fileNum:taskType", d0.1))
}else{
  Anova(chosenModel[[1]], type = 'III')
  plot(effect("fileNum:taskType", chosenModel[[1]]))
}

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "F0 (Pitch)") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "F0") # Display and save plot

# Speech features: Jitter ######
formula <- 'jitterLocal_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

# Anova(chosenModel[[1]], type = 'III')
Anova(d0.1, type = 'III')

plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

source("functions.R") # Load document where functions are stored

figure = behaviorplot(emm0.1, fileNum, taskType, "Jitter") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "Jitter") # Display and save plot

# Speech features: Shimmer ######
formula <- 'shimmerLocaldB_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

if(includeBaseline == 2){
  Anova(d0.1, type = 'III')
  plot(effect("fileNum:taskType", d0.1))
}else{
  Anova(chosenModel[[1]], type = 'III')
  plot(effect("fileNum:taskType", chosenModel[[1]]))
}

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Shimmer") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "Shimmer") # Display and save plot

# Speech features: HNR ######
formula <- 'HNRdBACF_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

# Anova(chosenModel[[1]], type = 'III')
Anova(d0.1, type = 'III')

plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Harmonics-to-Noise Ratio") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "HNR") # Display and save plot

# Speech features: mean seg length ######
formula <- 'MeanVoicedSegmentLengthSec ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Mean Voiced Segment Length") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "MeanSegLength") # Display and save plot

# Speech features: voiced segs per sec ######
formula <- 'VoicedSegmentsPerSec ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Speech rate") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "VoicedSegmensPerSec") # Display and save plot

####### Behavioral data #######
# Behavioral: NA ######
formula <- 'VAS_NA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Negative Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "NegativeAffect") # Display and save plot

# Behavioral: PAA ######
formula <- 'VAS_PAA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Postive Activating Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "PostiveActivatingAffect") # Display and save plot

# Behavioral: PSA ######
formula <- 'VAS_PSA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Positive Soothing Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "PositiveSoothingAffect") # Display and save plot

# Behavioral: Stress ######
formula <- 'VAS_Stress ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

if(includeBaseline == 2){
  Anova(d0.1, type = 'III')
  plot(effect("fileNum:taskType", d0.1))
}else{
  Anova(chosenModel[[1]], type = 'III')
  plot(effect("fileNum:taskType", chosenModel[[1]]))
}

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Stress") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "Stress") # Display and save plot

####### Physiological data #######
# Physiological: HRV - RMSSD ######
formula <- 'rmssd ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1, d0.2)
tabel <- cbind(AIC(d0.1), AIC(d0.2))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))
plot(effect("taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "HRV - RMSSD") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "HRV_RMSSD") # Display and save plot

#Physiological: SCRR - response rate ######
formula <- 'SCRR ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3, tabel, chosenModel, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
# d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
# d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))
plot(effect("taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "SCRR") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "SCRR") # Display and save plot

####################


library(plyr)
allDataTest <- ddply(allData,.(participantNum, taskType),transform,
                     deltaF0 = F0semitoneFrom27.5Hz_sma3nz_amean - F0semitoneFrom27.5Hz_sma3nz_amean[1],
                     deltaJitter = jitterLocal_sma3nz_amean - jitterLocal_sma3nz_amean[1],
                     deltaShimmer = shimmerLocaldB_sma3nz_amean - shimmerLocaldB_sma3nz_amean[1],
                     deltaHNR = HNRdBACF_sma3nz_amean - HNRdBACF_sma3nz_amean[1],
                     deltaMeanVoiced = MeanVoicedSegmentLengthSec - MeanVoicedSegmentLengthSec[1],
                     deltaSpeed = VoicedSegmentsPerSec - VoicedSegmentsPerSec[1],
                     deltaNA = VAS_NA - VAS_NA[1],
                     deltaPAA = VAS_PAA - VAS_PAA[1],
                     deltaPSA = VAS_PSA - VAS_PSA[1],
                     deltaStress = VAS_Stress - VAS_Stress[1],
                     deltaSCRR = SCRR - SCRR[1], 
                     deltaRMSSD = rmssd - rmssd[1])
allDataTest$change2
smalldataset = allDataTest[,c("participantNum", "taskType", "fileNum")]
smalldataset = cbind(smalldataset, select(allDataTest,contains("delta")))
