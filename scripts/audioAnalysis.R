##############################
#                            #
#     Audio Analysis V1      #
#       Cyberball-MIST       #
#                            #
#############################
# 
# Author: Mitchel Kappen 
# 6-4-2022

library(arrow) # Parquets
library(lme4)
library(car)
library(emmeans)
library(ggplot2)
library(dplyr)

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
source("functions.R") # Load document where functions are stored

nAGQ = 1
plotPrefix <- "../figures/"

##### Loading data ##### 
# Audio Data
audioData <-
  as.data.frame(read_parquet("../loc_data/df_gemaps_func.parquet"))

# Limesurvey Data
questionData <- as.data.frame(read.csv("../loc_data/QuestionnaireResults.csv")) 
questionData$participantNum = questionData$Participant.Number # Change name for merge

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

audioData = merge(audioData, questionData, by = "participantNum") # Merge audioData with trait questionnaires

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
            paradigm = as.factor(paradigm))
audioData$fileNum <- ordered(audioData$fileNum, levels = c('baseline', 'control', 'control rest', 'stress', 'stress rest')) # Factorize (ordered) moment
levels(audioData$fileNum) <- list("Baseline"  = "baseline", "Control Task" = "control", "Control Rest" = "control rest", "Stress Task" = "stress", "Stress Rest" = "stress rest")
levels(audioData$taskType) <- list(Cyberball = "cybb", MIST = "mist")

# Create a dataframe omitting all other time moments
audioData = filter(audioData, fileNum == "Control Task" | fileNum == "Stress Task")

summary(audioData)

####### Speech features #######
###### Speech features: F0 ######
formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

datatemp <- audioData[c('F0semitoneFrom27.5Hz_sma3nz_amean', 'fileNum', 'taskType', 'Sex', 'paradigm', 'participantNum')] # Clean dataframe to check with Jens

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "F0 (Pitch)") # Create plot
savePlot(figure, "F0") # Display and save plot

###### Speech features: Jitter ######
formula <- 'jitterLocal_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

# Anova(chosenModel[[1]], type = 'III')
Anova(d0.1, type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later

# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Jitter") # Create plot
savePlot(figure, "Jitter") # Display and save plot

###### Speech features: Shimmer ######
formula <- 'shimmerLocaldB_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

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
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Shimmer") # Create plot
savePlot(figure, "Shimmer") # Display and save plot

###### Speech features: HNR ######
formula <- 'HNRdBACF_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1,d0.2,d0.3)
tabel <- cbind(AIC(d0.1), AIC(d0.2), AIC(d0.3))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

# Anova(chosenModel[[1]], type = 'III')
Anova(d0.1, type = 'III')

plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later

# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Harmonics-to-Noise Ratio") # Create plot
savePlot(figure, "HNR") # Display and save plot

###### Speech features: mean seg length ######
formula <- 'MeanVoicedSegmentLengthSec ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

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
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Mean Voiced Segment Length") # Create plot
savePlot(figure, "MeanSegLength") # Display and save plot

###### Speech features: voiced segs per sec ######
formula <- 'VoicedSegmentsPerSec ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

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
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Speech rate") # Create plot
savePlot(figure, "VoicedSegmensPerSec") # Display and save plot

####### Behavioral data #######
###### Behavioral: NA ######
formula <- 'VAS_NA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

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
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Negative Affect") # Create plot
savePlot(figure, "NegativeAffect") # Display and save plot

###### Behavioral: PAA ######
formula <- 'VAS_PAA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

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
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Postive Activating Affect") # Create plot
savePlot(figure, "PostiveActivatingAffect") # Display and save plot

###### Behavioral: PSA ######
formula <- 'VAS_PSA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

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
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Positive Soothing Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "PositiveSoothingAffect") # Display and save plot

###### Behavioral: Stress ######
formula <- 'VAS_Stress ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = audioData # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

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
# emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

figure = behaviorplot(emm0.1, fileNum, taskType, "Stress") # Create plot
figure = addpvalues(figure, emmeans0.1)
savePlot(figure, "Stress") # Display and save plot

####### Correlations #######
cor.test(audioData$F0semitoneFrom27.5Hz_sma3nz_amean, audioData$VAS_Stress, method=c("pearson", "kendall", "spearman"))
