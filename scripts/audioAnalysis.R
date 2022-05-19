##############################
#                            #
#     Audio Analysis V1      #
#       Cyberball-MIST       #
#                            #
#############################
# 
# Author: Mitchel Kappen 
# 6-4-2022
##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
nAGQ = 1
# BASEPATH <- "Z:/shares/ghep_lab/2021_VanhollebekeKappen_EEGStudy2_MIST_Cyberball_Audio/"
BASEPATH <- "D:/Data/EEG_Study_2/"
plotPrefix <- paste0(BASEPATH, "Data/Interim/Audio/figures/")

##### Loading data ##### 
# Audio Data
audioData <-
  as.data.frame(read_parquet(paste0(BASEPATH,"Data/Raw/Audio/df_gemaps_func.parquet")))

# Limesurvey Data
questionData <- as.data.frame(read.csv("../loc_data/QuestionnaireResults.csv")) 
questionData$participantNum = questionData$Participant.Number # Change name for merge

# Behavioral Data
behavioralData <-
  as.data.frame(read_parquet(paste0(BASEPATH,"Data/Raw/Behavioral/behavioralData_CYBB_MIST.parquet")))
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

audioData <- audioData %>% # Factorize relevant variables
  transform(participantNum = as.factor(participantNum),
            taskType = as.factor(taskType),
            descriptionType = as.factor(descriptionType),
            experimentPhase = as.factor(experimentPhase),
            Sex = as.factor(Sex))
audioData$fileNum <- ordered(audioData$fileNum, levels = c('baseline', 'control', 'control rest', 'stress', 'stress rest')) # Factorize (ordered) moment

summary(audioData)

####### Speech features #######
###### Speech features: F0 ######
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileName * taskType + (1|participantNum)' # Declare formula
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ experimentPhase + (1|participantNum)' # Declare formula
formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula


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

plot(effect("fileName:taskType", chosenModel[[1]]))
plot(effect("experimentPhase", chosenModel[[1]]))
plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

###### Speech features: Jitter ######
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileName * taskType + (1|participantNum)' # Declare formula
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ experimentPhase + (1|participantNum)' # Declare formula
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

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileName:taskType", chosenModel[[1]]))
plot(effect("experimentPhase", chosenModel[[1]]))
plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

###### Speech features: Shimmer ######
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileName * taskType + (1|participantNum)' # Declare formula
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ experimentPhase + (1|participantNum)' # Declare formula
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

plot(effect("fileName:taskType", chosenModel[[1]]))
plot(effect("experimentPhase", chosenModel[[1]]))
plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

###### Speech features: HNR ######
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileName * taskType + (1|participantNum)' # Declare formula
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ experimentPhase + (1|participantNum)' # Declare formula
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

Anova(chosenModel[[1]], type = 'III')

plot(effect("fileName:taskType", chosenModel[[1]]))
plot(effect("experimentPhase", chosenModel[[1]]))
plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

###### Speech features: mean seg length ######
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileName * taskType + (1|participantNum)' # Declare formula
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ experimentPhase + (1|participantNum)' # Declare formula
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

Anova(d0.3, type = 'III')

plot(effect("fileNum", d0.3))
plot(effect("experimentPhase", chosenModel[[1]]))
plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts

###### Speech features: voiced segs per sec ######
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileName * taskType + (1|participantNum)' # Declare formula
# formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ experimentPhase + (1|participantNum)' # Declare formula
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

plot(effect("fileNum", chosenModel[[1]]))
plot(effect("experimentPhase", chosenModel[[1]]))
plot(effect("fileNum:taskType", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts


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
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
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
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
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
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
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
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts


####### Correlations #######
cor.test(audioData$F0semitoneFrom27.5Hz_sma3nz_amean, audioData$VAS_Stress, method=c("pearson", "kendall", "spearman"))