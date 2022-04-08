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

audioData <-
  as.data.frame(read_parquet(paste0(BASEPATH,"Data/Raw/Audio/df_gemaps_func.parquet")))

questionData <- as.data.frame(read.csv("../loc_data/QuestionnaireResults.csv")) 
questionData$participantNum = questionData$Participant.Number

audioData = merge(audioData, questionData, by = "participantNum")

audioData <- audioData %>%
  transform(participantNum = as.factor(participantNum),
            taskType = as.factor(taskType),
            descriptionType = as.factor(descriptionType),
            experimentPhase = as.factor(experimentPhase),
            Sex = as.factor(Sex))

# This is easier to work with to plot over time
audioData$fileNum[audioData$fileName == "audio_picture_baseline.wav"] = "baseline"
audioData$fileNum[audioData$fileName == "audio_referential_control.wav"] = "control"
audioData$fileNum[audioData$fileName == "audio_picture_control.wav"] = "control rest"
audioData$fileNum[audioData$fileName == "audio_referential_stress.wav"] = "stress"
audioData$fileNum[audioData$fileName == "audio_picture_stress.wav"] = "stress rest"
audioData$fileNum <- ordered(audioData$fileNum, levels = c('baseline', 'control', 'control rest', 'stress', 'stress rest'))
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

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
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
