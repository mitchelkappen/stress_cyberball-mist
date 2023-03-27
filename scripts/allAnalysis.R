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
source("functions.R") # Load document where functions are stored
options(contrasts = c("contr.sum","contr.poly")) #use this for the p value of the t test

includeBaseline = 0 # 0 if not included, 2 if it should be included
nAGQ = 1
plotPrefix <- "/../figures/"
pvalues = c() # Create a variable to store all p-values to correct later

# Create empty dataframe for forestplot
forestdf <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Outcome" = character(0), "D" = numeric(0), "Lower" = numeric(0), "Upper" = numeric(0), "Group" = character(0)))
forestdf = data.frame(Outcome=character(0), Group=character(0),  effectsize=numeric(0), Lower=numeric(0), Upper=numeric(0), Beta=numeric(0), SE=numeric(0), t=numeric(0))
tasks = c("Cyberball", "MIST")

##### Loading data ##### 
# Audio Data
# audioData <- as.data.frame(read_parquet("../loc_data/final/df_gemaps_func_noisy.parquet"))
audioData <- as.data.frame(read_parquet("../loc_data/final/df_gemaps_func_16khz_noisy.parquet"))

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

####### Speech features #######
# Speech features: F0 ######
formula <- 'F0semitoneFrom27.5Hz_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Add pvalues to dataframe to later correct all

figure = behaviorplot(emm0.1, fileNum, taskType, "F0 (Pitch)") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "F0") # Display and save plot
figureF0 = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'F0'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Speech features: Jitter ######
formula <- 'jitterLocal_sma3nz_amean ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')

plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Add pvalues to dataframe to later correct all

figure = behaviorplot(emm0.1, fileNum, taskType, "Jitter") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "Jitter") # Display and save plot
figureJIT = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'Jitter'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Speech features: Shimmer ######
formula <- 'shimmerLocaldB_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Add pvalues to dataframe to later correct all

figure = behaviorplot(emm0.1, fileNum, taskType, "Shimmer") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "Shimmer") # Display and save plot
figureSHIM = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'Shimmer'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Speech features: HNR ######
formula <- 'HNRdBACF_sma3nz_amean ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Add pvalues to dataframe to later correct all

figure = behaviorplot(emm0.1, fileNum, taskType, "Harmonics-to-Noise Ratio") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "HNR") # Display and save plot
figureHNR = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'HNR'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Speech features: mean seg length ######
formula <- 'MeanVoicedSegmentLengthSec ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Add pvalues to dataframe to later correct all

figure = behaviorplot(emm0.1, fileNum, taskType, "Mean Voiced Segment Length") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "MeanSegLength") # Display and save plot
figureSEG = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'Voiced Seg Length'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Speech features: voiced segs per sec ######
formula <- 'VoicedSegmentsPerSec ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Add pvalues to dataframe to later correct all

figure = behaviorplot(emm0.1, fileNum, taskType, "Speech rate") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "VoicedSegmensPerSec") # Display and save plot
figureSPC = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'Voiced Seg per Sec.'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Speech features: Combine plots #######
figure <- ggarrange(figureF0, figureJIT, figureSHIM, figureHNR, figureSEG, figureSPC,
                    labels = c("A", "B", "C", "D", "E", "F"),
                    ncol = 2, nrow = 3,
                    common.legend = TRUE, legend="bottom")
savePlot(figure, "CombinedSpeech", widthval = 5000, heightval = 5700) # Display and save plot

# Correct P values SPEECH ######
names = c('F0_Cybb', 'F0_MIST', 'Jitter_Cybb', 'Jitter_MIST', 'Shimmer_Cybb', 'Shimmer_MIST', 
          'HNR_Cybb', 'HNR_MIST', 'MeanVoicedSegLength_Cybb', 'MeanVoicedSegLength_MIST', 'VoicedperSec_Cybb', 'VoicedperSec_MIST')
ps = list()
ps[names] = p.adjust(pvalues, method = "fdr", length(pvalues)) # Create list containing fdr corrected pvalues
collectedPvalues = ps
pvalues = c() # Empty the variable for other categories

####### Behavioral data #######
# Behavioral: NA ######
formula <- 'VAS_NA ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Add pvalues to dataframe to later correct all

# Plot
figure = behaviorplot(emm0.1, fileNum, taskType, "Negative Affect") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "NegativeAffect") # Display and save plot
figureNA = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'Negative Affect'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Behavioral: Stress ######
formula <- 'VAS_Stress ~ fileNum * taskType + Sex + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
pvalues  = append(pvalues ,summary(emmeans0.1$contrasts)$p.value) # Add pvalues to dataframe to later correct all

# Plot
figure = behaviorplot(emm0.1, fileNum, taskType, "Stress") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "Stress") # Display and save plot
figureS = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'Stress'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

# Behavioural: Combine plots #######
figure <- ggarrange(figureNA, figureS,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend="bottom")
savePlot(figure, "CombinedSelfReports", widthval = 5000, heightval = 1900) # Display and save plot

# Correct P values SELF-REPORT ######
names = c('NA_Cybb', 'NA_MIST', 'Stress_Cybb', 'Stress_MIST')
ps = list()
ps[names] = p.adjust(pvalues, method = "fdr", length(pvalues)) # Create list containing fdr corrected pvalues
collectedPvalues = append(collectedPvalues, ps)
pvalues = c() # Empty the variable for other categories

####### Physiological data #######
# Physiological: SCRR - response rate ######
formula <- 'SCRR ~ fileNum * taskType + (1|participantNum)' # Declare formula

dataModel = allData # Ensure correct data is taken
rm(d0.1, emmeans0.1, emmeans0.2, emm0.1, figure) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)

Anova(d0.1, type = 'III')
plot(effect("fileNum:taskType", d0.1))

emmeans0.1 <- emmeans(d0.1, pairwise ~ fileNum | taskType, adjust ="none", type = "response") #we don't adjust because we do this later
emmeans0.2 <- emmeans(d0.1, pairwise ~ taskType | fileNum, adjust ="none", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
pairs(pairs(emmeans(d0.1, ~ fileNum | taskType)), by = NULL, adjust = "fdr")
collectedPvalues = append(collectedPvalues, summary(emmeans0.1$contrasts)$p.value)

# Plot
figure = behaviorplot(emm0.1, fileNum, taskType, "SCRR") # Create plot
figure = addpvalues(figure, emmeans0.1)
figure = addpvaluesBetween(figure, emmeans0.2)
savePlot(figure, "SCRR") # Display and save plot
figureSCRR = figure

effSummary <- summary(eff_size(emmeans0.1, sigma=sigma(d0.1), edf=df.residual(d0.1)))
# Cohen's D for Forest Plots
for(i in 1:length(effSummary$taskType)){
  name = 'SCRR'
  effectsize = effSummary$effect.size[i] * -1 # Inverted
  Upper = effSummary$lower.CL[i] * -1 # Inverted
  Lower = effSummary$upper.CL[i] * -1 # Inverted
  
  contrastdf = summary(emmeans0.1$contrasts) # get contrasts
  Beta = contrastdf$estimate[i] * -1 # Inverted
  SE = contrastdf$SE[i]
  t = contrastdf$t.ratio[i] * -1 # Inverted
  forestdf[nrow(forestdf) + 1,] = c(name, as.character(effSummary$taskType[i]), effectsize, Lower, Upper, Beta, SE, t)
}

####################
# Audio Sample descriptives #######
t.first <- allData[match(unique(allData$participantNum), allData$participantNum),] # Create dataframe with one line per unique participant 
sprintf("Number of participants: %.f", nrow(t.first))
sprintf("Number of Men: %.f. Number of Women: %.f.", sum(t.first$Sex == 'Man') , sum(t.first$Sex == 'Vrouw')) 
sprintf("Age, Mean: %.2f, SD: %.2f.", mean(t.first$Age) , sd(t.first$Age))

# Now more specific
sum(allData$fileNum == 'Control Task')
sum(allData$fileNum == 'Control Task' & allData$taskType == 'Cyberball')
sum(allData$fileNum == 'Control Task' & allData$taskType == 'MIST')
length(unique(allData$participantNum[allData$fileNum == 'Control Task']))

sum(allData$fileNum == 'Stress Task')
sum(allData$fileNum == 'Stress Task' & allData$taskType == 'Cyberball')
sum(allData$fileNum == 'Stress Task' & allData$taskType == 'MIST')
length(unique(allData$participantNum[allData$fileNum == 'Stress Task']))

# Forest Plot ####
# https://stackoverflow.com/questions/58657802/forest-plot-with-subgroups-in-ggplot2
library('ggplot2') 

# Sort for visual preference
backup = forestdf
# forestdf = forestdf[order(forestdf$Group,decreasing=TRUE),]

# you can do the factoring here
forestdf$Outcome = factor(forestdf$Outcome, levels = c("Jitter", "Shimmer", "HNR", "Voiced Seg Length", "Voiced Seg per Sec.", "F0",
                                                       "Stress", "Negative Affect",
                                                       "SCRR"
                                                       ))
# Make them numeric
forestdf$effectsize = round(as.numeric(forestdf$effectsize), digits = 2)
forestdf$Lower = round(as.numeric(forestdf$Lower), digits = 2)
forestdf$Upper = round(as.numeric(forestdf$Upper), digits = 2)
forestdf$Beta = round(as.numeric(forestdf$Beta), digits = 2)
forestdf$SE = round(as.numeric(forestdf$SE), digits = 2)
forestdf$t = round(as.numeric(forestdf$t), digits = 2)

forestdf$pvalues = round(as.numeric(collectedPvalues), digits = 3)

#define colours for dots and bars
# dotCOLS = c("#a6d8f0","#f9b282") # these are actually the bars
barCOLS = c("#56B4E9", "#E69F00")
dotCOLS = c("#56B4E9", "#E69F00")
boxlims = c(0.5, 6.5, 10.5, 12.5)

removevars = 1
if(removevars == 1){
  backup = forestdf
  # boxlims = c(0.5, 1.5, 3.5, 9.5)
  boxlims = c(0.5, 6.5, 8.5, 9.5)
}

dodgevar = 0.5
##### Make Forest plot ####
forestplot <- ggplot(forestdf, aes(x=Outcome, y=effectsize, ymin=Upper, ymax=Lower,col=Group,fill=Group, group=Group)) + 
  # Make background white
  geom_rect(aes(xmin = boxlims[1], xmax = boxlims[4], ymin = -Inf, ymax = Inf),
            fill = "gray100", alpha = 0.2, linetype = "blank") +
  
  # Draw dashed lines to indicate category borders
  geom_segment(aes(x = boxlims[3], xend = boxlims[3], y = -Inf, yend = 1.5), color = "black", linetype = "longdash") +
  geom_segment(aes(x = boxlims[2], xend = boxlims[2], y = -Inf, yend = 1.5), color = "black", linetype = "longdash") +
  
  # Draw forestplot
  geom_linerange(size=8,position=position_dodge(width = dodgevar), alpha = ifelse(forestdf$pvalues < .05, 1, 0.5)) +
  geom_hline(yintercept=0, lty=2, size = 1.5) + # Draw vertical 0 line
  
  # Create dots for effect sizes
  geom_point(size=4, shape=21, colour= ifelse(forestdf$pvalues < .05, "black", "white"), alpha = ifelse(forestdf$pvalues < .05, 1, 0.5),
             stroke = 1.4, position=position_dodge(width = dodgevar)) +
  
  # Set bar and dot colors
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  
  # Set figure specifics
  scale_x_discrete(name="") +
  scale_y_continuous(limits = c(-1, 1.5)) +
  
  # Set orientation and theme
  coord_flip()+
  theme_pubr() +
  plot_theme_apa()+
  ylab("Effect Size (Cohen's D) with 95% CI") + # Plot is flipped, this is actually the x-axis
  theme(legend.position = "bottom", legend.text = element_text(size = 18), legend.title = element_text(size = 18))

# Add the category arrows
forestplot <- forestplot + 
  geom_segment(aes(x = boxlims[1]+.05, xend = boxlims[2]-.05, y = 1.35, yend = 1.35), color = "black", arrow = arrow(length = unit(0.4, "cm"), end = "both", type = "closed")) +
  geom_text(aes(x = mean(boxlims[1:2]), y = 1.4, label = "Speech"), color = "black", angle = 270) +
  geom_segment(aes(x = boxlims[2]+.05, xend = boxlims[3]-.05, y = 1.35, yend = 1.35), color = "black", arrow = arrow(length = unit(0.4, "cm"), end = "both", type = "closed")) +
  geom_text(aes(x = mean(boxlims[2:3]), y = 1.4, label = "Self-reports"), color = "black", angle = 270) +
  geom_segment(aes(x = boxlims[3]+.05, xend = boxlims[4]-.05, y = 1.35, yend = 1.35), col = "black", arrow = arrow(length = unit(0.4, "cm"), end = "both", type = "closed")) +
  geom_text(aes(x = mean(boxlims[3:4]), y = 1.4, label = "Physiological"), col = "black", angle = 270)

# Remove dots (1) and groupname (2) in legend
forestplot <- forestplot + guides(fill = guide_legend(override.aes = list(shape = NA, size = 0)))
forestplot <- forestplot + labs(fill = NULL, color = NULL)

savePlot(forestplot, "forestPlot", widthval = 2600, heightval = 3000) # Display and save plot

# Conduct the paired t-test
t.test(forestdf$effectsize[forestdf$Group == "Cyberball"], forestdf$effectsize[forestdf$Group == "MIST"])
wilcox.test(forestdf$effectsize[forestdf$Group == "Cyberball"], forestdf$effectsize[forestdf$Group == "MIST"], paired = TRUE)

# Non parametric with absolute values
wilcox.test(abs(forestdf$effectsize[forestdf$Group == "Cyberball"]), abs(forestdf$effectsize[forestdf$Group == "MIST"]), paired = TRUE)

differences <- abs(forestdf$effectsize[forestdf$Group == "Cyberball"]) - abs(forestdf$effectsize[forestdf$Group == "MIST"])

# Create the Q-Q plot
qqplot(differences, main = "Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(differences)

allData$F0semitoneFrom27.5Hz_sma3nz_amean[allData$fileNum == 'Stress Task'] - allData$F0semitoneFrom27.5Hz_sma3nz_amean[allData$fileNum == 'Control Task'] 

allData$diff <- ave(allData$F0semitoneFrom27.5Hz_sma3nz_amea, factor(allData$fileNum), FUN=function(x) c(NA,diff(x)))
