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
BASEPATH <- "Z:/shares/ghep_lab/2021_VanhollebekeKappen_EEGStudy2_MIST_Cyberball_Audio/"
plotPrefix <- paste0(BASEPATH, "Data/Interim/Audio/figures/")

##### Loading data ##### 

audioData <-
  as.data.frame(read_parquet(paste0(BASEPATH,"Data/Raw/Audio/df_gemaps_func.parquet")))

audioData <- audioData %>%
  transform(participantNum = as.factor(participantNum),
            taskType = as.factor(taskType),
            descriptionType = as.factor(descriptionType),
            experimentPhase = as.factor(experimentPhase))

summary(audioData)
