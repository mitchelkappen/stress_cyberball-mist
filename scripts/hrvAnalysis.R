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
library(effects) # Plot effects

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window
nAGQ = 1

# Set and Get directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set WD to script location
nAGQ = 1
plotPrefix <- "../figures/"

##### Loading data ##### 
# Audio Data
hrvdata <- as.data.frame(read_parquet("../loc_data/df_rr_feat.parquet"))
hrvdata <- as.data.frame(read_parquet("../loc_data/df_rr_feat_last_5min.parquet"))
hrvdata <- as.data.frame(read_parquet("../loc_data/df_feat_tot.parquet"))

# This part if just the control vs stress block
mist = hrvdata[hrvdata$trigger == "Start MIST stress" | hrvdata$trigger == "Start MIST control", ]
cybb = hrvdata[hrvdata$trigger == "Start cyberball stress" | hrvdata$trigger == "Start cyberball control", ]
data = rbind(mist,cybb)
data$phase[grepl("stress", data$trigger)] = 'Stress'
data$phase[grepl("control", data$trigger)] = 'Control'
data$phase = as.factor(data$phase)
# This part if all the phases
data = hrvdata

data$trigger[grepl("Start MIST stress", data$trigger)] = 'Stress'
data$trigger[grepl("Start cyberball stress", data$trigger)] = 'Stress'

data$trigger[grepl("Start MIST control", data$trigger)] = 'Control'
data$trigger[grepl("Start cyberball control", data$trigger)] = 'Control'

data$trigger[grepl("Start cyberball control", data$trigger)] = 'Control'

data$trigger[data$trigger == "Start rest baseline (eyes closed)"] = "Baseline (closed)"
data$trigger[data$trigger == "Start rest baseline (eyes open)"] = "Baseline (open)"

data$trigger[data$trigger == "Start rest control (eyes closed)"] = "Rest Control (closed)"
data$trigger[data$trigger == "Start rest control (eyes open)"] = "Rest Control (open)"

data$trigger[data$trigger == "Start rest stress (eyes closed)"] = "Rest Stress (closed)"
data$trigger[data$trigger == "Start rest stress (eyes open)"] = "Rest Stress (open)"

data$phase = factor(data$trigger, ordered = TRUE, 
       levels = c("Baseline (closed)", "Baseline (open)",
                  "Control", 
                  "Rest Control (closed)", "Rest Control (open)",
                  "Stress",
                  "Rest Stress (closed)", "Rest Stress (open)"))

data = data[nchar(data$condition) == 4, ] # Kick out the ones with weird condition name - means exclusion

data$trigger = as.factor(data$trigger)
data$patient_id = as.factor(data$patient_id)
data$condition = as.factor(data$condition)


## HRV
formula <- 'rmssd ~ phase * condition + (1|patient_id)' # Declare formula

dataModel = data # Ensure correct data is taken
rm(d0.1, d0.2, d0.3) # Just to be sure you're not comparing former models for this comparison

d0.1 <- lmer(formula,data=dataModel)
d0.2 <- glmer(formula,data=dataModel, family = Gamma(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)
d0.3 <- glmer(formula,data=dataModel, family = inverse.gaussian(link = "identity"),glmerControl(optimizer= "bobyqa", optCtrl = list(maxfun = 100000)),nAGQ = nAGQ)

# Model Selection
modelNames = c(d0.1)
tabel <- cbind(AIC(d0.1))
chosenModel = modelNames[which(tabel == min(tabel))] # Get model with lowest AIC

Anova(chosenModel[[1]], type = 'III')

plot(effect("phase:condition", chosenModel[[1]]))

emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ phase | condition, adjust ="fdr", type = "response") #we don't adjust because we do this later
emmeans0.1 <- emmeans(chosenModel[[1]], pairwise ~ condition | phase, adjust ="fdr", type = "response") #we don't adjust because we do this later
emm0.1 <- summary(emmeans0.1)$emmeans
emmeans0.1$contrasts
