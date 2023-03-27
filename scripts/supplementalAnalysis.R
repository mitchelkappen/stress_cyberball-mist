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