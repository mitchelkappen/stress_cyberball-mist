# Plot functions for:
#   audioAnalysis.R
#   hrvAnalysis.R

# One general plotting function
audio_pretty_plot <-
  function(emmean_dataframe, title){
    ggplot(emmean_dataframe, aes(x=condition, y=emmean, colour = condition)) +
      geom_point(size = 5) + 
      geom_line(aes(group = 1),size = 1, colour = "black", linetype = "dotted")+
      geom_errorbar(width=.25, size = 1.4, aes(ymin=emmean-SE, ymax=emmean+SE))+
      labs(y = title, x = "Feedback condition")+
      scale_colour_manual(values=cbPalette)+
      plot_theme_apa()
  }
# One general theme to clean up code
plot_theme_apa <-
  function(...){
    theme(
      # legend.key.size=unit(1.3, 'cm'),
      # legend.text=element_text(size=13),
      legend.position = "none",
      plot.title = element_text(size=rel(2)),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major.y = element_line( size=.1, color="#dedede" ),
      axis.text.x=element_text(size=rel(2)),
      axis.title.y=element_text(size=rel(1.5)),
      axis.title.x = element_text(size=rel(1.5)))
  }

# Behaviorplot specific stuff
pd <- position_dodge(0.02) # To prevent errorbars overlapping, use position_dodge to move them horizontally - move them .05 to the left and right
xplotPosition = 7.1 # set variable for right x location in plot
cbPalette <- c("#56B4E9", "#E69F00") # Set plot colors to colorblind friendly

behaviorplot <- 
  function(emmeanDataframe, fileNum, taskType, ylabel){
    ggplot(emmeanDataframe, aes(x=fileNum, y=emmean, color=taskType)) +
    geom_point(size = 3) + 
    geom_line(aes(linetype = taskType, group = taskType),size = 1)+
    geom_errorbar(width=.125, aes(ymin=emmean-SE, ymax=emmean+SE), position=pd)+
    # geom_hline(yintercept=0, linetype="dashed")+
    scale_colour_manual(values=cbPalette)+
    scale_linetype_manual(values=c("dashed", "solid")) +
    theme_bw(base_size = 8)+
    theme(legend.position="bottom")+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
    labs(y = ylabel, x = "Phase")+
    theme(axis.text.x = element_text(size = 16))+ # X Axis ticks
    theme(axis.text.y = element_text(size = 10))+ # Y axis ticks
    theme(axis.title = element_text(size = 16))+ # Axis titles
    theme(legend.text = element_text(size = 16))+ # Legend text
    theme(legend.title = element_text(size = 14))+ # Legend title
    plot_theme_apa()+
    # scale_x_discrete(labels=c("-3", "-2(r)", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8"))+
    theme(
      axis.text.x=element_text(size=rel(3)),
      axis.text.y=element_text(size=rel(2)),
      axis.title.y=element_text(size=rel(1)),
      axis.title.x = element_text(size=rel(1)),
      # legend.position = "bottom",
      legend.position = c(.8,.85),
      legend.title = element_blank()
    )
  }

addpvalues <- 
  function(gplot, emmean){
    means = summary(emmean$emmeans) # Set up emmeans variable
    contrasts = summary(emmean$contrasts) # Set up contrast variable
    
    numberofsigs = sum(contrasts$p.value < 0.05)
    for(i in 0:numberofsigs){ # Loop over number of significant contrasts
      if(i > 0){ # Only do stuff if significance present | Yes, this is hacky
        str = as.character(contrasts$contrast[contrasts$p.value < 0.05][i]) # Find relevant string contrast
        emm1 = sub(" -.*", "", str) # Get first part of string for 1st emmeans
        emm2 = sub(".* - ", "", str) # Get second part of string for 1st emmeans
        
        task = as.character(contrasts$taskType[contrasts$p.value < 0.05][i]) # Get correct corresponding task
        index1 = means$taskType == task & means$fileNum == emm1 # Compute logical index for relevant values
        index2 = means$taskType == task & means$fileNum == emm2
        
        emmeanloc = mean(c(means$emmean[index1],means$emmean[index2])) # Compute mean of the two emmeans for positioning
        stdev = sd(c(means$emmean[index1],means$emmean[index2]))
        
        # Check for significance level
        if(contrasts$p.value[contrasts$p.value < 0.05][i] < .001){
          significance = '***'
        }else if(contrasts$p.value[contrasts$p.value < 0.05][i] < .01){
          significance = '**'
        }else if(contrasts$p.value[contrasts$p.value < 0.05][i] < .05){
          significance = '*'
        }
        
        # Give significance stars corresponding colors for clarity
        if(task == 'Cyberball'){
          color = cbPalette[1]
        }else{
          color = cbPalette[2]
        }
        
        # Add significance to plot and return plot
        gplot = gplot + annotate(geom="text", x= 1.5, y=emmeanloc + stdev/7, label=significance, color=color, size = 10) # Add the annotation line to the ggplot
        
      }

    }
    return(gplot)
  }

addpvaluesBetween <- 
  function(gplot, emmean){
    means = summary(emmean$emmeans) # Set up emmeans variable
    contrasts = summary(emmean$contrasts) # Set up contrast variable
    
    numberofsigs = sum(contrasts$p.value < 0.05)
    for(i in 0:numberofsigs){ # Loop over number of significant contrasts
      if(i > 0){ # Only do stuff if significance present | Yes, this is hacky
        str = as.character(contrasts$contrast[contrasts$p.value < 0.05][i]) # Find relevant string contrast
        emm1 = sub(" -.*", "", str) # Get first part of string for 1st emmeans
        emm2 = sub(".* - ", "", str) # Get second part of string for 1st emmeans
        
        task = as.character(contrasts$fileNum[contrasts$p.value < 0.05][i]) # Get correct corresponding task
        index1 = means$fileNum == task & means$taskType == emm1 # Compute logical index for relevant values
        index2 = means$fileNum == task & means$taskType == emm2
        
        emmeanloc = mean(c(means$emmean[index1],means$emmean[index2])) # Compute mean of the two emmeans for positioning
        stdev = sd(c(means$emmean[index1],means$emmean[index2]))
        
        # Check for significance level
        if(contrasts$p.value[contrasts$p.value < 0.05][i] < .001){
          significance = '***'
        }else if(contrasts$p.value[contrasts$p.value < 0.05][i] < .01){
          significance = '**'
        }else if(contrasts$p.value[contrasts$p.value < 0.05][i] < .05){
          significance = '*'
        }
        
        # Add significance to plot and return plot
        if(task == "Control Task"){
          xloc = 0.8
          xloc2 = xloc + xloc / 20
        }else if(task == "Stress Task"){
          xloc = 2.2
          xloc2 = xloc - xloc / 20
        }
        
        ystart = means$emmean[index1]
        yend = means$emmean[index2]
        
        gplot = gplot + geom_segment( aes(x = xloc2, y = ystart, xend = xloc2, yend = yend, linetype = "R fans"), linetype = "solid", colour = "black")
        gplot = gplot + annotate(geom="text", x = xloc, y=emmeanloc, label=significance, color='black', size = 10) # Add the annotation line to the ggplot
        
        figure = figure + geom_segment( aes(x = xloc2, y = ystart, xend = xloc2, yend = yend, linetype = "R fans"), linetype = "solid", colour = "black")
        figure = figure + annotate(geom="text", x = xloc, y=emmeanloc, label=significance, color='black', size = 10)
      }
      
    }
    return(gplot)
  }

savePlot <- function(plotName, filename) {
  ggsave(file=paste0(plotPrefix, filename, ".jpeg"), width = 4000, height = 2800, dpi = 300, units = "px") # Save plot
  print(plotName)
}

##############
