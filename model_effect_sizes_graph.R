############################################
### Project: Conifer Regeneration Study
### Purpose: Effect sizes graphs
### Date Created: 12/6/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################

########################################################
### RUN Douglasfir_GLMM.R and Lodgepole_GLMM.R FIRST ###
########################################################

#####################################################
#### Graph to compare standardized effect sizes ####
####################################################
library(sjPlot)
library(sjlabelled)
library(sjmisc)

##########################################################
### parameter standardized coefficients graph for all models
##########################################################
library(sjPlot)
p = plot_models(
  Final_PICO_slope_hli,
  Final_PICO_elev,
  PSME_Final_slope,
  spacing = 0.5,
  colors = c("darkolivegreen4", "salmon4","salmon3"),
  show.values = FALSE, show.p = TRUE, p.shape = TRUE,
  line.size = 1,
  dot.size = 2,
  axis.labels = c("Distance to seed source", "Transect HLI", "Elevation (m)", "Spring HLI", "Transect slope", "Distance to spring (m)"),
  vline.color = "black", transform = NULL, axis.title = "Standardized Effect Sizes (+/- SD)", 
  m.labels = c("Lodgepole pine", "Lodgepole pine (elevation)", "Douglas-fir"), 
  legend.title = "Models")


p = p+ theme_sjplot(base_family = "serif")

p = p+  
  theme(
    legend.position = "bottom",  
    legend.box = "horizontal", 
    legend.text = element_text(color = "black", size = 12), 
    axis.text = element_text(color = "black", size = 12), 
    axis.title = element_text(color = "black", size = 13), 
    legend.title = element_text(color = "black", size = 13), legend.justification = .2
  )
p



