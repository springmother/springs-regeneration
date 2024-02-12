############################################
### Project: Conifer Regeneration Study
### Purpose: Density graphs for manuscript
### Date Created: 7/25/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

###########################################################
          #### RUN 1_data_updates.R FIRST  #####
###########################################################

##################
## Douglas-fir ##
##################

PSME_dens = 
  tree_counts %>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Density))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot(outlier.size = 0.5)+
  scale_fill_discrete(name = "Distance Range (m)")+
  theme_classic()+
  geom_smooth(method = "lm", se =TRUE, aes(group =1), linewidth = 1, color = "blue")+
  ylim(0,2500)+
  ylab("Density (trees/hectare)")+
  xlab("")+
  ggtitle("(a) Douglas-fir (n = 60)")+
  theme(axis.text.x = element_text(size = 11, family = "serif", angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 11, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"))

####################
## lodgepole pine ##
####################
PICO_dens = 
  tree_counts %>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(x = Distance_range_m, y = Density))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot(outlier.size = 0.5)+
  geom_smooth(method = "lm", se = TRUE, aes(group =1), color = "blue", linewidth = 1)+
  theme_classic()+
  ylim(0,2500)+
  ylab("")+
  xlab("")+
  ggtitle("(b) Lodgepole pine (n = 34)")+
  theme(axis.text.x = element_text(size = 11, family = "serif", angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 11, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"))


PICO_PSME = grid.arrange(arrangeGrob(PSME_dens, PICO_dens,  ncol = 2, bottom = textGrob("Distance to Spring (m)", gp = gpar(fontfamily = "serif",fontsize = 11))))


