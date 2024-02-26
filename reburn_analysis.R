###################################################################################
### Project: Conifer Regeneration Study
### Purpose: Reburn density differences
### Date Created: 02/21/2024
### by: Grace Peven, gpeven@uidaho.edu
##################################################################################

###########################################
### RUN 1_data_updates.R FIRST ###
###########################################

# load libraries
library(dplyr)

#####################
#### Douglas-fir ####
#####################
PSME_counts = 
  tree_counts %>%
  filter(Species == "PSME")


PSME_reburn = wilcox.test(Density~No_Burns, data = PSME_counts, paired = FALSE)
print(PSME_reburn)
names(PSME_counts)



PSME_reburns = ggplot(PSME_counts, aes(No_Burns, Density))+
  stat_boxplot( aes(No_Burns, Density), 
                geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.colour = "red", outlier.size = 0.8,  fatten = 2)+
  xlab("Number of Burns")+
  ylab("Density (trees/hectare)")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, family = "serif"), 
        axis.text.y = element_text(size = 10,family = "serif"), 
        axis.title.y = element_text(size = 10, family = "serif"),
        axis.title.x = element_text(size = 10, family = "serif"))

ggsave(filename = "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Chap3_Regen/R_outputs/Appendix/PSME_reburn.jpeg",  width = 5, height = 2.5, units = "in",dpi = 500)



#####################
## lodgepole pine ##
#####################
PICO_counts = 
  tree_counts %>%
  filter(Species == "PICO")

PICO_reburn = wilcox.test(Density~No_Burns, data = PICO_counts, paired = FALSE)
print(PICO_reburn)


PICO_reburns = ggplot(PICO_counts, aes(No_Burns, Density))+
  stat_boxplot( aes(No_Burns, Density), 
                geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.colour = "red", outlier.size = 0.8,  fatten = 2)+
  xlab("Number of Burns")+
  ylab("Density (trees/hectare)")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, family = "serif"), 
        axis.text.y = element_text(size = 10,family = "serif"), 
        axis.title.y = element_text(size = 10, family = "serif"),
        axis.title.x = element_text(size = 10, family = "serif"))

ggsave(filename = "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Chap3_Regen/R_outputs/Appendix/PICO_reburn.jpeg",  width = 5, height = 2.5, units = "in",dpi = 500)


