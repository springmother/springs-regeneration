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

##################################################
#### distance to seed source by species graph ####
##################################################
tree_count_gen <- 
  tree_counts %>%
  group_by(Transect_count, Spring_Name, Distance_range_m, Species) %>%
  summarize(Sum_Density = sum(Density)

# AVG_DENSITY column is the total density at each distance bin for the entire 
#spring divided by the number of transects per spring

tree_count_gen$Avg_Density = tree_count_gen$Sum_Density/tree_count_gen$Transect_count
tree_count_gen$Spring_Name = factor(tree_count_gen$Spring_Name)
tree_count_gen$Distance_range_m <- factor(tree_count_gen$Distance_range_m)

### Adding distance to live tree by spring
tree_count_springs = transectdata %>%
  group_by(Spring_Name) %>%
  summarise(Mean_dist_tree = mean (Average_dist_to_live_tree))

tree_count_gen <- merge(tree_count_gen, tree_count_springs, by = "Spring_Name")
tree_count_gen$Mean_dist_tree[is.na(tree_count_gen$Mean_dist_tree)] <- 500

tree_count_gen %>% 
  filter(Species == "PSME" | Species == "PICO")%>%
  ggplot(aes(Mean_dist_tree, Avg_Density, color = Species))+
  geom_point(size = 1)+
  geom_smooth(method = "loess", alpha = 0.1, se = TRUE)+
  scale_color_manual(values = c("PSME" = "darkgreen", "PICO" = "orange"),labels = c("PSME" = "Douglas Fir", "PICO" = "Lodgepole Pine"))+
  ggtitle("")+
  xlab("Distance to suriving seed source (m)")+
  ylab("Average density per spring (trees/hectare)")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, family ="serif"), 
        axis.text.y = element_text(size = 10, family ="serif"), 
        axis.title.y = element_text(size = 11, family ="serif"),
        axis.title.x = element_text(size = 11, family ="serif"), 
        plot.title = element_text(size = 11, family ="serif"),
        legend.title = element_text(size = 11, family ="serif"), 
        legend.text = element_text(size = 10, family ="serif"),
        legend.position = "bottom",  
        legend.box = "horizontal"
  )

