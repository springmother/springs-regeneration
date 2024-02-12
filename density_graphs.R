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
ggsave(filename = "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Chap3_Regen/R_outputs/density_facet.jpeg", plot = PICO_PSME,  width = 8, height = 3, units = "in",dpi = 500)

#### Normalized Density ###
#PSME norm
tree_counts %>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Density_norm))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot(outlier.size = 0.5)+
  scale_fill_discrete(name = "Distance Range (m)")+
  theme_classic()+
  ylab("Normalized Density by Transect (trees/hectare)")+
  xlab("Distance from spring (meters)")+
  ggtitle("(a) Douglas-fir")+
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11))
### Doug Fir densitys with dist. to seed source color coded
tree_counts %>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Density, color = Average_dist_to_live_tree))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot()+
  geom_jitter(position=position_jitter(width=.2), size=2)+
  scale_color_continuous(low = "lightgreen", high = "red", breaks = c(50,500)) +
  theme_bw() +
  ylab("Density (trees/ha)") +
  xlab("Distance from Spring (m)") +
  labs(color = "Average Distance to Seed Source (m)") +
  ggtitle("Douglas-fir density at all transects") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom", legend.title = element_text(size = 10))

### PICO density with dist. to seed source color coded
tree_counts %>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(x = Distance_range_m, y = Density, color = Average_dist_to_live_tree))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot()+
  geom_jitter(position=position_jitter(width=.2), size=2)+
  scale_color_continuous(low = "lightgreen", high = "red", breaks = c(50,500)) +
  theme_bw() +
  ylab("Density (trees/ha)") +
  xlab("Distance from Spring (m)") +
  labs(color = "Average Distance to Seed Source (m)") +
  ggtitle("Lodgepole pine density at all transects") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom", legend.title = element_text(size = 10))

###############################################################################
### Generalizing across distance bins and species without transect ID

tree_count_gen <- 
  tree_counts %>%
  group_by(Transect_count, Spring_Name, Distance_range_m, Species) %>%
  summarize(Sum_Density = sum(Density))





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


 #### Doug Fir density - average density at springs 
tree_count_gen%>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Avg_Density)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.colour = "black") +
  geom_jitter(position = position_jitter(width = .3), size = 3) +
  theme_bw() +
  ylab("Average Density (trees/ha)") +
  xlab("Distance from Spring (m)") +
  ggtitle("Douglas Fir density distribution at springs") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom", legend.title = element_text(size = 10))


tree_count_gen%>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Avg_Density, color = Mean_dist_tree)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.colour = "black") +
  geom_jitter(position = position_jitter(width = .3), size = 3) +
  scale_color_continuous(low = "darkgreen", high = "red", breaks = c(50,500)) +
  theme_bw() +
  ylab("Average Density (trees/ha)") +
  xlab("Distance from Spring (m)") +
  labs(color = "Average Distance to Seed Source (m)") +
  ggtitle("Douglas Fir density distribution at springs") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom", legend.title = element_text(size = 10))




###########################################################################
### Density of PSME by Distance and grouped by Spring
tree_counts %>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Density, fill = Spring_Name)) +
  geom_bar(position = "dodge", stat = "identity")+
  theme_bw()+
  ylab("Tree Density (trees/ha)")+
  xlab("Distance from Spring (m)")+
  ggtitle("Douglas Fir Count - All Transects")

## Density Sum facet wraps by Spring
### PSME
tree_counts %>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Density)) +
  geom_bar(stat = "identity")+
  facet_wrap(~ Spring_Name)+
  theme_bw()+
  ylab("Tree Density (trees/ha)")+
  xlab("Distance Bin (m)")+
  ggtitle("PSME Density")

### PICO
tree_counts %>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(x = Distance_range_m, y = Density)) +
  geom_bar(stat = "identity")+
  facet_wrap(~ Spring_Name)+
  theme_bw()+
  ylab("Tree Density (trees/ha)")+
  xlab("Distance Bin (m)")+
  ggtitle("PICO Density")

## All Species
ggplot(data = tree_counts, aes(x = Distance_range_m, y = Density)) +
  geom_bar(stat = "identity")+
  facet_wrap(~ Spring_Name)+
  theme_bw()+
  ylab("Tree Density (trees/ha)")+
  xlab("Distance Bin (m)")+
  ggtitle("Tree Density")

## Sum of Tree density across all springs/transects for PSME
tree_counts %>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Density)) +
  geom_bar(stat = "identity")+
  theme_bw()+
  ylab("Tree Density (trees/ha)")+
  xlab("Distance Bin (m)")+
  ggtitle("Douglas Fir Density - All Springs")

## Sum of Tree density across all springs/transects for PICO
tree_counts %>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(x = Distance_range_m, y = Density)) +
  geom_bar(stat = "identity")+
  theme_bw()+
  ylab("Tree Density (trees/ha)")+
  xlab("Distance Bin (m)")+
  ggtitle("PICO Density - All Springs")


## Sum of Tree density across all springs/transects for ALL SPECIES
transectdata%>% 
  ggplot(aes(Distance_range_m))+
  geom_bar()+
  theme_bw()+
  ylab("Tree Count")+
  xlab("Distance Bin (m)")+
  ggtitle("All Species")


### Density of PSME by Distance and Spring
tree_counts %>% 
  filter(Species == "PSME") %>% 
  ggplot(aes(x = Distance_range_m, y = Density, fill = Spring_Name)) +
  geom_bar(position = "dodge", stat = "identity")+
  theme_bw()+
  ylab("Tree Density (trees/ha)")+
  xlab("Distance from Spring (m)")+
  ggtitle("Douglas Fir Count - All Transects")



### Count of PICO by Distance

tree_counts %>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(x = Distance_range_m, y = Density))+
  geom_boxplot()+
  geom_jitter()+
  theme_bw()+
  ylab("Density (trees/ha)")+
  xlab("Distance from Spring (m)")+
  ggtitle("Lodgepole- All Transects")

transectdata %>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(Distance_range_m))+
  geom_bar()+
  theme_bw()+
  ylab("Tree Count")+
  xlab("Distance from Spring (m)")+
  ggtitle("Lodgepole Count - All Transects")

tree_counts%>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(Distance_range_m, Density))+
  geom_boxplot()



### PICO density at all transects
tree_counts %>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(x = Distance_range_m, y = Density, color = Average_dist_to_live_tree)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.colour = "black") +
  geom_jitter(position = position_jitter(width = .2), size = 2) +
  scale_color_continuous(low = "darkgreen", high = "red", breaks = c(100,500)) +
  theme_bw() +
  ylab("Density (trees/ha)") +
  xlab("Distance from Spring (m)") +
  labs(color = "Average Distance to Seed Source (m)") +
  ggtitle("Lodgepole Pine Density at all Transects") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom", legend.title = element_text(size = 10))

### summarized by spring for PICO including zeroes
tree_count_gen %>% 
  filter(Species == "PICO") %>% 
  ggplot(aes(x = Distance_range_m, y = Avg_Density, color = Mean_dist_tree)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(outlier.colour = "black") +
  geom_jitter(position = position_jitter(width = .2), size = 2) +
  scale_color_continuous(low = "darkgreen", high = "red", breaks = c(100,500)) +
  theme_bw() +
  ylab("Average Density (trees/ha)") +
  xlab("Distance from Spring (m)") +
  labs(color = "Average Distance to Seed Source (m)") +
  ggtitle("Lodgepole Pine Average Density at Springs") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12),
    plot.title = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom", legend.title = element_text(size = 10))



unique_species_list <- transectdata %>%
  group_by(Spring_Name) %>%
  distinct(Species) %>%
  ungroup()

# View the resulting data frame
View(unique_species_list)



#### Reburned springs density distribution

tree_counts_reburn = 
  transectdata %>%
  group_by(Distance_range_m, Species, Spring_Name, Spring_Type, Transect_ID, Forest_Type, Area_ha, Average_dist_to_live_tree, CC_1_PCT, CC_2_PCT, CC_3_PCT, Elevation_ft, Aspect, Transect_Slope_deg, Slope, Years_Since_Fire, HS_FireYear, Transect_count, No_Burns) %>%
  summarize(Tree_Count = sum(Count))

tree_counts_reburn$Density = tree_counts_reburn$Tree_Count/tree_counts_reburn$Area_ha 



medians <- tree_counts_reburn %>%
  filter(Species == "PSME", No_Burns == 1) %>%
  group_by(Distance_range_m) %>%
  summarize(median_value = median(Density))


PSME_reburn_1 = 
  tree_counts_reburn %>% 
  filter(Species == "PSME", No_Burns == 1) %>% 
  ggplot(aes(x = Distance_range_m, y = Density))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot(outlier.size = 0.5)+
  theme_classic()+
  ggtitle("Douglas-fir - Spring burned once (n=17)")+
  ylim(0,4000)+
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11))+ 
  geom_text(data = medians, aes(x = Distance_range_m, y = median_value, label = paste("Median =", median_value)),
            vjust = -10, hjust = 0, size = 3, color = "darkgreen")
###################################################################

medians_2 <- tree_counts_reburn %>%
  filter(Species == "PSME", No_Burns == 2) %>%
  group_by(Distance_range_m) %>%
  summarize(median_value = median(Density))

PSME_reburn_2 = 
  tree_counts_reburn %>% 
  filter(Species == "PSME", No_Burns == 2) %>% 
  ggplot(aes(x = Distance_range_m, y = Density))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot(outlier.size = 0.5)+
  theme_classic()+
  ylim(0,4000)+
  ggtitle("Douglas-fir- Reburned springs (n=10)")+
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11))+
  geom_text(data = medians_2, aes(x = Distance_range_m, y = median_value, label = paste("Median =", median_value)),
            vjust = -10, hjust = 0, size = 3, color = "darkgreen")


PSME_reburns = grid.arrange(PSME_reburn_1, PSME_reburn_2, ncol = 1)
ggsave(filename = "C:/Users/gpeve/OneDrive - University of Idaho/Springs Research/Data/Chap3_Regen/R_outputs/PSME_reburn_dens.jpeg", plot = PSME_reburns,  width = 7, height = 6, units = "in",dpi = 500)

#########################################
## PICO

medians <- tree_counts_reburn %>%
  filter(Species == "PICO", No_Burns == 1) %>%
  group_by(Distance_range_m) %>%
  summarize(median_value = median(Density))


PICO_reburn_1 = 
  tree_counts_reburn %>% 
  filter(Species == "PICO", No_Burns == 1) %>% 
  ggplot(aes(x = Distance_range_m, y = Density))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot(outlier.size = 0.5)+
  theme_classic()+
  ggtitle("Lodgepole pine - Spring burned once (n=17)")+
  ylim(0,4000)+
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11))+ 
  geom_text(data = medians, aes(x = Distance_range_m, y = median_value, label = paste("Median =", median_value)),
            vjust = -10, hjust = 0, size = 3, color = "darkgreen")
###################################################################

medians_2 <- tree_counts_reburn %>%
  filter(Species == "PICO", No_Burns == 2) %>%
  group_by(Distance_range_m) %>%
  summarize(median_value = median(Density))

PICO_reburn_2 = 
  tree_counts_reburn %>% 
  filter(Species == "PICO", No_Burns == 2) %>% 
  ggplot(aes(x = Distance_range_m, y = Density))+
  stat_boxplot(geom = "errorbar",width = 0.15)+
  geom_boxplot(outlier.size = 0.5)+
  theme_classic()+
  ylim(0,4000)+
  ggtitle("Lodgepole pine- Reburned springs (n=10)")+
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11))+
  geom_text(data = medians_2, aes(x = Distance_range_m, y = median_value, label = paste("Median =", median_value)),
            vjust = -10, hjust = 0, size = 3, color = "darkgreen")


PICO_reburns = grid.arrange(PICO_reburn_1, PICO_reburn_2, ncol = 1)
ggsave(filename = "C:/Users/gpeve/OneDrive - University of Idaho/Springs Research/Data/Chap3_Regen/R_outputs/PICO_reburn_dens.jpeg", plot = PICO_reburns,  width = 7, height = 6, units = "in",dpi = 500)
