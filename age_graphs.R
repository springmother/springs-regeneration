
############################################
### Project: Conifer Regeneration Study
### Purpose: Tree age graphs
### Date Created: 7/25/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################


library(ggplot2)
library(gridExtra)
library(stringr)
library(dplyr)

###########################################################
# NEED TO RUN 1_data_updates.R and age_height_model.R first
###########################################################

# count number of trees per species
age_counts <- transectdata %>%
  group_by(Species) %>%
  summarize(count = sum(!is.na(Age_new)))
print(age_counts)
######################################################
### Boxplot for tree ages by fire year and species ###
######################################################

# updating distance bin labels for graphs
transectdata$Distance_range_m <- factor(transectdata$Distance_range_m, levels = c("0_10", "10_20", "20_30", "30_40"), labels = c("0-10", "10-20", "20-30", "30-40"))

# Douglas-fir
a1 = transectdata %>%
  filter(Species == "PSME") %>%
  ggplot(aes(y = Age_new, x = Distance_range_m)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = .2), size = 0.25) +
  stat_summary(
    fun = "mean",
    geom = "point",
    size = 1.5,
    color = "blue"
  )+
  ggtitle("(a) Douglas-fir (n = 521)") +
  xlab("") +
  ylab("Age (years)") +
  facet_wrap(~Fire_Year)+
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    text = element_text(size = 10, family = "serif"),                    # Increase text size
    plot.title = element_text(size = 10, family = "serif") # Increase title size
  )



## lodgepole pine
a2 = transectdata %>%
  filter(Species == "PICO") %>%
  ggplot(aes(y = Age_new, x = Distance_range_m)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitter(width = .2), size = 0.25) +
  stat_summary(
    fun = "mean",
    geom = "point",
    size = 1.5,
    color = "blue"
  )+
  ggtitle("(b) Lodgepole pine (n = 261)") +
  xlab("Distance from Spring (m)") +
  ylab("Age (years)") +
  facet_wrap(~Fire_Year)+
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    text = element_text(size = 10, family = "serif"),                    # Increase text size
    plot.title = element_text(size = 10, family = "serif") # Increase title size
  )

a3 = grid.arrange(a1, a2, nrow = 2)

ggsave(filename = "C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Chap3_Regen/R_outputs/PSME_PICO_age_v2.jpeg",  plot = a3, width = 6.5, height = 5, units = "in",dpi = 500)



################################
### Summary with Fire Year
##################################
# age statistics included in appendix of manuscript
age_stats =
  transectdata %>%
  filter(!Count == 0)%>%
  group_by(Distance_range_m,Species, Fire_Year) %>%
  summarize(Mean_Age = mean(Age_new, na.rm = TRUE), Median_Age = median(Age_new, na.rm = TRUE), Max_Age = max(Age_new, na.rm = TRUE),Min_Age = min(Age_new, na.rm = TRUE), n = length(Age_new))

View(age_stats)
#######################################################################


 

 