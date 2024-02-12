############################################
### Project: Conifer Regeneration Study
### Purpose: Growth rate analysis
### Date Created: 7/25/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################

##### Growth Rate for field aged trees
growth_rate = read.csv("Age_Height.csv")
factor(growth_rate$Distance_to_spring_m)
growth_rate$Distance_to_spring_m <- factor(growth_rate$Distance_to_spring_m, levels = c("0_10", "10_20", "20_30", "30_40"), labels = c("0-10", "10-20", "20-30", "30-40"))

#############################################
         ### Douglas-fir ###
#############################################
## anova test
PSME_gr = growth_rate %>%
  filter(Species =="PSME")

PSME_anova <- aov(Growth_Rate ~ Distance_to_spring_m, data = PSME_gr)
print(PSME_anova)
summary(PSME_anova)

## growth rate visualization
growth_rate %>%
  filter(Species == "PSME") %>%
  ggplot(aes(Distance_to_spring_m, Growth_Rate))+
  stat_boxplot( aes(Distance_to_spring_m, Growth_Rate), 
                geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.colour = "red", outlier.size = 0.8,  fatten = 0.5) +    
  stat_summary(fun.y=mean, geom="point", size=1) +
  xlab("Distance to Spring (m)")+
  ylab("Average Growth Rate (m / yr)")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))

#############################################
         ### lodgepole pine ###
#############################################
PICO_gr = growth_rate %>%
  filter(Species =="PICO")
# anova test
PICO_anova <- aov(Growth_Rate ~ Distance_to_spring_m, data = PICO_gr)
print(PICO_anova)
summary(PICO_anova)

# growth rate visualization
growth_rate %>%
  filter(Species == "PICO") %>%
  ggplot(aes(Distance_to_spring_m, Growth_Rate))+
  stat_boxplot( aes(Distance_to_spring_m, Growth_Rate), 
                geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.colour = "red", outlier.size = 0.8,  fatten = 0.5) +    
  stat_summary(fun.y=mean, geom="point", size=1) +
  xlab("Distance to Spring (m)")+
  ylab("Average Growth Rate (m / yr)")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10))

########################################################
