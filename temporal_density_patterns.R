############################################
### Project: Conifer Regeneration Study
### Purpose:Cumulative Density with Time Since Fire
### Date Created: 10/18/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################

library(ggplot2)
library(ggborderline)
library(gridExtra)
library(dplyr)

#######################################################
## RUN 1_data_updates.R and age_height_model.R FIRST 
#######################################################
 

################################
## Make updates to dataset ##
################################

transectdata$Distance_range_m = factor(transectdata$Distance_range_m)            # set distance bins as factor
transectdata$Spring_Name = factor(transectdata$Spring_Name)                      # set spring name as factor

## years after fire tree established
transectdata$est_yrs_since_fire = (transectdata$Years_Since_Fire) - (transectdata$Age_new) ### years after fire that trees established
transectdata$Distance_range_m <- factor(transectdata$Distance_range_m, levels = c("0_10", "10_20", "20_30", "30_40"), labels = c("0-10", "10-20", "20-30", "30-40"))
transectdata$estyear = 2023- transectdata$Age_new ## year a tree established

estyear_counts = 
  transectdata %>%
  group_by(Distance_range_m, Species, Area_ha, estyear, Fire_Year) %>%
  summarize(Tree_Count = sum(Count))
estyear_counts$Density = estyear_counts$Tree_Count/estyear_counts$Area_ha 

estyear_new = 
  estyear_counts %>%
  group_by(Species, Distance_range_m, estyear) %>%
  arrange(estyear) %>% 
  summarise(sum = sum(Density)) %>%
  mutate(cumsum = cumsum(sum))

estyear_new$Distance_range_m = factor(estyear_new$Distance_range_m)

estyear_fires= 
  estyear_counts %>%
  group_by(Fire_Year,Species, Distance_range_m, estyear) %>%
  arrange(estyear) %>% 
  summarise(sum = sum(Density)) %>%
  mutate(cumsum = cumsum(sum))

estyear_fires$Distance_range_m = factor(estyear_fires$Distance_range_m)

estyear_blank = read.csv("estyearfires.csv")   ## easier to create the table structure I want in excel and then import

estyear_blank$Fire_Year = estyear_blank$HS_FireYear
estyear_merge <- merge(estyear_counts, estyear_blank, by = c("Species", "Distance_range_m", "Fire_Year", "estyear"), all = TRUE)

estyear_merge$Density[is.na(estyear_merge$Density)] <- 0

estyear_merge_new= 
  estyear_merge %>%
  group_by(Fire_Year,Species, Distance_range_m, estyear) %>%
  arrange(estyear) %>% 
  summarise(sum = sum(Density)) %>%
  mutate(cumsum = cumsum(sum))

View(estyear_merge_new)

################################################################
## Density with time since fire separated by fire and species ##
################################################################

## Douglas-fir 1988

PSME_sum_1988 = estyear_merge_new %>%
  filter(Species == "PSME", Fire_Year == 1988) %>%
  ggplot(aes(x = years_fire_est, y = sum, fill = Distance_range_m)) +
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = seq(0, 35, by = 2))+
  labs(fill = "Distance to spring (m)",
       x = "Years Since Fire",
       title = "Douglas-fir: 1988",
       color = "Distance to spring (m)")+
  scale_fill_brewer(palette = "Greens", direction =-1) +  # Set fill color using Blues palette
  scale_color_brewer(palette = "Greens", direction =-1)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 10)
  )

PSME_sum_1988 = PSME_sum_1988 +
  geom_borderline(data = estyear_merge_new %>%
                    filter(Species == "PSME", Fire_Year == 1988),
                  aes(x = years_fire_est, y = cumsum * 0.2, color= Distance_range_m),
                  size = 1, bordercolour = "black",borderwidth = 0.5, show.legend = FALSE) +
  scale_y_continuous(name = "Density (trees/hectare)",
                     sec.axis = sec_axis( ~ . / 0.2, name = "Cumulative Density (trees/hectare)"))


print(PSME_sum_1988)

## Douglas-fir 2000

PSME_sum_2000 = estyear_merge_new %>%
  filter(Species == "PSME", Fire_Year == 2000) %>%
  ggplot(aes(x = years_fire_est, y = sum, fill = Distance_range_m)) +
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = seq(0, 35, by = 2))+
  labs(fill = "Distance to spring (m)",
       x = "Years Since Fire",
       title = "Douglas-fir: 2000",
       color = "Distance to spring (m)")+
  scale_fill_brewer(palette = "Greens", direction =-1) +  # Set fill color using Blues palette
  scale_color_brewer(palette = "Greens", direction =-1)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 10)
  )

PSME_sum_2000 = PSME_sum_2000 +
  geom_borderline(data = estyear_merge_new %>%
                    filter(Species == "PSME", Fire_Year == 2000),
                  aes(x = years_fire_est, y = cumsum * 0.2, color= Distance_range_m),
                  size = 1, bordercolour = "black",borderwidth = 0.5, show.legend = FALSE) +
  scale_y_continuous(name = "Density (trees/hectare)",
                     sec.axis = sec_axis( ~ . / 0.2, name = "Cumulative Density (trees/hectare)"))


print(PSME_sum_2000)


## Douglas-fir 2006

PSME_sum_2006 = estyear_merge_new %>%
  filter(Species == "PSME", Fire_Year == 2006) %>%
  ggplot(aes(x = years_fire_est, y = sum, fill = Distance_range_m)) +
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = seq(0, 35, by = 2))+
  labs(fill = "Distance to spring (m)",
       x = "Years Since Fire",
       title = "Douglas-fir: 2006",
       color = "Distance to spring (m)")+
  scale_fill_brewer(palette = "Greens", direction =-1) +  # Set fill color using Blues palette
  scale_color_brewer(palette = "Greens", direction =-1)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 10)
  )

PSME_sum_2006 = PSME_cumsum_2006 +
  geom_borderline(data = estyear_merge_new %>%
                    filter(Species == "PSME", Fire_Year == 2006),
                  aes(x = years_fire_est, y = cumsum * 0.8, color= Distance_range_m),
                  size = 1, bordercolour = "black",borderwidth = 0.5, show.legend = FALSE) +
  scale_y_continuous(name = "Density (trees/hectare)",
                     sec.axis = sec_axis( ~ . / 0.8, name = "Cumulative Density (trees/hectare)"))


print(PSME_sum_2006)

#####################

## Lodgepole pine 1988
PICO_sum_1988 = estyear_merge_new %>%
  filter(Species == "PICO", Fire_Year == 1988) %>%
  ggplot(aes(x = years_fire_est, y = sum, fill = Distance_range_m)) +
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = seq(0, 35, by = 2))+
  labs(fill = "Distance to spring (m)",
       x = "Years Since Fire",
       title = "Lodgepole pine: 1988",
       color = "Distance to spring (m)")+
  scale_fill_brewer(palette = "Greens", direction =-1) +  # Set fill color using Blues palette
  scale_color_brewer(palette = "Greens", direction =-1)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 10)
  )

PICO_sum_1988 = PICO_cumsum_1988 +
  geom_borderline(data = estyear_merge_new %>%
                    filter(Species == "PICO", Fire_Year == 1988),
                  aes(x = years_fire_est, y = cumsum * 0.8, color= Distance_range_m),
                  size = 1, bordercolour = "black",borderwidth = 0.5, show.legend = FALSE) +
  scale_y_continuous(name = "Density (trees/hectare)",
                     sec.axis = sec_axis( ~ . / 0.8, name = "Cumulative Density (trees/hectare)"))


print(PICO_sum_1988)


## Lodgepole pine 2000

PICO_sum_2000 = estyear_merge_new %>%
  filter(Species == "PICO", Fire_Year == 2000) %>%
  ggplot(aes(x = years_fire_est, y = sum, fill = Distance_range_m)) +
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = seq(0, 35, by = 2))+
  labs(fill = "Distance to spring (m)",
       x = "Years Since Fire",
       title = "Lodgepole pine: 2000",
       color = "Distance to spring (m)")+
  scale_fill_brewer(palette = "Greens", direction =-1) +  # Set fill color using Blues palette
  scale_color_brewer(palette = "Greens", direction =-1)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11), 
        plot.title = element_text(size = 11),
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 10)
  )

PICO_sum_2000 = PICO_cumsum_2000 +
  geom_borderline(data = estyear_merge_new %>%
                    filter(Species == "PICO", Fire_Year == 2000),
                  aes(x = years_fire_est, y = cumsum * 0.8, color= Distance_range_m),
                  size = 1, bordercolour = "black",borderwidth = 0.5, show.legend = FALSE) +
  scale_y_continuous(name = "Density (trees/hectare)",
                     sec.axis = sec_axis( ~ . / 0.8, name = "Cumulative Density (trees/hectare)"))


print(PICO_sum_2000)


#################################################
## Combine all data together and average density
#################################################

estyear_merge$years_fire_est = estyear_merge$estyear - estyear_merge$Fire_Year

### average density by species, distance bin, and years since fire

estyear_avg = estyear_merge %>%
  group_by(Species, Distance_range_m, years_fire_est) %>%
  summarise(avg_dens = mean(Density), med_dens = median(Density), min_dens = min(Density))

##cumulative avg density
estyear_avg =
  estyear_avg%>%
  group_by(Species, Distance_range_m) %>%
  arrange(years_fire_est) %>% 
  mutate(cumsum_avg = cumsum(avg_dens), cumsum_med = cumsum(med_dens), cumsum_min = cumsum(min_dens))
estyear_avg1 = estyear_avg %>%
  group_by(Species, Distance_range_m) %>%
  summarise(tot_dens= max(cumsum_avg))

estyear_avg = merge(estyear_avg1, estyear_avg, by= c("Species", "Distance_range_m"))
estyear_avg$pct_cumul = (estyear_avg$cumsum_avg/estyear_avg$tot_dens)*100


############################
### Figures for Manuscript
###########################
## Douglas-fir 
avg_PSME_plot = estyear_avg %>%
  filter(Species == "PSME") %>%
  ggplot(aes(x = years_fire_est, y = avg_dens, fill = Distance_range_m)) +
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = seq(0, 35, by = 2))+
  labs(fill = "Distance to spring (m)",
       x = "Years since fire",
       title = "Douglas-fir: all fires",
       color = "Distance to spring (m)")+
  scale_fill_brewer(palette = "Greens", direction =-1) +  # Set fill color using Blues palette
  scale_color_brewer(palette = "Greens", direction =-1)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, family = "serif"), 
        axis.text.y = element_text(size = 10, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"),
        legend.title = element_text(size = 11, family = "serif"), 
        legend.text = element_text(size = 10, family = "serif"),
        legend.position = "bottom",  
        legend.box = "horizontal"
  )


avg_PSME_plot = avg_PSME_plot +
  geom_borderline(data = estyear_avg %>%
                    filter(Species == "PSME"),
                  aes(x = years_fire_est, y = pct_cumul*21, color= Distance_range_m),
                  size = 1, bordercolour = "black",borderwidth = 0.5, show.legend = FALSE) +
  scale_y_continuous(name = "Mean density (trees/hectare)",
                     sec.axis = sec_axis( ~ . / 21, name = "Cumulative total percent of seedlings"))
print(avg_PSME_plot)

#### Lodgepole pine 

avg_PICO_plot = estyear_avg %>%
  filter(Species == "PICO") %>%
  ggplot(aes(x = years_fire_est, y = avg_dens, fill = Distance_range_m)) +
  geom_bar(stat = "identity")+
  scale_x_continuous(breaks = seq(0, 35, by = 2))+
  labs(fill = "Distance to spring (m)",
       x = "Years since fire",
       title = "Lodgepole pine: all fires",
       color = "Distance to spring (m)")+
  scale_fill_brewer(palette = "Greens", direction =-1) +  # Set fill color using Blues palette
  scale_color_brewer(palette = "Greens", direction =-1)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 10, family = "serif"), 
        axis.text.y = element_text(size = 10, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"),
        legend.title = element_text(size = 11, family = "serif"), 
        legend.text = element_text(size = 10, family = "serif"),
        legend.position = "bottom",  
        legend.box = "horizontal"
  )


avg_PICO_plot = avg_PICO_plot +
  geom_borderline(data = estyear_avg %>%
                    filter(Species == "PICO"),
                  aes(x = years_fire_est, y = pct_cumul*30, color= Distance_range_m),
                  size = 1, bordercolour = "black",borderwidth = 0.5, show.legend = FALSE) +
  scale_y_continuous(name = "Mean density (trees/hectare)",
                     sec.axis = sec_axis( ~ . / 30, name = "Cumulative total percent of seedlings"))
print(avg_PICO_plot)



