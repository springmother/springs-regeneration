
############################################
### Project: Conifer Regeneration Study
### Purpose: Height to age model fit and visualization
### Date Created: 10/27/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################

#load libraries
library(ggplot2)

# load data
growth_rate = read.csv("Age_Height.csv")                                        #field measured ages
sitedata = read.csv("Sitedata_FINAL.csv", header = TRUE)                        #site data
age_height <- left_join(growth_rate, sitedata, by = "Spring_Name")              #join dataframes

###########################################
#### PSME - ALL Fire Years ####
PSME_age= age_height %>%
  filter(Species == "PSME")

lm_PSME = lm(Age_yrs ~ log(Height_m), data = PSME_age)
summary(lm_PSME) 
# y = 3.5339*log(Height_m)+11.3801 
# R1 = 79%

Flm0 = fitted(lm_PSME)

ci95 <- predict(lm_PSME, PSME_age, interval = "confidence", level = 0.95)

PSME_age <- cbind(PSME_age, ci95)

ggplot(PSME_age, aes(x = Height_m, y = Age_yrs)) +
  geom_point() +
  geom_line(aes(y = Flm0), color = "blue") +
  geom_line(aes(y = lwr), lty = 2) +
  geom_line(aes(y = upr), lty = 2) +
  xlab("Height (m)")+
  ylab("Age (years)")+
  theme_classic()

## this full model including all fire years really under predicts the older trees
## probably better to split up by fire year

###############################################
            ## Douglas-fir ##
###############################################
PSME_1988 = age_height %>%
  filter(Species == "PSME", Fire_Year == 1988)

ggplot(PSME_1988, aes(x = Height_m, y = Age_yrs))+ # definitely a log relationship
  geom_point()

lm1 = lm(Age_yrs ~ log(Height_m), data = PSME_1988)
summary(lm1) 
sqrt(mean(lm1$residuals^2)) ##RMSE
# y = 3.4759*log(Height_m)+11.4213
# R2 = 87%
# RMSE = 1.54

Flm1 = fitted(lm1)

ci95 <- predict(lm1, PSME_1988, interval = "confidence", level = 0.95)

PSME_1988 <- cbind(PSME_1988, ci95)

g1 = ggplot(PSME_1988, aes(x = Height_m, y = Age_yrs)) +
  geom_point() +
  geom_line(aes(y = Flm1), color = "blue") +
  geom_line(aes(y = lwr), lty = 2) +
  geom_line(aes(y = upr), lty = 2) +
  xlab("Height (m)")+
  ylab("Age (years)")+
  theme_classic()+
  ggtitle("Douglas-fir (1988)")+
  theme(axis.text.x = element_text(size = 11, family = "serif"), 
        axis.text.y = element_text(size = 11, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"))


# Add equation and R-squared to the plot
g1 = g1 +
  geom_text(x = 4, y = 5, label = "y = 3.4759*log(height)+ 11.4213", size = 4, family = "serif")
g1 = g1 +
  geom_text(x = 4, y = 3, label = "R2 = 87%, RMSE = 1.54", size = 4, family = "serif")

################################################################################
## 2000 and 2006 fires were combined because of small sample size in 2006 fire.
PSME_2000_2006 = age_height %>%
  filter(Species == "PSME", Fire_Year == 2000|Fire_Year == 2006)

lm2 = lm(Age_yrs ~ log(Height_m), data = PSME_2000_2006)
summary(lm2) 
sqrt(mean(lm2$residuals^2)) ##RMSE
# y = 3.1333*log(Height_m) + 10.7284
# R2 = 66%
# RMSE = 2.01

Flm2 = fitted(lm2)

ci95 <- predict(lm2, PSME_2000_2006, interval = "confidence", level = 0.95)

PSME_2000_2006 <- cbind(PSME_2000_2006, ci95)

g2 = ggplot(PSME_2000, aes(x = Height_m, y = Age_yrs)) +
  geom_point() +
  geom_line(aes(y = Flm2), color = "blue") +
  geom_line(aes(y = lwr), lty = 2) +
  geom_line(aes(y = upr), lty = 2) +
  xlab("Height (m)")+
  ylab("Age (years)")+
  theme_classic()+
  ggtitle("Douglas-fir (2000 & 2006)")+
  theme(axis.text.x = element_text(size = 11, family = "serif"), 
        axis.text.y = element_text(size = 11, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"))


# Add equation and R-squared to the plot
g2 = g2 +
  geom_text(x = 3, y = 6, label = "y = 3.133*log(height) + 10.7284", size = 4, family = "serif")
g2 = g2 +
  geom_text(x = 4, y = 4, label = "R2 = 66%, RMSE = 2.01", size = 4, family = "serif")


###################################
      ## lodgepole pine ##
###################################
PICO_1988 = age_height %>%
  filter(Species == "PICO", Fire_Year == 1988)

lm4 = lm(Age_yrs ~ log(Height_m), data = PICO_1988)
summary(lm4) 
sqrt(mean(lm4$residuals^2)) ##RMSE
# y = 7.622*log(Height_m)+11.468
# R1 = 89%
# RMSE = 3.2

Flm4 = fitted(lm4)

ci95 <- predict(lm4, PICO_1988, interval = "confidence", level = 0.95)

PICO_1988 <- cbind(PICO_1988, ci95)

g4 = ggplot(PICO_1988, aes(x = Height_m, y = Age_yrs)) +
  geom_point() +
  geom_line(aes(y = Flm4), color = "blue") +
  geom_line(aes(y = lwr), lty = 2) +
  geom_line(aes(y = upr), lty = 2) +
  xlab("Height (m)")+
  ylab("Age (years)")+
  theme_classic()+
  ggtitle("Lodgepole pine (1988)")+
  theme(axis.text.x = element_text(size = 11, family = "serif"), 
        axis.text.y = element_text(size = 11, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"))



# Add equation and R-squared to the plot
g4 = g4 +
  geom_text(x = 7.5, y = 6, label = "y = 7.622*log(height) + 11.468", size = 4, family = "serif")
g4 = g4 +
  geom_text(x = 7.5, y = 2, label = "R2 = 89%, RMSE = 3.2", size = 4, family = "serif")



################################################
PICO_2000 = age_height %>%                         # no PICOs in 2006 fire
  filter(Species == "PICO", !Fire_Year == 1988)

PICO_2000 <- PICO_2000 %>%### There is a 24 year old tree that burned in the Diamond complex which doesn't make sense unless it was a seedling that survived the fire. Will remove
  filter(Age_yrs != 24)

lm5 = lm(Age_yrs ~ log(Height_m), data = PICO_2000)
summary(lm5) 
sqrt(mean(lm5$residuals^2)) ##RMSE
# y =  3.5959*log(Height_m)+10.9901
# R1 = 69%
# RMSE = 2.2

Flm5 = fitted(lm5)

ci95 <- predict(lm5, PICO_2000, interval = "confidence", level = 0.95)

PICO_2000 <- cbind(PICO_2000, ci95)

g5 = ggplot(PICO_2000, aes(x = Height_m, y = Age_yrs)) +
  geom_point() +
  geom_line(aes(y = Flm5), color = "blue") +
  geom_line(aes(y = lwr), lty = 2) +
  geom_line(aes(y = upr), lty = 2) +
  xlab("Height (m)")+
  ylab("Age (years)")+
  theme_classic()+
  ggtitle("Lodgepole pine (2000)")+
  theme(axis.text.x = element_text(size = 11, family = "serif"), 
        axis.text.y = element_text(size = 11, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"))



# Add equation and R-squared to the plot
g5 = g5 +
  geom_text(x = 5, y = 6, label = "y = 3.5959*log(height)+10.9901", size = 4, family = "serif")
g5 = g5 +
  geom_text(x = 5, y = 4, label = "R2 = 69%, RMSE = 2.2", size = 4, family = "serif")



#############################
     ## other species ##
#############################

#### Equations for PIPO and PIEN trees for all fire years combined

other_ha <- age_height %>%
  filter(!(Species %in% c("PSME", "PICO")))

lm_ha = lm(Age_yrs ~ log(Height_m), data = other_ha )
summary(lm_ha) 
sqrt(mean(lm_ha$residuals^2)) ##RMSE
# y =  4.7397*log(Height_m)+9.5786
# R2 = 62%
# RMSE = 2.5
Flm6 = fitted(lm_ha)

ci95 <- predict(lm_ha, other_ha, interval = "confidence", level = 0.95)

other_ha<- cbind(other_ha, ci95)

g6 = ggplot(other_ha, aes(x = Height_m, y = Age_yrs)) +
  geom_point() +
  geom_line(aes(y = Flm6), color = "blue") +
  geom_line(aes(y = lwr), lty = 2) +
  geom_line(aes(y = upr), lty = 2) +
  xlab("Height (m)")+
  ylab("Age (years)")+
  theme_classic()+
  ggtitle("All other species (all fires)")+
  theme(axis.text.x = element_text(size = 11, family = "serif"), 
        axis.text.y = element_text(size = 11, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"))

g6 = g6 +
  geom_text(x = 2, y = 5, label = "y = 4.7397 * log(height) + 9.5786", size = 4, family = "serif")

g6 = g6 +
  geom_text(x = 2, y = 3, label = "R2 = 62%, RMSE = 2.5", size = 4, family = "serif")

### combine into one graph
age_height_models = grid.arrange(arrangeGrob(g1, g2, g4, g5, g6,  ncol = 2))

#############################################################
### Ready for density_time_since_fire.R and age_graphs.R  ###
#############################################################
