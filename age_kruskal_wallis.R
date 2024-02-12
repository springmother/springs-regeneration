
############################################
### Project: Conifer Regeneration Study
### Purpose: Kruskal-Wallis and Pairwise Tests
### Date Created: 10/30/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################

# load libraries
library(pgirmess)
library(dplyr)
library(dunn.test)

###########################
### Kruskal-Wallis Tests ###
###########################

#####################
#### Douglas-fir ####
#####################
shapiro.test(transectdata$Age_new) ### significantly different than normal

## 1988 Fire
PSME_age_1988 <- transectdata %>%
  filter(Species == "PSME", Fire_Year == 1988)

age_PSME_1988 <- kruskal.test(Age_new ~ Distance_range_m, data = PSME_age_1988)
print(age_PSME_1988)

pairwise_age_1988 <- kruskalmc(Age_new~Distance_range_m, alpha = 0.05, data = PSME_age_1988)
print(pairwise_age_1988)
boxplot(Age_new~Distance_range_m, data = PSME_age_1988)

### 2000 Fire
PSME_age_2000 <- transectdata %>%
  filter(Species == "PSME", Fire_Year == 2000)

age_PSME_2000  <- kruskal.test(Age_new ~ Distance_range_m, data = PSME_age_2000 )
print(age_PSME_2000 )

pairwise_age_2000  <- kruskalmc(Age_new~Distance_range_m, alpha = 0.05, data = PSME_age_2000 )
print(pairwise_age_2000 )
boxplot(Age_new~Distance_range_m, data = PSME_age_2000)
dunn.test(PSME_age_2000$Age_new, PSME_age_2000$Distance_range_m, method = "holm")

## 2006 Fire
PSME_age_2006 <- transectdata %>%
  filter(Species == "PSME", Fire_Year == 2006)

age_PSME_2006 <- kruskal.test(Age_new ~ Distance_range_m, data = PSME_age_2006)
print(age_PSME_2006)

pairwise_age_2006 <- kruskalmc(Age_new~Distance_range_m, alpha = 0.05, data = PSME_age_2006)
print(pairwise_age_2006)
boxplot(Age_calc~Distance_range_m, data = PSME_age_2006)
dunn.test(PSME_age_2006$Age_new, PSME_age_2006$Distance_range_m, method = "holm")

########################
#### lodgepole pine ####
########################

## 1988 Fire
PICO_age_1988 <- transectdata %>%
  filter(Species == "PICO", Fire_Year == 1988)

age_PICO_1988 <- kruskal.test(Age_new ~ Distance_range_m, data = PICO_age_1988)
print(age_PICO_1988)

pairwise_age_1988 <- kruskalmc(Age_new~Distance_range_m, alpha = 0.05, data = PICO_age_1988)
print(pairwise_age_1988)
dunn.test(PICO_age_1988$Age_new, PICO_age_1988$Distance_range_m, method = "holm")
boxplot(Age_new~Distance_range_m, data = PICO_age_1988)

### 2000 Fire
PICO_age_2000 <- transectdata %>%
  filter(Species == "PICO", Fire_Year == 2000)

age_PICO_2000  <- kruskal.test(Age_new ~ Distance_range_m, data = PICO_age_2000 )
print(age_PICO_2000 )

pairwise_age_2000  <- kruskalmc(Age_new~Distance_range_m, alpha = 0.05, data = PICO_age_2000 )
print(pairwise_age_2000 )
boxplot(Age_new~Distance_range_m, data = PICO_age_2000)

