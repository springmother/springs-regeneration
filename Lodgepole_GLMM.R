###################################################################################
### Project: Conifer Regeneration Study
### Purpose: Lodgepole pine Density GLMM
### Date Created: 11/30/2023
### by: Grace Peven, gpeven@uidaho.edu
##################################################################################


# load libraries
library(lme4)
library(ggplot2)
library(dplyr)
require(glmmTMB)
require(DHARMa)

###########################################
### RUN 1_data_updates.R FIRST ###
###########################################

# load libraries
library(lme4)
library(ggplot2)
library(dplyr)
require(glmmTMB)
require(DHARMa)
library(performance)

###########################
#### Response Variable ####
###########################

# Tree Count

#####################
#### Covariates ####
####################

# 1. Distance to spring (continous)
# 2. Elevation (continous)
# 3. Site Slope (continuous)
# 4. Transect Slope (continous)
# 5. HLI Spring
# 6. HLI transect
# 7. 5 year post-fire precipitation
# 8. 5 year post-fire max temp


#######################
### Lodgepole pine ###
######################
PICO_counts = 
  tree_counts %>%
  filter(Species == "PICO")

#################################

##########################################
####  Poisson models #####
##########################################

M1 <- glmer(Tree_Count ~ Distance_rank.c +
              Transect_Slope_deg.c+
              HLI_spring.c+
              offset(log(Area_m2))+ 
              (1|Transect_ID), 
            family = poisson, 
            data = PICO_counts) 

check_overdispersion(M1) # dispersion stat = 1.8
summary(M1)

M2 <- glmer(Tree_Count ~ Distance_rank.c +
             elevation_m.c+
              offset(log(Area_m2))+ 
              (1|Transect_ID), 
            family = poisson, 
            data = PICO_counts) 

check_overdispersion(M2) # dispersion stat = 1.8
summary(M2)

## try nbinom models to handle overdispersion

##########################################
####  Negative Binomial models #####
##########################################


### try negative binomial with non-collinear vars
M3_PICO_nb = glmmTMB(Tree_Count ~ Distance_rank.c+
                       HLI_transect.c+
                       Transect_Slope_deg.c+
                       offset(log(Area_m2))+
                       (1 | Spring_Name/Transect_ID),
                     family = nbinom2, 
                     data = PICO_counts)
summary(M3_PICO_nb)
check_singularity(M3_PICO_nb)

AIC(M3_PICO_poi, M3_PICO_nb) ## negative binomial has much better fit


## try interaction effect
M4_PICO_nb = glmmTMB(Tree_Count ~ Distance_rank.c*
                       HLI_transect.c+
                       Transect_Slope_deg.c+
                       offset(log(Area_m2))+
                       (1 | Spring_Name/Transect_ID),
                     family = nbinom2, 
                     data = PICO_counts)
summary(M4_PICO_nb)
# no sig. interaction effect


M5_PICO_elevation = glmmTMB(Tree_Count ~ Distance_rank.c+
                            elevation_m.c+ 
                            offset(log(Area_m2))+
                            (1 | Spring_Name/Transect_ID),
                          family = nbinom2, 
                          data = PICO_counts)
summary(M5_PICO_elevation)


M6_PICO_precip = glmmTMB(Tree_Count ~ Distance_rank.c+
                            avg_sum_precip_5yr.c+
                            offset(log(Area_m2))+
                            (1 | Spring_Name/Transect_ID),
                          family = nbinom2, 
                          data = PICO_counts)
summary(M6_PICO_precip)

M7_PICO_tmax = glmmTMB(Tree_Count ~ Distance_rank.c+
                                    Avg_tmax_degC_5yr.c+
                                  offset(log(Area_m2))+
                                  (1 | Spring_Name/Transect_ID),
                                family = nbinom2, 
                                data = PICO_counts)
summary(M7_PICO_tmax)
 
## Significant relationships to climate for PICO

AIC(M3_PICO_nb, M4_PICO_nb, M5_PICO_elevation, M6_PICO_precip, M7_PICO_tmax)

# models M5 - M7 have best fit.

##############################
### BEST FITTING MODELS ###
##############################
# singularity when i have both spring and transect as RE, but not singular with
# just transect or just spring. 

### compare to decide whether to use spring or transect ID as random effect
Final_PICO_elev_spring= glmmTMB(Tree_Count ~ Distance_rank.c+
                      elevation_m.c+ 
                      offset(log(Area_m2))+
                      (1|Spring_Name),
                    family = nbinom2, 
                    data = PICO_counts)

Final_PICO_elev= glmmTMB(Tree_Count ~ Distance_rank.c+
                           elevation_m.c+ 
                           offset(log(Area_m2))+
                           (1|Transect_ID),
                         family = nbinom2, 
                         data = PICO_counts)

performance(Final_PICO_elev)### much better fit model. ICC is 0.25 compared to 0.09 in spring RE structure
performance(Final_PICO_elev_spring) 
summary(Final_PICO_elev)
check_overdispersion(Final_PICO_elev)
check_singularity(Final_PICO_elev)
check_collinearity(Final_PICO_elev)
r2_nakagawa(Final_PICO_elev)

confint(Final_PICO_elev)

## precip
Final_PICO_precip= glmmTMB(Tree_Count ~ Distance_rank.c+
                          avg_sum_precip_5yr.c+ 
                           offset(log(Area_m2))+
                           (1 | Transect_ID),
                         family = nbinom2, 
                         data = PICO_counts)

performance(Final_PICO_precip)
confint(Final_PICO_precip)
summary(Final_PICO_precip)
check_overdispersion(Final_PICO_precip)
check_collinearity(Final_PICO_precip)
r2_nakagawa(Final_PICO_precip) 

## tmax
Final_PICO_tmax= glmmTMB(Tree_Count ~ Distance_rank.c+
                             Avg_tmax_degC_5yr.c+ 
                             offset(log(Area_m2))+
                             (1 | Transect_ID),
                           family = nbinom2, 
                           data = PICO_counts)

summary(Final_PICO_tmax)
check_overdispersion(Final_PICO_tmax)
check_collinearity(Final_PICO_tmax)
r2_nakagawa(Final_PICO_tmax) 
confint(Final_PICO_tmax)

## slope and hli

Final_PICO_slope_hli = glmmTMB(Tree_Count ~ Distance_rank.c+
                             Transect_Slope_deg.c+
                             HLI_spring.c+
                             offset(log(Area_m2))+
                             (1 |Transect_ID),
                           family = nbinom2, 
                           data = PICO_counts)
summary(Final_PICO_slope_hli)
performance(Final_PICO_slope_hli)
check_collinearity(Final_PICO_slope_hli)
confint(Final_PICO_slope_hli)

AIC(Final_PICO_elev, Final_PICO_precip, Final_PICO_slope_hli, Final_PICO_tmax) ### Final_PICO_elev and Final_PICO_slope_hli have best fit

performance(Final_PICO_elev)

######################################################
####  MODEL VALIDATION 
######################################################

### Final_PICO_elev###
#############################

#1. create DHARMa object of residuals
simulationOutput <- simulateResiduals(fittedModel = Final_PICO_elev)

#2. Plot scaled residuals
plot(simulationOutput)
# no overdispersion, distribution is correct, res vs. predicted looks good with slight decreasing trend.

#3. Plot residuals against all predictors
### Residual issues 
plotResiduals(simulationOutput, PICO_counts$Distance_rank.c)
plotResiduals(simulationOutput, PICO_counts$Average_dist_to_live_tree.c) # slight quantile dev. but not alarming
plotResiduals(simulationOutput, PICO_counts$HLI_spring.c)   # slight quantile dev. but not alarming
plotResiduals(simulationOutput, PICO_counts$HLI_transect.c) 
plotResiduals(simulationOutput, PICO_counts$elevation_m.c) # some quantile deviance detected
plotResiduals(simulationOutput, PICO_counts$Years_since_fire.c)
plotResiduals(simulationOutput, PICO_counts$Avg_SPEI_5yr.c) 
plotResiduals(simulationOutput, PICO_counts$Avg_tmin_degC_5yr.c) 
plotResiduals(simulationOutput, PICO_counts$Avg_tmax_degC_5yr.c) 
plotResiduals(simulationOutput, PICO_counts$avg_sum_precip_5yr.c)
#4. Goodness-of-fit tests

testUniformity(simulationOutput) # no problems
testDispersion(simulationOutput) # no problems
testOutliers(simulationOutput)   # no problems
testZeroInflation(simulationOutput) # looks good, don't need to use ZI


# calculating x, y positions per Spring_Name
groupLocations = aggregate(PICO_counts, list(PICO_counts$Spring_Name), mean)

# calculating residuals per group
res2 = recalculateResiduals(simulationOutput, group = PICO_counts$Spring_Name)
# running the spatial test on grouped residuals
testSpatialAutocorrelation(res2, x=groupLocations$Longitude, y=groupLocations$Latitude)

# no issues with spatial autocorrelation



