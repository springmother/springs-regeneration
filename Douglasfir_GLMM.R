###################################################################################
### Project: Conifer Regeneration Study
### Purpose: Douglas-fir Density GLMM
### Date Created: 12/6/2023
### by: Grace Peven, gpeven@uidaho.edu
##################################################################################

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
# 2. Distance to seed source (continuous)
# 3. Site Slope (continuous)
# 4. Transect Slope (continous)
# 5. HLI Spring
# 6. HLI transect
# 7. 5 year post-fire precipitation
# 8. 5 year post-fire max temp

########################
#### Random effects ####
########################

# Spring_Name           ## There are multiple observations/transects per spring
                        ## so we assign Spring Name as a random effect

# Transect ID           ## Since there are repeat sub-transect level measurements (distance bins) 

#########################




###########################
### Douglas-fir model ###
###########################
PSME_counts = 
  tree_counts %>%
  filter(Species == "PSME")


# Model set-up plan: 
#### 1. try Poisson distribution first (test for overdispersion)
#### 2. then try negative binomial if overdispersed
#### 3. then try zero-inflated to see if this improves model fit

##########################
### start with Poisson ###
##########################

# will need to iterate through several models since there is collinearity among
# predictor variables. see 2_density_data_exploration.R

###################
## distance midpoint and years since fire
##################

########################
### Distance midpoint and precip
#########################

M1 = glmer(Tree_Count ~ Distance_rank.c+
                  Average_dist_to_live_tree.c +
              HLI_transect.c+
                  avg_sum_precip_5yr.c +
                  offset(log(Area_m2))+ 
                  (1 | Spring_Name/Transect_ID), 
                family = poisson, 
                data = PSME_counts)

summary(M2)
check_overdispersion(M2)
# dispersion stat: 2.07
############
## HLI transect v. HLI spring?
M1a = glmer(Tree_Count ~ Distance_rank.c+
             Average_dist_to_live_tree.c +
             HLI_spring.c+
             avg_sum_precip_5yr.c +
             offset(log(Area_m2))+ 
             (1 | Spring_Name/Transect_ID), 
           family = poisson, 
           data = PSME_counts)
AIC(M1, M1a)
## HLI transect much better fit

###################
## Distance midpoint and max temp
##################
M2 = glmer(Tree_Count ~ Distance_rank.c+
             Average_dist_to_live_tree.c +
             HLI_transect.c+
             Avg_tmax_degC_5yr.c +
             offset(log(Area_m2))+ 
             (1 | Spring_Name/Transect_ID), 
           family = poisson, 
           data = PSME_counts)

summary(M3)
check_overdispersion(M3)
# dispersion stat: 2.07

AIC(M1, M2) # slightly better fir for precip

### Try ZIP models to see if this handles overdispersion ###

M3z = glmmTMB(Tree_Count ~ Distance_rank.c+
             Average_dist_to_live_tree.c +
             HLI_transect.c+
             Avg_tmax_degC_5yr.c +
             offset(log(Area_m2))+ 
             (1 | Spring_Name/Transect_ID),
            ziformula = ~1,
           family = poisson, 
           data = PSME_counts)

summary(M4z)
check_overdispersion(M3z)
# dispersion stat: 0.97

M3z_precip = glmmTMB(Tree_Count ~ Distance_range_m+
                Average_dist_to_live_tree.c +
                HLI_transect.c+
                avg_sum_precip_5yr.c +
                offset(log(Area_m2))+ 
                (1 | Spring_Name/Transect_ID),
              ziformula = ~1,
              family = poisson, 
              data = PSME_counts)

summary(M4z_precip)

AIC(M1, M2,  M3z, M3z_precip) ##ZI with max temp has best fit now
################################################################################
####################### Negative Binomial Models ###############################
################################################################################

### fitting full models with non-collinear vars and then comparing 

M_nb1 = glmmTMB(Tree_Count ~ Distance_rank.c + 
                   Average_dist_to_live_tree.c +
                   Years_Since_Fire.c+
                   HLI_transect.c+
                   Transect_Slope_deg.c+
                   offset(log(Area_m2))+
                   (1 | Spring_Name/Transect_ID),
                 family = nbinom2, 
                 data = PSME_counts)
summary(M_nb1)
# try HLI Spring
M_nb2 = glmmTMB(Tree_Count ~ Distance_rank.c + 
                   Average_dist_to_live_tree.c +
                   Years_Since_Fire.c+
                   HLI_spring.c+
                   Transect_Slope_deg.c+
                   offset(log(Area_m2))+
                   (1 | Spring_Name/Transect_ID),
                 family = nbinom2, 
                 data = PSME_counts)
summary(M_nb2)

AIC(M_nb1, M_nb2) #HLI Transect is still better

# try precip
M_nb3 = glmmTMB(Tree_Count ~ Distance_rank.c + 
                    Average_dist_to_live_tree.c +
                    avg_sum_precip_5yr.c+
                    Years_Since_Fire.c+
                    HLI_transect.c+
                    Transect_Slope_deg.c+
                    offset(log(Area_m2))+
                    (1 | Spring_Name/Transect_ID),
                  family = nbinom2, 
                  data = PSME_counts)
summary(M_nb3)

AIC(M_nb1, M_nb3) ## no precip is better fit but I'm interested in keeping precip
                  ## to compare effects 

# transect slope and tmax are collinear, so have to exclude t-slope
M_nb4 = glmmTMB(Tree_Count ~ Distance_rank.c + 
                    Average_dist_to_live_tree.c +
                   Avg_tmax_degC_5yr.c+
                    Years_Since_Fire.c+
                    HLI_transect.c+
                    offset(log(Area_m2))+
                    (1 | Spring_Name/Transect_ID),
                  family = nbinom2, 
                  data = PSME_counts)
summary(M_nb4)

AIC(M_nb3,M_nb4) # <2 difference. let's keep both to compare tmax and precip

#################################################################
############# Final Reduced Models to Validate ##################
#################################################################

PSME_Final_precip = glmmTMB(Tree_Count ~ Distance_rank.c+
                                 HLI_transect.c+ 
                              Average_dist_to_live_tree.c +
                              avg_sum_precip_5yr.c+
                              offset(log(Area_m2))+
                              (1 | Spring_Name/Transect_ID),
                            family = nbinom2, 
                            data = PSME_counts)
summary(PSME_Final_precip)
confint(PSME_Final_precip)


#slope 

PSME_Final_slope = glmmTMB(Tree_Count ~ Distance_rank.c+
                             HLI_transect.c+ 
                              Average_dist_to_live_tree.c +
                              Transect_Slope_deg.c+
                              offset(log(Area_m2))+
                              (1 | Spring_Name/Transect_ID),
                            family = nbinom2, 
                            data = PSME_counts)
summary(PSME_Final_slope)
performance(PSME_Final_slope)
confint(PSME_Final_slope)

# t max
PSME_Final_tmax = glmmTMB(Tree_Count ~ Distance_rank.c+
                            HLI_transect.c+ 
                  Average_dist_to_live_tree.c +
                    Avg_tmax_degC_5yr.c+
                  offset(log(Area_m2))+
                  (1 | Spring_Name/Transect_ID),
                family = nbinom2, 
                data = PSME_counts)


summary(PSME_Final_tmax)
performance(PSME_Final_tmax)
confint(PSME_Final_tmax)

AIC(PSME_Final_precip, PSME_Final_tmax, PSME_Final_slope) ## PSME_Final_slope has best fit

#########################
## Model Validation
#########################
# quick validation/checks
check_zeroinflation(PSME_Final_slope)
check_overdispersion(PSME_Final_slope)
check_collinearity(PSME_Final_slope)
check_singularity(PSME_Final_slope)
r2_nakagawa(PSME_Final_slope) 

############################################
### Model Validation with DHARMa ###
###########################################
# nice DHARMa tutorial: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

#1. create DHARMa object of residuals
simulationOutput <- simulateResiduals(fittedModel = PSME_Final_slope)

#2. Plot scaled residuals
plot(simulationOutput)

# no overdispersion, distribution is correct, res vs. predicted looks good with slight decreasing trend.

#3. Plot residuals against all predictors
### Residual issues 
plotResiduals(simulationOutput, PSME_counts$Distance_rank.c)
plotResiduals(simulationOutput, PSME_counts$Average_dist_to_live_tree.c)
plotResiduals(simulationOutput, PSME_counts$HLI_spring.c)  
plotResiduals(simulationOutput, PSME_counts$HLI_transect.c) 
plotResiduals(simulationOutput, PSME_counts$elevation_m.c) # some quantile deviance detected
plotResiduals(simulationOutput, PSME_counts$Years_since_fire.c)
plotResiduals(simulationOutput, PSME_counts$Avg_tmin_degC_5yr.c) # why are these exactly the same
plotResiduals(simulationOutput, PSME_counts$Avg_tmax_degC_5yr.c) # why are these exactly the same
plotResiduals(simulationOutput, PSME_counts$avg_sum_precip_5yr.c)
plotResiduals(simulationOutput, PSME_counts$Transect_Slope_deg.c)
#4. Goodness-of-fit tests

testUniformity(simulationOutput) # no problems
testDispersion(simulationOutput) # no problems
testOutliers(simulationOutput)   # no problems
testZeroInflation(simulationOutput) # looks good, don't need to use ZI


# calculating x, y positions per Spring_Name
groupLocations = aggregate(PSME_counts, list(PSME_counts$Spring_Name), mean)

# calculating residuals per group
res2 = recalculateResiduals(simulationOutput, group = PSME_counts$Spring_Name)
# running the spatial test on grouped residuals
testSpatialAutocorrelation(res2, x=groupLocations$Longitude, y=groupLocations$Latitude)

# no issues with spatial autocorrelation



##############################
### PSME_Final_tmax ###
#############################

#1. create DHARMa object of residuals
simulationOutput <- simulateResiduals(fittedModel = PSME_Final_tmax)

#2. Plot scaled residuals
plot(simulationOutput)
# no overdispersion, distribution is correct, res vs. predicted looks good with slight decreasing trend.

#3. Plot residuals against all predictors
### Residual issues 
plotResiduals(simulationOutput, PSME_counts$Distance_rank.c)
plotResiduals(simulationOutput, PSME_counts$Average_dist_to_live_tree.c)
plotResiduals(simulationOutput, PSME_counts$HLI_spring.c)  
plotResiduals(simulationOutput, PSME_counts$HLI_transect.c) 
plotResiduals(simulationOutput, PSME_counts$elevation_m.c) # some quantile deviance detected
plotResiduals(simulationOutput, PSME_counts$Years_since_fire.c)
plotResiduals(simulationOutput, PSME_counts$Avg_SPEI_5yr.c) 
plotResiduals(simulationOutput, PSME_counts$Avg_tmin_degC_5yr.c) # why are these exactly the same
plotResiduals(simulationOutput, PSME_counts$Avg_tmax_degC_5yr.c) # why are these exactly the same
plotResiduals(simulationOutput, PSME_counts$avg_sum_precip_5yr.c)
#4. Goodness-of-fit tests

testUniformity(simulationOutput) # no problems
testDispersion(simulationOutput) # no problems
testOutliers(simulationOutput)   # no problems
testZeroInflation(simulationOutput) # looks good, don't need to use ZI


# calculating x, y positions per Spring_Name
groupLocations = aggregate(PSME_counts, list(PSME_counts$Spring_Name), mean)

# calculating residuals per group
res2 = recalculateResiduals(simulationOutput, group = PSME_counts$Spring_Name)
# running the spatial test on grouped residuals
testSpatialAutocorrelation(res2, x=groupLocations$Longitude, y=groupLocations$Latitude)

# no issues with spatial autocorrelation

