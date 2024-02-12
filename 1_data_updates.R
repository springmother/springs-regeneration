############################################
### Project: Conifer Regeneration Study
### Purpose: Initial Data Import and Updates
### Date Created: 8/31/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################
install.packages("dplyr")
library(dplyr)

######### RUN THIS SCRIPT FIRST ################

####################
## 1. Import Data ##
####################
setwd()                                                                         # set working directory

transectdata = read.csv("Transectdata_FINAL.csv", header = TRUE)                # read in transect dataset. 

sitedata = read.csv("Sitedata_FINAL.csv", header = TRUE)                        # bring in site-level data containing co-variates

###################################
## 2. Investigate Data structure ##
###################################

dim(transectdata)                                                                # How many rows and columns?
names(transectdata)                                                              # What are the attribute names?
str(transectdata)  

################################
## 3. Make updates to dataset ##
################################

transectdata$Distance_range_m = factor(transectdata$Distance_range_m)            # set distance bins as factor
transectdata$Spring_Name = factor(transectdata$Spring_Name)                      # set spring name as factor

transectdata$Area_ha = transectdata$Area_m2/10000                                # create new column area_ha and convert from m2 to hectare

# number of transects per spring
transectdata = transectdata %>%
  group_by(Spring_Name) %>%
  mutate(Transect_count = n_distinct(Transect_ID)) %>%
  ungroup()                                                                      # adds number of transects per spring


# convert elevation to meters
transectdata$elevation_m = transectdata$Elevation_ft/3.281


# identify missing values
colSums(is.na(transectdata))                                                    #Identify if there are missing values, and where they are located

transectdata$Average_dist_to_live_tree[is.na(transectdata$Average_dist_to_live_tree)] <- 500 # Populate missing distance to live tree column with default value of 500.
                                                                                              # Missing values indicate there were no trees in sight.  


#############################################################
## 4. Calculate post-fire climate variables at each spring ##
#############################################################

## upload prism data extracted over a 4 km grid cell for each spring site
prism_clim_springs = read.csv("C:/Users/gpeven/OneDrive - University of Idaho/Springs Research/Data/Climate/PRISM/PRISM_climate_springs.csv")
prism_clim_springs$Spring_Name = prism_clim_springs$Name

# merge with site data to add fire year
prism_clim_springs <- left_join(prism_clim_springs, sitedata %>% select(Spring_Name, Fire_Year), by = "Spring_Name")


# filter to relevant months (March - August)
prism_clim_springs <- prism_clim_springs %>%
  filter(Month %in% c(03,04,05,06,07,08))

# Group by "Year" and calculate the average seasonal climate vars from March - August
prism_clim_springs_byyear<- prism_clim_springs  %>%
  group_by(Year, Fire_Year, Spring_Name) %>%
  summarise(Avg_tmin_degC = mean(tmin_C), Avg_tmax_degC = mean(tmax_C), sum_precip = sum(ppt_mm))


### Calculate 5 year averages

# 2000 fire
avg_5_yrs_2000 <- prism_clim_springs_byyear %>%
  filter(Year >= 2001 & Year <= 2005) %>%
  group_by(Spring_Name, Fire_Year) %>%
  summarize(avg_sum_precip_5yr = mean(sum_precip), Avg_tmax_degC_5yr = mean(Avg_tmax_degC), Avg_tmin_degC_5yr = mean(Avg_tmin_degC))

avg_5_yrs_2000 = 
  avg_5_yrs_2000 %>%
  filter(Fire_Year == 2000)


# 1988 fire
avg_5_yrs_1988 <- prism_clim_springs_byyear %>%
  filter(Year >= 1989 & Year <= 1993) %>%
  group_by(Spring_Name, Fire_Year) %>%
  summarize(avg_sum_precip_5yr = mean(sum_precip), Avg_tmax_degC_5yr = mean(Avg_tmax_degC), Avg_tmin_degC_5yr = mean(Avg_tmin_degC))

avg_5_yrs_1988= 
  avg_5_yrs_1988 %>%
  filter(Fire_Year == 1988)


# 2006 fire
avg_5_yrs_2006 <- prism_clim_springs_byyear %>%
  filter(Year >= 2007 & Year <= 2011) %>%
  group_by(Spring_Name, Fire_Year) %>%
  summarize(avg_sum_precip_5yr = mean(sum_precip), Avg_tmax_degC_5yr = mean(Avg_tmax_degC), Avg_tmin_degC_5yr = mean(Avg_tmin_degC))

avg_5_yrs_2006= 
  avg_5_yrs_2006 %>%
  filter(Fire_Year == 2006)

## bind data together
springs_clim <- rbind(avg_5_yrs_1988, avg_5_yrs_2000, avg_5_yrs_2006)

sitedata = left_join(springs_clim, sitedata, by = "Spring_Name")

## double Fire Years, make updates to avoid duplicates
sitedata <- sitedata %>% select(-Fire_Year.y)
sitedata  <- sitedata  %>%
  rename(Fire_Year = Fire_Year.x)

##############################
## 5. Join all data together
##############################
transectdata <- left_join(transectdata, sitedata, by = "Spring_Name")            # merge site level data to transect data by spring name

transectdata  <- transectdata  %>% filter(!Transect_ID == "")                    # some blank rows were imported so this removes them

transectdata <- transectdata %>% filter(Spring_Name != "Game Knob")              # delete the spring that burned at moderate severity

#######################################################
## 6. Add in Heat Load Index for spring and transect
#######################################################

#McCune and Keon, 2002:
#"(1-cos(aspect_deg - 45))/2"; index of 0 - 1. 0 = coolest, 1 = hottest

## transect level
#################
# the average aspect (degrees) across each transect was derived from a 10-meter 
# DEM in ArcGIS using the extract surface information tool.


transect_aspect = read.csv("transects_aspect.csv")                         
transect_aspect$Aspect_deg = round(transect_aspect$Z_Mean)
transect_aspect$aspect_rad = transect_aspect$Aspect_deg * (pi/180)
transect_aspect$HLI_transect = (1-cos(transect_aspect$aspect_rad - 45))/2

transect_aspect$Transect_ID = transect_aspect$Name
transectdata <- left_join(transectdata, transect_aspect %>% select(Transect_ID, HLI_transect), by = "Transect_ID") ### Add to transectdata table

## spring level
##################
transectdata$aspect_rad = transectdata$Aspect * (pi/180)
transectdata$HLI_spring = (1-cos(transectdata$aspect_rad - 45))/2


###############################################
## 7. Create new dataframe with density data ##
###############################################
# we need to do this to add up all the trees per species per distance bin and transect

tree_counts = 
  transectdata %>%
  group_by(Distance_range_m, Distance_rank, Species, Spring_Name, Latitude, Longitude,
           Spring_Type, Transect_ID, Forest_Type, Area_m2, Area_ha, Average_dist_to_live_tree, 
           elevation_m, HLI_spring, HLI_transect, Transect_Slope_deg, Slope, 
           Years_Since_Fire, Fire_Year, Transect_count, Avg_tmin_degC_5yr, Avg_tmax_degC_5yr, avg_sum_precip_5yr) %>%
  summarize(Tree_Count = sum(Count))

tree_counts$Density = tree_counts$Tree_Count/tree_counts$Area_ha                   ## add density data (trees/ha)
View(tree_counts)

## note: Distance_rank = the mid-point distance of each distance bin. see manuscript
##       for more details.

#####################################
## 8. Apply Age equations to all data ##
####################################
## code from age_height_model.R
#1. subset data by species and fire year
#2. recombine all data into one table. 

# PSME 1988
PSME_1988_ha = transectdata %>%
  filter(Species == "PSME", Fire_Year == 1988)

PSME_1988_ha$Age_new = round(3.4759*log(PSME_1988_ha$Height_m)+11.4213) 


# PSME 2000 & 2006
PSME_2000_ha = transectdata %>%                         
  filter(Species == "PSME", Fire_Year == 2000| Fire_Year == 2006)

PSME_2000_ha$Age_new = round(3.1333*log(PSME_2000_ha$Height_m) + 10.7284) 


# PICO 1988
PICO_1988_ha = transectdata %>%                         
  filter(Species == "PICO", Fire_Year == 1988)

PICO_1988_ha$Age_new = round(7.622*log(PICO_1988_ha$Height_m)+11.468) 

# PICO 2000
PICO_2000_ha = transectdata %>%                         
  filter(Species == "PICO", Fire_Year == 2000)

PICO_2000_ha$Age_new = round(3.5959*log(PICO_2000_ha$Height_m)+10.9901) 


# Other Species: Apply general model to other species left
# PIPO and PIEN
other_ha <- transectdata %>%
  filter(!(Species %in% c("PSME", "PICO")))

other_ha$Age_new = round(4.7397*log(other_ha$Height_m)+9.5786)

# combine all

transectdata = rbind(PSME_1988_ha, PSME_2000_ha, PICO_1988_ha, PICO_2000_ha, other_ha)

## how many trees were negative from age models?
print(100 * sum(transectdata$Age_new < 1, na.rm = TRUE) / sum(!is.na(transectdata$Age_new)), digits = 2)

## force negative values to be 1
transectdata$Age_new[transectdata$Age_new < 0] <- 1



#####################################################
## 9. Final dataset updates before moving to analysis 
#####################################################

### Make sure these are factors
tree_counts$Distance_range_m = factor(tree_counts$Distance_range_m )
tree_counts$Fire_Year = factor(tree_counts$Fire_Year )
tree_counts$Spring_Name = factor(tree_counts$Spring_Name )
tree_counts$Transect_ID = factor(tree_counts$Transect_ID )


## need to standardize continous co-variates to all have comparable values 
## in model results. This doesn't change the value of the variables, just the scale.

MyStd <- function(x) { (x - mean(x)) / sd(x)}

tree_counts$Average_dist_to_live_tree.c <- MyStd(tree_counts$Average_dist_to_live_tree)
tree_counts$Slope.c  <- MyStd(tree_counts$Slope)
tree_counts$HLI_spring.c   <- MyStd(tree_counts$HLI_spring)
tree_counts$HLI_transect.c   <- MyStd(tree_counts$HLI_transect)
tree_counts$Transect_Slope_deg.c   <- MyStd(tree_counts$Transect_Slope_deg)
tree_counts$Years_Since_Fire.c   <- MyStd(tree_counts$Years_Since_Fire)
tree_counts$elevation_m.c   <- MyStd(tree_counts$elevation_m)
tree_counts$Distance_rank.c <-MyStd(tree_counts$Distance_rank)
tree_counts$Avg_tmin_degC_5yr.c <-MyStd(tree_counts$Avg_tmin_degC_5yr)
tree_counts$Avg_tmax_degC_5yr.c <-MyStd(tree_counts$Avg_tmax_degC_5yr)
tree_counts$avg_sum_precip_5yr.c <-MyStd(tree_counts$avg_sum_precip_5yr)

##################################
#### READY FOR OTHER SCRIPTS #####
##################################



