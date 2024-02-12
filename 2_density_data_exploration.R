############################################
### Project: Conifer Regeneration Study
### Purpose: Initial data exploration including collinearity
### Date Created: 8/31/2023
### by: Grace Peven, gpeven@uidaho.edu
############################################

# load libraries
library(ggplot2)
library(dplyr)
require(corrplot)
require(RColorBrewer)
source("C:/Users/gpeven/OneDrive - University of Idaho/Coursework/Fall 2023/GLMM_highlandstats/AllData_OnlineGLMMFreq/AllData/HighstatLibV13.R")
# above package is from Highland Stats file with custom functions

##############################
#### Description of Study ####
##############################

#This study aims to quantify spring influence on post-fire conifer 
#regeneration in high severity burned areas in the Northern Rocky Mountains. 

############################
#### Research Questions ####
############################

#(1) Are there differences between tree density and age with increasing distance from spring? 
#(2) Is distance from spring a significant predictor of tree density? 


###########################
#### Response Variables ####
###########################

# Tree Count


#####################
#### Covariates ####
####################

# 1. Distance to spring (categorical)
# 2. Distance to seed source (continuous)
# 2. Transect Slope (continuous)
# 3. Elevation (continuous)
# 4. Understory vegetation density (continuous)
# 5. Time since fire?
# 6. Forest Type 


################################################
############### Data Exploration ###############
################################################


###############################################
## 1. How many zeroes for response variable? ##
###############################################
                                                        
print(100 * sum(tree_counts$Tree_Count == 0) / nrow(tree_counts), digits = 2)       # percentage of zeroes in Density column

### tells me that 54% of data contains zeroes.

###########################################################
## 2. Create Histogram of Data to determine distribution ##
##########################################################

ggplot(tree_counts, aes(x = Tree_Count)) +
  geom_histogram(stat="count") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Tree Count") +
  ggtitle("Tree Count across all springs and species")


## since this is count data, it will be poisson or negative binomial. 
## Data is heavily concentrated around zero, which is not surprising since we 
## calculated percentage of zeroes in the last step. 

# we can do more tests to determine the likely probability distribution of 
# our response variable
library(moments)
kurtosis(tree_counts$Tree_Count) # kurtosis = 27, meaning the data are more likely to have outliers
                                 # and most data are around the tails and not around the mean
skewness(tree_counts$Tree_Count) # skewness = 4.1 , meaning the data are extremely skewed


# to determine the exact distribution (i.e., whether poisson or
# negative binomial) will be the best fit, we will make multiple models and compare
# them in another script (PSME_Density_GLMM.R and PICO_Density_GLMM.R)

##############################################
## 3. Explore outliers in response variable ##
##############################################
# R function below provided by Highland Statistics Ltd.
#Mixed effects models and extensions in ecology with R. (2009).
#Zuur, AF, Ieno, EN, Walker, N, Saveliev, AA, and Smith, GM. Springer.
Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)),
               groups=FALSE,
               strip = strip.custom(bg = 'white',
                                    par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE),
                             y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))
  
  print(P)  
}

# look for outliers in the response variables
Mydotplot(tree_counts$Tree_Count)
# there are a few data points >30 that could be considered an outlier, but 
# it is not a good idea to remove this point since it represents real data


# look for outliers in the covariates
MyVar <- c("Distance_rank","Transect_Slope_deg", "Average_dist_to_live_tree", "elevation_m", "HLI_spring", "HLI_transect", "Years_Since_Fire", "Slope", "avg_sum_precip_5yr", "Avg_tmax_degC_5yr", "Avg_tmin_degC_5yr") 
Mydotplot(tree_counts[MyVar])


########################
## 4. Collinearity #####
########################
# create data frame just for Douglas-fir
PSME_counts = 
  tree_counts %>%
  filter(Species == "PSME")

# create data frame just for lodgepole pine
PICO_counts = 
  tree_counts %>%
  filter(Species == "PICO")

## Douglas-fir: corr tree plots to assess collinearity
cor_tree <- cor(PSME_counts[,MyVar],use="pairwise.complete.obs", method = "spearman") # spearman does not assume a linear relationship and is better suited for non-normal data

corrplot_tree = corrplot(cor_tree, addCoef.col = 'black', type="upper", order="hclust",  
                          col=brewer.pal(n=10, name="RdYlBu"),tl.cex=0.8, tl.col="black")

## lodgepole pine: corr tree plots to assess collinearity
cor_tree <- cor(PICO_counts[,MyVar],use="pairwise.complete.obs", method = "spearman") # spearman does not assume a linear relationship and is better suited for non-normal data

corrplot_tree = corrplot(cor_tree, addCoef.col = 'black', type="upper", order="hclust",  
                         col=brewer.pal(n=10, name="RdYlBu"),tl.cex=0.8, tl.col="black")

## there are many collinear variables for both Douglas-fir and lodgepole pine, 
## will need to account for this in GLMMs. See manuscript for more details.
####################################

##############################################################################
## 5. Graphically explore relationships between density and covariates ##
##############################################################################

MyMultipanel.ggp2(Z = tree_counts,                   # this function doesn't work with categorical variables
                  varx = MyVar, 
                  vary = "Tree_Count", 
                  ylab = "Tree_Count",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = FALSE)





### Summarize data exploration findings overall
# zero inflation issues: maybe
# distribution type: poisson or nbinom
# outliers: yes, but real data
# collinearity: yes. 


##############################
### Other Data Exploration ###
##############################

# does tree count increase with area?
ggplot(PSME_counts, aes(x = Area_ha, y = Tree_Count))+
  geom_jitter()+
  geom_smooth(method = "lm")

### interaction effect HLI and dist to spring??

hli_spring =ggplot(tree_counts, aes(x = HLI_transect, y = Density, color = Distance_range_m)) +
  geom_point()+
  geom_smooth(method = "glm", se = FALSE)+
labs(fill = "Distance to spring (m)",
     x = "Heat Load Index",
     color = "Distance to spring (m)")+
  scale_color_brewer(palette = "BrBG", direction = -1) +  # Set fill color using Blues palette
  theme_classic()+
  theme(axis.text.x = element_text(size = 11, family = "serif"), 
        axis.text.y = element_text(size = 11, family = "serif"), 
        axis.title.y = element_text(size = 11, family = "serif"),
        axis.title.x = element_text(size = 11, family = "serif"), 
        plot.title = element_text(size = 11, family = "serif"),
        legend.title = element_text(size = 11, family = "serif"), 
        legend.text = element_text(size = 11, family = "serif"), 
        legend.position = "bottom",  
        legend.box = "horizontal"
  )


## explore clim/density relationships
ggplot(PSME_counts, aes(x = avg_sum_precip_5yr, y = Density))+geom_point()+geom_smooth(method = "glm")

ggplot(PSME_counts, aes(x = Avg_tmax_degC_5yr, y = Density))+geom_point()+geom_smooth(method = "glm")

ggplot(PSME_counts, aes(x = Avg_tmin_degC_5yr, y = Density))+geom_point()+geom_smooth(method = "glm")
