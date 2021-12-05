## Bocas plankton community - community analysis
## Date created: 01.19.21
## Date copied: 12.04.21
## Date updated: 12.04.21
## Run in R 4.1.1

library(zoo) # as.Date
library(mvabund)
library(vegan)
library(ggplot2)
library(ggfortify)
#library("grid.text")
library(ggpubr) #ggarrange
library(gridExtra) ##grid.arrange
library(grid) #textGrob
library(reshape2)

# Load data
data = read.csv("data/combined_counts.csv")
data$Date = as.Date(data$Date,"%m/%d/%y")

# Pull out only the columns of plankton counts
plankton_counts = data[,8:18]; plankton_counts$copepods = data$copepods

# Take a look at the distributions
par(mar=c(4,8,2,2))
boxplot(log10(plankton_counts),horizontal = T,las=2)

# Make smaller data set omitting 3 rarest groups (fish, crabs, shrimp)
plankton_common = cbind(plankton_counts[,2:6],plankton_counts[,9:12])

# Create mvabund object for common plankton groups
plankton_mv = mvabund(plankton_common)

# Confirm that mean and variance are positively correlated
meanvar.plot(plankton_mv,log="xy")

# Plot abundances using mvabund object... output is meh
data$Site = factor(data$Site, levels = c("STRI Point","Cristobal","Pastores"))
plot(plankton_mv~as.factor(data$Site), 
     col = as.numeric(data$Depth_ctgry), # red = 11 m; blue = 21 m
     shape = factor(data$Site),
     transformation = "no", 
     cex=0.8,lwd=0.7)

# Run models, first assuming Poisson distribution
glm1 = manyglm(plankton_mv ~ data$Site, family="poisson")
plot(glm1) #shows some residual spread over x-axis

glm2 =  manyglm(plankton_mv ~ data$Site*data$Depth_ctgry, family="negative_binomial")
plot(glm2) #residual spread much more consistent over x-axis
model2 = anova.manyglm(glm2, p.uni="adjusted") #takes 3.5 mins to run
model2 #gives model summary

glm3 = manyglm(plankton_mv ~ data$Site * 
                 data$Depth_ctgry + 
                 data$DO * 
                 data$temp_C *
                 data$salinity * 
                 data$chlorophyll,
               family="negative_binomial",
               nBoot = 1000, #default
               k=1, #default
               theta.method = "PHI") #default
plot(glm3) #still looks OK I think?
summary.manyglm(glm3)
model3 = anova.manyglm(glm3,p.uni = "adjusted") #takes 33 min
model3

#saveRDS(model3,"results/GLM_model_12.4.21.rds")
model3 = readRDS("results/GLM_model_12.4.21.rds") #looks OK

print.summary.manyglm(glm3)



