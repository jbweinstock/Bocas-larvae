## Bocas plankton community - PCA
## Date created: 01.19.21
## Date copied: 12.04.21
## Date updated: 12.04.21
## Run in R 4.1.1

library(zoo) # as.Date
library(mvabund)
library(vegan) #rda
library(ggplot2)
library(ggfortify)
library(ggpubr) #ggarrange
library(gridExtra) ##grid.arrange
library(grid) #textGrob
library(reshape2)

# Load data
data = read.csv("data/combined_counts.csv")
data$Date = as.Date(data$Date,"%m/%d/%y")

# Pull out only the columns of plankton counts
plankton_counts = data[,8:18]; plankton_counts$copepods = data$copepods

# Make smaller data set omitting 3 rarest groups (fish, crabs, shrimp)
plankton_common = cbind(plankton_counts[,2:6],plankton_counts[,9:12])

# Log-transform data to better meet PCA assumptions
plankton_common_log = log10(plankton_common+1)

# PCA
plank.pca = rda(na.omit(plankton_common_log), scale=F) #all are in same units, no need to scale
summary(plank.pca)
screeplot(plank.pca,type="l")
biplot(plank.pca,type="text")

data_nona = subset.data.frame(data,is.na(data$copepods)==F)
data_nona$PC1 = plank.pca$CA$u[,1]
data_nona$PC2 = plank.pca$CA$u[,2]

ggplot(data_nona, aes(x=PC1,y=PC2,shape=factor(Depth_ctgry))) + 
  theme_classic() + 
  #scale_fill_distiller(palette = "Spectral",direction = 1) + 
  scale_fill_gradient2(low="red",mid="white",high="blue",midpoint = 4) + 
  scale_shape_manual(values=c(21,24),
                     labels=c("mid","deep")) +
  stat_ellipse(geom = "polygon", type = "t", level=0.95,
               alpha = 0.25, color="black",
               #aes(color=factor(Depth_ctgry)), 
               show.legend = F) + 
  geom_point(size=2,col="black",alpha=0.8,stroke=0.7,aes(fill=DO))





