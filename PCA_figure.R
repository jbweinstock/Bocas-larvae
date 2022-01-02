## Bocas plankton community - PCA
## Date created: 01.19.21
## Date copied: 12.04.21
## Date updated: 12.31.21
## Run in R 4.1.1

library(zoo) # as.Date
#library(mvabund)
library(vegan) #rda, v2.5-7
library(ggvegan)
library(ggplot2)
library(ggfortify)
library(ggpubr) #ggarrange
library(gridExtra) ##grid.arrange
library(grid) #textGrob
library(reshape2)

# Load data
data = read.csv("data/combined_counts.csv")
data$Date = as.Date(data$Date,"%m/%d/%y")
data$Site = factor(data$Site,
                   levels = c("STRI Point",
                              "Cristobal",
                              "Pastores"))

# Pull out only the columns of plankton counts
plankton_counts = data[,8:18]; plankton_counts$copepods = data$copepods

# Make smaller data set omitting 3 rarest groups (fish, crabs, shrimp)
plankton_common = cbind(plankton_counts[,2:6],plankton_counts[,9:12])

# Log-transform data to better meet PCA assumptions (linear relationships between groups)
plankton_common_log = log10(plankton_common+1)
pairs(plankton_common)
pairs(plankton_common_log)

# PCA
plank.pca = rda(na.omit(plankton_common_log), scale=F) #all are in same units, no need to scale
summary(plank.pca)
screeplot(plank.pca,type="l")
biplot(plank.pca,type="text")

data_nona = subset.data.frame(data,is.na(data$copepods)==F)
data_nona$PC1 = plank.pca$CA$u[,1]
data_nona$PC2 = plank.pca$CA$u[,2]
vscores = as.data.frame(plank.pca$CA$v)

data_nona_P = subset.data.frame(data_nona,data_nona$Site=="Pastores")
data_nona_S = subset.data.frame(data_nona,data_nona$Site=="STRI Point")
data_nona_C = subset.data.frame(data_nona,data_nona$Site == "Cristobal")

p0 <- autoplot(plank.pca, arrows=TRUE, loadings = TRUE, layers="species",
               legend.position = "none", geom = "point",) + 
  theme_classic() +  xlim(-2.5,1.5) + theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  ggtitle("Loadings")

p1 <- ggplot(data_nona_S, aes(x=PC1,y=PC2,shape=factor(Depth_ctgry))) + 
  theme_classic() + xlim(-0.15,0.25) + ylim(-0.17,0.2) +
  #scale_fill_gradient2(low="yellow",mid="grey",high="purple",midpoint = 4) + 
  scale_fill_distiller(palette = "PuOr",
                       direction = 1) +
  scale_shape_manual(values=c(21,24),
                     labels=c("mid","deep")) +
  stat_ellipse(type = "t", level=0.95,
               color="black", size = 0.6,
               show.legend = F) + 
  geom_point(size=2.5,col="black",alpha=0.8,stroke=0.75,aes(fill=DO)) + 
  ggtitle("STRI Point") + 
  guides(shape=guide_legend(title="Depth")) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted")

p2 <- ggplot(data_nona_C, aes(x=PC1,y=PC2,shape=factor(Depth_ctgry))) + 
  theme_classic() + xlim(-0.15,0.25) + ylim(-0.17,0.2) + 
  #scale_fill_gradient2(low="yellow",mid="grey",high="purple",midpoint = 4) + 
  scale_fill_distiller(palette = "PuOr",
                       direction = 1) +
  scale_shape_manual(values=c(21,24),
                     labels=c("mid","deep")) +
  stat_ellipse(type = "t", level=0.95,
               color="black", size = 0.6,
               show.legend = F) + 
  geom_point(size=2.5,col="black",alpha=0.8,stroke=0.75,aes(fill=DO)) + 
  ggtitle("Cristobal") + 
  guides(shape=guide_legend(title="Depth")) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted")

p3 <- ggplot(data_nona_P, aes(x=PC1,y=PC2,shape=factor(Depth_ctgry))) + 
  theme_classic() + xlim(-0.15,0.25) + ylim(-0.17,0.2) + 
  scale_fill_distiller(palette = "PuOr",
                       direction = 1) +
  #scale_fill_gradient2(low="yellow",mid="grey",high="purple",midpoint = 4) + 
  scale_shape_manual(values=c(21,24),
                     labels=c("mid","deep")) +
  stat_ellipse(type = "t", level=0.95,
               color="black", size = 0.6,
               show.legend = F) + 
  geom_point(size=2.5,col="black",alpha=0.8,stroke=0.75,aes(fill=DO)) + 
  ggtitle("Pastores") + 
  guides(shape=guide_legend(title="Depth")) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted")

ggarrange(p0,p1,p2,p3,nrow=2,ncol=2,
          common.legend = T, legend = "bottom")


