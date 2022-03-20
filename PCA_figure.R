## Bocas plankton community - PCA
## Date created: 01.19.21
## Date copied: 12.04.21
## Date updated: 01.08.22
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
data = read.csv("data/combined_counts_interp.csv")
data$Date = as.Date(data$Date,"%m/%d/%y")
data$Site = factor(data$Site,
                   levels = c("STRI Point",
                              "Cristobal",
                              "Pastores"))

# Pull out only the columns of plankton counts
plankton_counts = data[,9:19]; plankton_counts$copepods = data$copepods

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

data_nona = subset.data.frame(data, is.na(data$copepods)==F)
data_nona$PC1 = plank.pca$CA$u[,1] # 36.42%
data_nona$PC2 = plank.pca$CA$u[,2] # 17.91%

data_nona$Site = factor(data_nona$Site, levels = c("STRI Point","Cristobal","Pastores"))
data_nona$Depth_ctgry = factor(data_nona$Depth_ctgry)

data_nona$chlorophyll = na.approx(data_nona$chlorophyll,method="linear",na.rm=F)
data_nona$DOM = na.approx(data_nona$DOM,method="linear",na.rm=F)
data_nona$BGA = na.approx(data_nona$BGA,method="linear",na.rm=F)
data_nona$turbidity = na.approx(data_nona$turbidity,method="linear",na.rm=F)

data_waterqual = as.data.frame(cbind(data_nona$DO, data_nona$temp_C,
                                     data_nona$salinity, data_nona$chlorophyll, 
                                     data_nona$DOM)) #, data_nona$turbidity,data_nona$BGA))

colnames(data_waterqual) = c("DO","temp","sal","chlor","DOM") #, "turbidity","BGA")

data_waterqual$Site = data_nona$Site
data_waterqual$Depth = data_nona$Depth_ctgry

# for envfit, followed this site: https://jkzorz.github.io/2020/04/04/NMDS-extras.html

env_mod <- envfit(ord = plank.pca, env = data_waterqual, na.rm = T)
env_mod # good news is, turbidity is not important. BGA maybe is, but we will let that go...
plot(env_mod)
en_coord_cont = as.data.frame(scores(env_mod, "vectors")) * ordiArrowMul(env_mod)
en_coord_cat = as.data.frame(scores(env_mod, "factors")) * ordiArrowMul(env_mod)

data_nona_P = subset.data.frame(data_nona,data_nona$Site=="Pastores")
data_nona_S = subset.data.frame(data_nona,data_nona$Site=="STRI Point")
data_nona_C = subset.data.frame(data_nona,data_nona$Site == "Cristobal")

p0 <- autoplot(plank.pca, arrows=TRUE, loadings = TRUE, layers="species",
               legend.position = "none", geom = "point",) + 
  theme_classic() +  xlim(-3,2) + theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  ggtitle("Loadings") + 
  #geom_point(data = data_nona, aes(x=PC1,y=PC2,col=Date)) +
  #scale_color_distiller(palette = "Spectral", direction = 1) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               data = en_coord_cont, size =1, alpha = 0.5, colour = "blue") +
  geom_point(data = en_coord_cat[4:5,], aes(x = PC1, y = PC2), 
             shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
  geom_text(data = en_coord_cat[4:5,], aes(x = PC1, y = PC2+0.1), 
            label = row.names(en_coord_cat[4:5,]), colour = "navy", fontface = "bold") + 
  geom_text(data = en_coord_cont, aes(x = PC1, y = PC2+0.1), colour = "blue", 
            fontface = "bold", label = row.names(en_coord_cont)) 


p1 <- ggplot(data_nona_S, aes(x=PC1,y=PC2,shape=factor(Depth_ctgry))) + 
  theme_classic() + xlim(-0.15,0.25) + ylim(-0.2,0.2) +
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
  theme_classic() + xlim(-0.15,0.25) + ylim(-0.2,0.2) + 
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
  theme_classic() + xlim(-0.15,0.25) + ylim(-0.2,0.2) + 
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


