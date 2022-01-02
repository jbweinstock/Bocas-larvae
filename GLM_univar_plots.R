## Bocas plankton community - Univariate plots
## Date created: 12.30.21
## Date updated: 12.30.21
## Run in R 4.1.1


library(zoo) # as.Date
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

## SITE-BY-DEPTH INTERACTIONS
# PTEROPODS
boxp1 <- ggplot(data, aes(x=Site,y=log10(pteropods+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Pteropods") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

# CHAETOGNATHS
boxp2 <- ggplot(data, aes(x=Site,y=log10(chaetognaths+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Chaetognaths") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

# LARVACEANS
boxp3 <- ggplot(data, aes(x=Site,y=log10(larvaceans+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Larvaceans") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

# COPEPODS
boxp4 <- ggplot(data, aes(x=Site,y=log10(copepods+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Copepods") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

# BIVALVES
boxp5 <- ggplot(data, aes(x=Site,y=log10(bivalves+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Bivalves") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

# GASTROPODS
boxp6 <- ggplot(data, aes(x=Site,y=log10(gastropods+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Gastropods") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

# PLUTEI
boxp7 <- ggplot(data, aes(x=Site,y=log10(plutei+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Plutei") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

# BARNACLE NAUPLII
boxp8 <- ggplot(data, aes(x=Site,y=log10(barnacle_nauplii+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Nauplii") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

# CYPRIDS
boxp9 <- ggplot(data, aes(x=Site,y=log10(barnacle_cyprids+1))) + 
  geom_boxplot(aes(fill=factor(Depth_ctgry)),
               alpha=0.6) + 
  theme_classic() + ggtitle("Cyprids") + 
  scale_fill_manual(values = rep(c("white","grey"),3))

ggarrange(boxp1,boxp2,boxp3,boxp4,
          boxp5,boxp6,boxp8,boxp9,boxp7,
          ncol = 2, nrow=5,
          common.legend = T)


## CONTINUOUS VARIABLES
# PTEROPODS
ggplot(data, aes(x=salinity, 
                 y=log10(pteropods+1))) + 
  geom_point(aes(shape = Site,fill=Site), 
             size = 2, 
             alpha = 0.5,
             #fill = "grey",
             stroke=0.75) + 
  geom_smooth(method = "lm",
              se = F,
              aes(col=factor(Depth_ctgry))) + 
  theme_classic() + 
  scale_shape_manual(values = c(21,22,24))

# CHAETOGNATHS
ggplot(data, aes(x=log10(chlorophyll), 
                 y=log10(chaetognaths+1))) + 
  geom_point(aes(shape=Site,
                 fill=Site),
             size=2, alpha = 0.5) + 
  theme_classic() + 
  geom_smooth(method = "lm",
              se = F,
              aes(col=factor(Depth_ctgry))) + 
  scale_shape_manual(values = c(21,22,24))

ggplot(data, aes(x=DO,
                 y=log10(chaetognaths+1),
                         fill=DOM)) + 
  geom_point(aes(shape=Site),size=2) + 
  theme_classic() + 
  geom_smooth(se = F,
              #method = "gam",
              #formula = y ~ poly(x, 2),
              aes(col=factor(Depth_ctgry,
                             linetype=Site))) + 
  scale_fill_distiller(palette = "Purples",direction = 1) + 
#  scale_fill_gradient2(high="purple",
#                        mid="grey",
#                        low="yellow",
#                        midpoint = 2) + 
  scale_shape_manual(values = c(21,22,24))

# LARVACEANS
ggplot(data, aes(x=DO, 
                 y=log10(larvaceans+1))) + 
  geom_point(aes(shape=Site,
                 fill=Site),
             size=2, alpha = 0.5) + 
  theme_classic() + 
  geom_smooth(method = "lm",
              se = F) + 
  scale_shape_manual(values = c(21,22,24))

# COPEPODS
ggplot(data, aes(x=DO, 
                 y=log10(copepods+1),
                 fill=temp_C)) + 
  geom_point(aes(shape=Site),
             size=2, 
             stroke=0.75,
             alpha = 0.9) + 
  theme_classic() + 
  geom_smooth(method = "gam",
              formula = y~ log(x),
              se = F) + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "Spectral",
                       direction = -1)

ggplot(data, aes(x=DOM, 
                 y=log10(copepods+1),
                 fill=salinity)) + 
  geom_point(aes(shape=Site),
             size=2, 
             stroke=0.75,
             alpha = 0.9) + 
  theme_classic() + 
  geom_smooth(method = "lm",
              #formula = y~ log(x),
              se = F,
              aes(col=Site)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "Blues",
                       direction = 1)

# BIVALVES
ggplot(data, aes(x=DOM, 
                 y=log10(bivalves),
                 fill=DO)) + 
  geom_point(aes(shape=Site),
             size=2, 
             stroke=0.75,
             alpha = 0.9) + 
  theme_classic() + 
  geom_smooth(#method = "gam",
    #formula = y~ log(x),
    se = F) + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "PuOr",
                       direction = 1)

ggplot(data, aes(x=temp_C, 
                 y=log10(bivalves),
                 fill=DOM)) + 
  geom_point(aes(shape=Site),
             size=2, 
             stroke=0.75,
             alpha = 0.9) + 
  theme_classic() + 
  geom_smooth(method = "lm",
    #formula = y~ log(x),
    se = F) + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "Purples",
                       direction = 1)

# GASTROPODS
ggplot(data, aes(x=DO, 
                 y=log10(gastropods+1),
                 fill = temp_C)) + 
  geom_point(aes(shape = Site), 
             size = 2, 
             alpha = 0.9,
             #fill = "grey",
             stroke=0.75) + 
  geom_smooth(method = "gam",
              formula = y ~ log(x),
              se = F,
              aes(col = Site)) + 
  theme_classic() + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "Spectral",
                       direction = -1)

# PLUTEI
ggplot(data, aes(x=DO, 
                 y=log10(plutei+1),
                 fill=DOM)) + 
  geom_point(aes(shape = Site), 
             size = 2, 
             alpha = 0.9,
             #fill = "grey",
             stroke=0.75) + 
  geom_smooth(method = "gam",
              formula = y ~ log(x),
              se = F,
              aes(col = Site)) + 
  theme_classic() + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "Purples",
                       direction = 1)

# BARNACLE NAUPLII
ggplot(data, aes(x=DOM, 
                 y=log10(barnacle_nauplii+1),
                 fill = DO)) + 
  geom_point(aes(shape = Site), 
             size = 2, 
             alpha = 0.9,
             stroke=0.75) + 
  geom_smooth(method = "gam",
              formula = y ~ poly(x,2),
              se = F) + 
  theme_classic() + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "PuOr",
                       direction = -1)

ggplot(data, aes(x=log10(chlorophyll), 
                 y=log10(barnacle_nauplii+1),
                 fill = DO)) + 
  geom_point(aes(shape = Site), 
             size = 2, 
             alpha = 0.9,
             stroke=0.75) + 
  geom_smooth(method = "lm",
              se = F) + 
  theme_classic() + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "PuOr",
                       direction = 1)

# CYPRIDS
ggplot(data, aes(x=DOM, 
                 y=log10(barnacle_cyprids+1),
                 fill = salinity)) + 
  geom_point(aes(shape = Site), 
             size = 2, 
             alpha = 0.9,
             stroke=0.75) + 
  geom_smooth(method = "gam",
              formula = y ~ poly(x,2),
              se = F) + 
  theme_classic() + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "Spectral",
                       direction = 1)



ggplot(data, aes(x=DOM, 
                 y=DO,
                 fill=(bivalves))) + 
  geom_point(aes(shape=factor(Depth_ctgry)),
             size=2, 
             stroke=0.75,
             alpha = 0.9) + 
  theme_classic() + 
  geom_smooth(method = "lm",
    #formula = y~ log(x),
    se = F,
    aes(col=factor(Depth_ctgry))) + 
  scale_shape_manual(values = c(21,22,24)) + 
  scale_fill_distiller(palette = "Spectral",
                       direction = -1)




