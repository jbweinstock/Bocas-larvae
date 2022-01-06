## Bocas plankton community - Plankton plots vs DO
## Date created: 01.01.22
## Date updated: 01.01.22
## Run in R 4.1.1


library(zoo) # as.Date
library(vegan) #rda
library(ggvegan)
library(ggplot2)
library(ggfortify)
library(ggpubr) #ggarrange
library(gridExtra) ##grid.arrange
library(grid) #textGrob
library(reshape2)
library(interp) #using to grid plankton data

# Load data
data = read.csv("data/combined_counts.csv")
data$Date = as.Date(data$Date,"%m/%d/%y")
data$Site = factor(data$Site, levels = c("STRI Point","Cristobal","Pastores"))
data$location = paste(data$Site,data$Depth_ctgry,sep=" ")
data$location = factor(data$location, levels = c("STRI Point 10", "STRI Point 20",
                                                 "Cristobal 10", "Cristobal 20",
                                                 "Pastores 10", "Pastores 20"))
data$Depth_ctgry = factor(data$Depth_ctgry)


DO1 <- ggplot(data, aes(x=DO, y=log10(pteropods + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3,
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,25)) 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) + 
  ggtitle("Pteropods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")

DO2 <- ggplot(data, aes(x=DO, y=log10(chaetognaths + 1)/379)) + 
  geom_point(aes(fill=location), 
             alpha=0.4,size=2,shape=21) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  #scale_color_manual(values = c(brewer.pal(12,"Paired")[c(9,10)])) + 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Chaetognaths") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")

DO3 <- ggplot(data, aes(x=DO, y=log10(larvaceans  + 1)/379)) + 
  geom_point(aes(fill=location), 
             alpha=0.4,size=2,shape=21) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  #scale_color_manual(values = c(brewer.pal(12,"Paired")[c(9,10)])) + 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Larvaceans") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")

DO4 <- ggplot(data, aes(x=DO, y=log10(copepods + 1)/379)) + 
  geom_point(aes(fill=location), 
             alpha=0.4,size=2,shape=21) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  #scale_color_manual(values = c(brewer.pal(12,"Paired")[c(9,10)])) + 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Copepods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")

DO5 <- ggplot(data, aes(x=DO, y=log10(bivalves + 1)/379)) + 
  geom_point(aes(fill=location), 
             alpha=0.4,size=2,shape=21) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  #scale_color_manual(values = c(brewer.pal(12,"Paired")[c(9,10)])) + 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Bivalves") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")

DO6 <- ggplot(data, aes(x=DO, y=log10(gastropods + 1)/379)) + 
  geom_point(aes(fill=location), 
             alpha=0.4,size=2,shape=21) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  #scale_color_manual(values = c(brewer.pal(12,"Paired")[c(9,10)])) + 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Gastropods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")

DO7 <- ggplot(data, aes(x=DO, y=log10(plutei + 1)/379)) + 
  geom_point(aes(fill=location), 
             alpha=0.4,size=2,shape=21) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  #scale_color_manual(values = c(brewer.pal(12,"Paired")[c(9,10)])) + 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Plutei") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")

DO8 <- ggplot(data, aes(x=DO, y=log10(barnacle_nauplii + 1)/379)) + 
  geom_point(aes(fill=location), 
             alpha=0.4,size=2,shape=21) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  #scale_color_manual(values = c(brewer.pal(12,"Paired")[c(9,10)])) + 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Nauplii") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")

DO9 <- ggplot(data, aes(x=DO, y=log10(barnacle_cyprids + 1)/379)) + 
  geom_point(aes(fill=location), 
             alpha=0.4,size=2,shape=21) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  #scale_color_manual(values = c(brewer.pal(12,"Paired")[c(9,10)])) + 
  scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Cyprids") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"")



ggarrange(DO1, DO2, DO3,
          DO4, DO5, DO6,
          DO7, DO8, DO9,
          nrow=3, ncol=3,
          common.legend = T,
          legend = "bottom")







