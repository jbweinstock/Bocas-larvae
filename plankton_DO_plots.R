## Bocas plankton community - Plankton plots vs DO
## Date created: 01.01.22
## Date updated: 01.08.22
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
data = read.csv("data/combined_counts_interp.csv")
data$Date = as.Date(data$Date,"%m/%d/%y")
data$Site = factor(data$Site, levels = c("STRI Point","Cristobal","Pastores"))
data$location = paste(data$Site,data$Depth_ctgry,sep=" ")
data$location = factor(data$location, levels = c("STRI Point 10", "STRI Point 20",
                                                 "Cristobal 10", "Cristobal 20",
                                                 "Pastores 10", "Pastores 20"))
data$Depth_ctgry = factor(data$Depth_ctgry)

shallow <- subset.data.frame(data, data$Depth_ctgry == 10)
deep <- subset.data.frame(data, data$Depth_ctgry == 20)

# Mini section below for Table S1
######
shallow_S = subset.data.frame(shallow, shallow$Site == "STRI Point")
shallow_C = subset.data.frame(shallow, shallow$Site == "Cristobal")
shallow_P = subset.data.frame(shallow, shallow$Site == "Pastores")
deep_S = subset.data.frame(deep, deep$Site == "STRI Point")
deep_C = subset.data.frame(deep, deep$Site == "Cristobal")
deep_P = subset.data.frame(deep, deep$Site == "Pastores")
######

pter_mod_10 <- lm((log10(shallow$pteropods + 1)/379) ~ log10(shallow$DO))
pter_mod_20 <- lm((log10(deep$pteropods + 1)/379) ~ log10(deep$DO))

#shallow$pter_resids = pter_mod_10$residuals
#deep$pter_resids = pter_mod_20$residuals

DO1 <- ggplot(data, aes(x=DO, y=log10(pteropods + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             #stroke=0.8,
             alpha=0.4,size=2,col="black") + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              formula = y ~ log(x),
              size=1.7,
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Pteropods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.12"),
           x=1, y=0.0075, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.05"),
           x=1, y=0.0066, size=3,col="lightseagreen",parse=T)

chaet_mod_10 <- lm((log10(shallow$chaetognaths + 1)/379) ~ log10(shallow$DO))
chaet_mod_20 <- lm((log10(deep$chaetognaths + 1)/379) ~ log10(deep$DO))

DO2 <- ggplot(data, aes(x=DO, y=log10(chaetognaths + 1)/379)) + 
  geom_rect(xmin=1.85,xmax=2.25,ymin=-1,ymax=1,fill="grey90") +
  geom_vline(xintercept = 2.05, linetype = "dotted") +
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Chaetognaths") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.006"),
           x=7, y=0.0075, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.21"),
           x=7, y=0.0066, size=3,col="lightseagreen",parse=T)


larv_mod_10 <- lm((log10(shallow$larvaceans + 1)/379) ~ log10(shallow$DO))
larv_mod_20 <- lm((log10(deep$larvaceans + 1)/379) ~ log10(deep$DO))

DO3 <- ggplot(data, aes(x=DO, y=log10(larvaceans  + 1)/379)) + 
  geom_rect(xmin=2.124,xmax=3.596,ymin=-1,ymax=1,fill="grey90") +
  #geom_rect(xmin=3.103,xmax=3.837,ymin=-1,ymax=1,fill="grey80",alpha=0.5) +
  geom_vline(xintercept = 2.86, linetype = "dotted") +
  #geom_vline(xintercept = 3.47, linetype = "dotted") +
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Larvaceans") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.31"),
           x=1, y=0.0075, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.36"),
           x=1, y=0.0066, size=3,col="lightseagreen",parse=T)


cope_mod_10 <- lm((log10(shallow$copepods + 1)/379) ~ log10(shallow$DO))
cope_mod_20 <- lm((log10(deep$copepods + 1)/379) ~ log10(deep$DO))

DO4 <- ggplot(data, aes(x=DO, y=log10(copepods + 1)/379)) +  
  geom_rect(xmin=1.183,xmax=1.977,ymin=-1,ymax=1,fill="grey90") +
  geom_vline(xintercept = 1.58, linetype = "dotted") +
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Copepods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.002"),
           x=7, y=0.002, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.33"),
           x=7, y=0.001, size=3,col="lightseagreen",parse=T)

biv_mod_10 <- lm((log10(shallow$bivalves + 1)/379) ~ log10(shallow$DO))
biv_mod_20 <- lm((log10(deep$bivalves + 1)/379) ~ log10(deep$DO))

DO5 <- ggplot(data, aes(x=DO, y=log10(bivalves + 1)/379)) + 
  geom_rect(xmin=2.747,xmax=3.313,ymin=-1,ymax=1,fill="grey90") +
  geom_vline(xintercept = 3.03, linetype = "dotted") +
  geom_point(aes(fill=Depth_ctgry, shape=Site),
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Bivalves") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.02"),
           x=7.3, y=0.0018, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.42"),
           x=7.3, y=0.001, size=3,col="lightseagreen",parse=T)

gastr_mod_10 <- lm((log10(shallow$gastropods + 1)/379) ~ log10(shallow$DO))
gastr_mod_20 <- lm((log10(deep$gastropods + 1)/379) ~ log10(deep$DO))

DO6 <- ggplot(data, aes(x=DO, y=log10(gastropods + 1)/379)) + 
  geom_rect(xmin=2.008,xmax=2.652,ymin=-1,ymax=1,fill="grey90") +
  geom_vline(xintercept = 2.33, linetype = "dotted") +
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) +
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Gastropods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.09"),
           x=1, y=0.0082, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.46"),
           x=1, y=0.0073, size=3,col="lightseagreen",parse=T)

plut_mod_10 <- lm((log10(shallow$plutei + 1)/379) ~ log10(shallow$DO))
plut_mod_20 <- lm((log10(deep$plutei + 1)/379) ~ (deep$DO)^2)

DO7 <- ggplot(data, aes(x=DO, y=log10(plutei + 1)/379)) + 
  geom_rect(xmin=3.745,xmax=4.635,ymin=-1,ymax=1,fill="grey90") +
  geom_vline(xintercept = 4.19, linetype = "dotted") +
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ poly(x,2),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Plutei") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.10"),
           x=1, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.23"),
           x=1, y=0.0061, size=3,col="lightseagreen",parse=T)

naup_mod_10 <- lm((log10(shallow$barnacle_nauplii + 1)/379) ~ log10(shallow$DO))
naup_mod_20 <- lm((log10(deep$barnacle_nauplii + 1)/379) ~ log10(deep$DO))

DO8 <- ggplot(data, aes(x=DO, y=log10(barnacle_nauplii + 1)/379)) + 
  geom_rect(xmin=1.686,xmax=2.134,ymin=-1,ymax=1,fill="grey90") +
  geom_vline(xintercept = 1.91, linetype = "dotted") +
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Nauplii") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.008"),
           x=0.7, y=0.0075, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.18"),
           x=0.7, y=0.0068, size=3,col="lightseagreen",parse=T)

cyp_mod_10 <- lm((log10(shallow$barnacle_cyprids + 1)/379) ~ log10(shallow$DO))
cyp_mod_20 <- lm((log10(deep$barnacle_cyprids + 1)/379) ~ log10(deep$DO))

DO9 <- ggplot(data, aes(x=DO, y=log10(barnacle_cyprids + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Cyprids") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.02"),
           x=7.5, y=0.0009, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.10"),
           x=7.5, y=0.0003, size=3,col="lightseagreen",parse=T)



ggarrange(DO1, DO2, DO3,
          DO4, DO5, DO6,
          DO7, DO8, DO9,
          nrow=3, ncol=3,
          common.legend = T,
          legend = "bottom")


### Same plots for DOM
pter_DOMmod_10 <- lm((log10(shallow$pteropods + 1)/379) ~ shallow$DOM)
pter_DOMmod_20 <- lm((log10(deep$pteropods + 1)/379) ~ deep$DOM)

DOM1 <- ggplot(data, aes(x=DOM, y=log10(pteropods + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ poly(x,2),
              size=1.3,
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) +
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) + 
  ggtitle("Pteropods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.03"),
           x=4.2, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.03"),
           x=4.2, y=0.0065, size=3,col="lightseagreen",parse=T)

chaet_DOMmod_10 <- lm((log10(shallow$chaetognaths + 1)/379) ~ shallow$DOM)
chaet_DOMmod_20 <- lm((log10(deep$chaetognaths + 1)/379) ~ deep$DOM)

DOM2 <- ggplot(data, aes(x=DOM, y=log10(chaetognaths + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) +
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Chaetognaths") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.005"),
           x=0, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.09"),
           x=0, y=0.0065, size=3,col="lightseagreen",parse=T)

lar_DOMmod_10 <- lm((log10(shallow$larvaceans + 1)/379) ~ shallow$DOM)
lar_DOMmod_20 <- lm((log10(deep$larvaceans + 1)/379) ~ deep$DOM)

DOM3 <- ggplot(data, aes(x=DOM, y=log10(larvaceans  + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) +
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Larvaceans") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.15"),
           x=0, y=0.001, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.27"),
           x=0, y=0.0005, size=3,col="lightseagreen",parse=T)

cop_DOMmod_10 <- lm((log10(shallow$copepods + 1)/379) ~ shallow$DOM)
cop_DOMmod_20 <- lm((log10(deep$copepods + 1)/379) ~ deep$DOM)

DOM4 <- ggplot(data, aes(x=DOM, y=log10(copepods + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) +
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Copepods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.009"),
           x=0, y=0.002, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.23"),
           x=0, y=0.0015, size=3,col="lightseagreen",parse=T)

biv_DOMmod_10 <- lm((log10(shallow$bivalves + 1)/379) ~ shallow$DOM)
biv_DOMmod_20 <- lm((log10(deep$bivalves + 1)/379) ~ deep$DOM)

DOM5 <- ggplot(data, aes(x=DOM, y=log10(bivalves + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) + 
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Bivalves") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.008"),
           x=0, y=0.002, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.30"),
           x=0, y=0.0015, size=3,col="lightseagreen",parse=T)

gas_DOMmod_10 <- lm((log10(shallow$gastropods + 1)/379) ~ shallow$DOM)
gas_DOMmod_20 <- lm((log10(deep$gastropods + 1)/379) ~ deep$DOM)

DOM6 <- ggplot(data, aes(x=DOM, y=log10(gastropods + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) + 
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Gastropods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.05"),
           x=4, y=0.008, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.30"),
           x=4, y=0.0075, size=3,col="lightseagreen",parse=T)

plut_DOMmod_10 <- lm((log10(shallow$plutei + 1)/379) ~ shallow$DOM)
plut_DOMmod_20 <- lm((log10(deep$plutei + 1)/379) ~ deep$DOM)

DOM7 <- ggplot(data, aes(x=DOM, y=log10(plutei + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) +
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Plutei") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.15"),
           x=4, y=0.0065, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.13"),
           x=4, y=0.006, size=3,col="lightseagreen",parse=T)

naup_DOMmod_10 <- lm((log10(shallow$barnacle_nauplii + 1)/379) ~ shallow$DOM)
naup_DOMmod_20 <- lm((log10(deep$barnacle_nauplii + 1)/379) ~ deep$DOM)

DOM8 <- ggplot(data, aes(x=DOM, y=log10(barnacle_nauplii + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) +
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Nauplii") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.01"),
           x=4, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.15"),
           x=4, y=0.0065, size=3,col="lightseagreen",parse=T)

cyp_DOMmod_10 <- lm((log10(shallow$barnacle_cyprids + 1)/379) ~ shallow$DOM)
cyp_DOMmod_20 <- lm((log10(deep$barnacle_cyprids + 1)/379) ~ deep$DOM)

DOM9 <- ggplot(data, aes(x=DOM, y=log10(barnacle_cyprids + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2) + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              #formula = y ~ log(x),
              size=1.3, 
              aes(col=Depth_ctgry)) + 
  #scale_color_brewer(palette = "Paired", aesthetics = "fill") + 
  scale_shape_manual(values = c(21,22,24)) +
  #scale_color_manual(values = c(brewer.pal(9,"Purples")[c(6,9)])) +
  ggtitle("Cyprids") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.006"),
           x=4.5, y=0.006, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.04"),
           x=4.5, y=0.0055, size=3,col="lightseagreen",parse=T)



ggarrange(DOM1, DOM2, DOM3,
          DOM4, DOM5, DOM6,
          DOM7, DOM8, DOM9,
          nrow=3, ncol=3,
          common.legend = T,
          legend = "bottom")




