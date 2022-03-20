## Bocas plankton community - Plankton plots vs Temperature
## Date created: 02.13.22
## Date updated: 02.13.22
## Run in R 4.1.1


library(zoo) # as.Date
library(ggplot2)
library(ggpubr) #ggarrange

# Load data
data = read.csv("data/combined_counts.csv")
data$Date = as.Date(data$Date,"%m/%d/%y")
data$Site = factor(data$Site, levels = c("STRI Point","Cristobal","Pastores"))
data$location = paste(data$Site,data$Depth_ctgry,sep=" ")
data$location = factor(data$location, levels = c("STRI Point 10", "STRI Point 20",
                                                 "Cristobal 10", "Cristobal 20",
                                                 "Pastores 10", "Pastores 20"))
data$Depth_ctgry = factor(data$Depth_ctgry)

shallow <- subset.data.frame(data, data$Depth_ctgry == 10)
deep <- subset.data.frame(data, data$Depth_ctgry == 20)

# Temperature plots
pter_Tmod_10 <- lm((log10(shallow$pteropods + 1)/379) ~ shallow$temp_C)
pter_Tmod_20 <- lm((log10(deep$pteropods + 1)/379) ~ deep$temp_C)

T1 <- ggplot(data, aes(x=temp_C, y=log10(pteropods + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.004"),
           x=27, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.006"),
           x=27, y=0.0065, size=3,col="lightseagreen",parse=T)

chaet_Tmod_10 <- lm((log10(shallow$chaetognaths + 1)/379) ~ shallow$temp_C)
chaet_Tmod_20 <- lm((log10(deep$chaetognaths + 1)/379) ~ deep$temp_C)

T2 <- ggplot(data, aes(x=temp_C, y=log10(chaetognaths + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.01"),
           x=27, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.004"),
           x=27, y=0.0065, size=3,col="lightseagreen",parse=T)

lar_Tmod_10 <- lm((log10(shallow$larvaceans + 1)/379) ~ shallow$temp_C)
lar_Tmod_20 <- lm((log10(deep$larvaceans + 1)/379) ~ deep$temp_C)

T3 <- ggplot(data, aes(x=temp_C, y=log10(larvaceans  + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.005"),
           x=27, y=0.0075, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.132"),
           x=27, y=0.0071, size=3,col="lightseagreen",parse=T)

cop_Tmod_10 <- lm((log10(shallow$copepods + 1)/379) ~ shallow$temp_C)
cop_Tmod_20 <- lm((log10(deep$copepods + 1)/379) ~ deep$temp_C)

T4 <- ggplot(data, aes(x=temp_C, y=log10(copepods + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.005"),
           x=27, y=0.0015, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.23"),
           x=27, y=0.001, size=3,col="lightseagreen",parse=T)

biv_Tmod_10 <- lm((log10(shallow$bivalves + 1)/379) ~ shallow$temp_C)
biv_Tmod_20 <- lm((log10(deep$bivalves + 1)/379) ~ deep$temp_C)

T5 <- ggplot(data, aes(x=temp_C, y=log10(bivalves + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.002"),
           x=27, y=0.0018, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.08"),
           x=27, y=0.0013, size=3,col="lightseagreen",parse=T)

gas_Tmod_10 <- lm((log10(shallow$gastropods + 1)/379) ~ shallow$temp_C)
gas_Tmod_20 <- lm((log10(deep$gastropods + 1)/379) ~ deep$temp_C)

T6 <- ggplot(data, aes(x=temp_C, y=log10(gastropods + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.14"),
           x=27, y=0.0015, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.11"),
           x=27, y=0.001, size=3,col="lightseagreen",parse=T)

plut_Tmod_10 <- lm((log10(shallow$plutei + 1)/379) ~ shallow$temp_C)
plut_Tmod_20 <- lm((log10(deep$plutei + 1)/379) ~ deep$temp_C)

T7 <- ggplot(data, aes(x=temp_C, y=log10(plutei + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.01"),
           x=27, y=0.0071, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.20"),
           x=27, y=0.0067, size=3,col="lightseagreen",parse=T)

naup_Tmod_10 <- lm((log10(shallow$barnacle_nauplii + 1)/379) ~ shallow$temp_C)
naup_Tmod_20 <- lm((log10(deep$barnacle_nauplii + 1)/379) ~ deep$temp_C)

T8 <- ggplot(data, aes(x=temp_C, y=log10(barnacle_nauplii + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.0002"),
           x=27, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.0006"),
           x=27, y=0.0065, size=3,col="lightseagreen",parse=T)

cyp_Tmod_10 <- lm((log10(shallow$barnacle_cyprids + 1)/379) ~ shallow$temp_C)
cyp_Tmod_20 <- lm((log10(deep$barnacle_cyprids + 1)/379) ~ deep$temp_C)

T9 <- ggplot(data, aes(x=temp_C, y=log10(barnacle_cyprids + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.003"),
           x=27, y=0.0054, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.03"),
           x=27, y=0.005, size=3,col="lightseagreen",parse=T)



ggarrange(T1, T2, T3,
          T4, T5, T6,
          T7, T8, T9,
          nrow=3, ncol=3,
          common.legend = T,
          legend = "bottom")



