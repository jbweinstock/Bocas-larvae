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

ggplot(data, aes(x=DO, y=log10(pteropods + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")

ggplot(data, aes(x=DO, y=log10(chaetognaths + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")

ggplot(data, aes(x=DO, y=log10(larvaceans  + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")

ggplot(data, aes(x=DO, y=log10(copepods + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")

ggplot(data, aes(x=DO, y=log10(bivalves + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")

ggplot(data, aes(x=DO, y=log10(gastropods + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")

ggplot(data, aes(x=DO, y=log10(plutei + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")

ggplot(data, aes(x=DO, y=log10(barnacle_nauplii + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")

ggplot(data, aes(x=DO, y=log10(barnacle_cyprids + 1))) + 
  geom_point(aes(col=location)) + 
  theme_classic() + 
  geom_smooth(method = "gam", se=F,
              formula = y ~ log(x), 
              aes(col=location)) + 
  scale_color_brewer(palette = "Paired")






