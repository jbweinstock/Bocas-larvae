# script to make stacked bar graph in ggplot
# author: R.C.

#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages("RColorBrewer") 
#install.packages("devtools")
#install.packages("ggpubr")

library (tidyverse)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(ggpubr)

# load data
Forbargraph = read.csv("data/Forbargraph.csv")
Forbargraph$Depth = as.factor(Forbargraph$Depth)

# make data into long-form so everything is in one giant column
mdat = melt(Forbargraph, id.vars=c("Week", "Site","Depth"),
            measure.vars=c("pteropods", "chaetognaths", "larvaceans", "bivalves","gastropods","plutei", "barnacle_nauplii", "barnacle_cyprids", "copepods"))


# make 3 smaller datasets, one for each site
# then plot a stacked bargraph for each one split by depth 

mdatPast <- mdat %>% 
  filter(Site == "Pastores")

mdatCrist <- mdat %>% 
  filter(Site == "Cristobal")

mdatSTRI <- mdat %>% 
  filter(Site == "STRI Point")


p1 <- ggplot(mdatSTRI, aes(fill=variable, y=value, x=Week)) + 
  geom_bar(position="stack", stat="identity")+
  facet_grid(~ fct_rev(Depth)) +
  theme_classic() + 
  theme (text = element_text(size=10))+
  theme(strip.background = element_blank())+
  theme(legend.position="bottom")+
  ylim(c(0,5000)) +
  scale_fill_brewer(palette = "Set1")+
  labs(y="Count")+
  ggtitle("STRI Point")+
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(aspect.ratio = 1)+
  guides(fill=guide_legend(nrow=3,byrow=TRUE, reverse=TRUE))


p2 <- ggplot(mdatCrist, aes(fill=variable, y=value, x=Week)) + 
  geom_bar(position="stack", stat="identity")+
  facet_grid(~ fct_rev(Depth)) +
  theme_classic() + 
  theme (text = element_text(size=10))+
  theme(strip.background = element_blank())+
  theme(legend.position="bottom")+
  ylim(c(0,5000)) +
  scale_fill_brewer(palette = "Set1")+
  labs(y="Count")+
  ggtitle("Cristobal")+
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(aspect.ratio = 1)+
  guides(fill=guide_legend(nrow=3,byrow=TRUE, reverse=TRUE))

p3 <- ggplot(mdatPast, aes(fill=variable, y=value, x=Week)) + 
  geom_bar(position="stack", stat="identity")+
  facet_grid(~ fct_rev(Depth)) +
  theme_classic() + 
  theme (text = element_text(size=10))+
  theme(strip.background = element_blank())+
  theme(legend.position="bottom")+
  ylim(c(0,5000)) +
  scale_fill_brewer(palette = "Set1")+
  labs(y="Count")+
  ggtitle("Pastores")+
  theme(legend.key.size = unit(0.5, 'cm'))+
  theme(aspect.ratio = 1)+
  guides(fill=guide_legend(nrow=3,byrow=TRUE, reverse=TRUE))


