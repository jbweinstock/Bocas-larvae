## Bocas plankton community - Plankton plots in PC space
## Date created: 12.31.21
## Date updated: 01.04.22
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
data$Depth_ctgry = factor(data$Depth_ctgry)

data$chlorophyll = na.approx(data$chlorophyll,method="linear",na.rm=F)
data$DOM = na.approx(data$DOM,method="linear",na.rm=F)
data$BGA = na.approx(data$BGA,method="linear",na.rm=F)
data$turbidity = na.approx(data$turbidity,method="linear",na.rm=F)

data_waterqual = as.data.frame(cbind(data$DO, data$temp_C, #data$turbidity,
                                     data$salinity, data$chlorophyll, 
                                     data$DOM)) #, data$BGA))
colnames(data_waterqual) = c("DO","temp", #"turbidity",
                             "sal","chlor","DOM") #,"BGA")

water.pca = rda(na.omit(data_waterqual), scale=T) #different units, have to scale
summary(water.pca)
screeplot(water.pca,type="l")
biplot(water.pca,type="text")

data$PO_PC1 = water.pca$CA$u[,1] # 45.7%
data$PO_PC2 = water.pca$CA$u[,2] # 23.6%

#data$total = (data$pteropods + data$chaetognaths + data$larvaceans + 
#                data$copepods + data$bivalves + data$gastropods + 
#                data$plutei + data$barnacle_nauplii + data$barnacle_cyprids)/100

ggplot(data, aes(x=PO_PC1, y=PO_PC2, fill=Week)) + 
  geom_point(size=3, alpha=0.9, stroke = 0.8, aes(shape=Site)) + 
  scale_fill_distiller(palette = "Spectral") + theme_classic() + 
  scale_shape_manual(values = c(21,22,24)) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted")

loadings <- autoplot(water.pca, arrows=TRUE, loadings = TRUE, #layers="species",
                     legend.position = "none", geom = "point") + 
  theme_classic() +  xlim(-3,3) + theme(legend.position = "none") + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  ggtitle("Loadings") + 
  xlab("PC1 (36.3%)") + 
  ylab("PC2 (22.4%)") + 
  theme(legend.position = "none")


#PTEROPODS
grid_pter <- with(data, interp::interp(x=PO_PC1, y=PO_PC2, z=pteropods,
                                  duplicate = "mean",
                                  method = "linear"))

griddf_pter <- subset(data.frame(x = rep(grid_pter$x, nrow(grid_pter$z)),
                            y = rep(grid_pter$y, each = ncol(grid_pter$z)),
                            z = as.numeric(grid_pter$z)),
                 !is.na(z))

colnames(griddf_pter) = c("PO_PC1", "PO_PC2", "pteropods")

pter <- ggplot(data=griddf_pter, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(pteropods + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") + 
  ggtitle("Pteropods") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))

# CHAETOGNATHS
grid_chaet <- with(data, interp::interp(x=PO_PC1, y=PO_PC2, z=chaetognaths,
                                       duplicate = "mean",
                                       method = "linear"))

griddf_chaet <- subset(data.frame(x = rep(grid_chaet$x, nrow(grid_chaet$z)),
                            y = rep(grid_chaet$y, each = ncol(grid_chaet$z)),
                            z = as.numeric(grid_chaet$z)),
                 !is.na(z))

colnames(griddf_chaet) = c("PO_PC1", "PO_PC2", "chaetognaths")

chaet <- ggplot(data=griddf_chaet, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(chaetognaths + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") +
  ggtitle("Chaetognaths") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))

# LARVACEANS
grid_larv <- with(data, interp::interp(x=PO_PC1, y=PO_PC2, z=larvaceans,
                                       duplicate = "mean",
                                       method = "linear"))

griddf_larv <- subset(data.frame(x = rep(grid_larv$x, nrow(grid_larv$z)),
                            y = rep(grid_larv$y, each = ncol(grid_larv$z)),
                            z = as.numeric(grid_larv$z)),
                 !is.na(z))

colnames(griddf_larv) = c("PO_PC1", "PO_PC2", "larvaceans")

larv <- ggplot(data=griddf_larv, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(larvaceans + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") +
  ggtitle("Larvaceans") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))

# COPEPODS
data_cop_nona = subset.data.frame(data, is.na(data$copepods) == F)

grid_cope <- with(data_cop_nona, interp::interp(x=PO_PC1, y=PO_PC2, z=copepods,
                                       duplicate = "mean",
                                       method = "linear"))

griddf_cope <- subset(data.frame(x = rep(grid_cope$x, nrow(grid_cope$z)),
                            y = rep(grid_cope$y, each = ncol(grid_cope$z)),
                            z = as.numeric(grid_cope$z)),
                 !is.na(z))

colnames(griddf_cope) = c("PO_PC1", "PO_PC2", "copepods")

copes <- ggplot(data=griddf_cope, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(copepods + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") + 
  ggtitle("Copepods") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))

# BIVALVES
grid_biv <- with(data, interp::interp(x=PO_PC1, y=PO_PC2, z=bivalves,
                                       duplicate = "mean",
                                       method = "linear"))

griddf_biv <- subset(data.frame(x = rep(grid_biv$x, nrow(grid_biv$z)),
                            y = rep(grid_biv$y, each = ncol(grid_biv$z)),
                            z = as.numeric(grid_biv$z)),
                 !is.na(z))

colnames(griddf_biv) = c("PO_PC1", "PO_PC2", "bivalves")

biv <- ggplot(data=griddf_biv, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(bivalves + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") +
  ggtitle("Bivalves") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))

# GASTROPODS
grid_gast <- with(data, interp::interp(x=PO_PC1, y=PO_PC2, z=gastropods,
                                       duplicate = "mean",
                                       method = "linear"))

griddf_gast <- subset(data.frame(x = rep(grid_gast$x, nrow(grid_gast$z)),
                            y = rep(grid_gast$y, each = ncol(grid_gast$z)),
                            z = as.numeric(grid_gast$z)),
                 !is.na(z))

colnames(griddf_gast) = c("PO_PC1", "PO_PC2", "gastropods")

gastr <- ggplot(data=griddf_gast, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(gastropods + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") + 
  ggtitle("Gastropods") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))

# PLUTEI
grid_plut <- with(data, interp::interp(x=PO_PC1, y=PO_PC2, z=plutei,
                                       duplicate = "mean",
                                       method = "linear"))

griddf_plut <- subset(data.frame(x = rep(grid_plut$x, nrow(grid_plut$z)),
                            y = rep(grid_plut$y, each = ncol(grid_plut$z)),
                            z = as.numeric(grid_plut$z)),
                 !is.na(z))

colnames(griddf_plut) = c("PO_PC1", "PO_PC2", "plutei")

plut <- ggplot(data=griddf_plut, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(plutei + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") +
  ggtitle("Plutei") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))

# NAUPLII
grid_naup <- with(data, interp::interp(x=PO_PC1, y=PO_PC2, z=barnacle_nauplii,
                                       duplicate = "mean",
                                       method = "linear"))

griddf_naup <- subset(data.frame(x = rep(grid_naup$x, nrow(grid_naup$z)),
                            y = rep(grid_naup$y, each = ncol(grid_naup$z)),
                            z = as.numeric(grid_naup$z)),
                 !is.na(z))

colnames(griddf_naup) = c("PO_PC1", "PO_PC2", "barnacle_nauplii")

naup <- ggplot(data=griddf_naup, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(barnacle_nauplii + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") + 
  ggtitle("Nauplii") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))

# CYPRIDS
grid_cyp <- with(data, interp::interp(x=PO_PC1, y=PO_PC2, z=barnacle_cyprids,
                                       duplicate = "mean",
                                       method = "linear"))

griddf_cyp <- subset(data.frame(x = rep(grid_cyp$x, nrow(grid_cyp$z)),
                            y = rep(grid_cyp$y, each = ncol(grid_cyp$z)),
                            z = as.numeric(grid_cyp$z)),
                 !is.na(z))

colnames(griddf_cyp) = c("PO_PC1", "PO_PC2", "barnacle_cyprids")

cyp <- ggplot(data=griddf_cyp, aes(x = PO_PC1, y = PO_PC2)) +
  geom_contour_filled(aes(z=log10(barnacle_cyprids + 1)),
                      breaks = seq(0, 4, 0.2),
                      show.legend = T) + 
  theme_classic() +
  geom_point(data=data, shape=21, 
             col = "black", fill = "white",
             alpha=0, size=2, stroke=1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  geom_vline(xintercept = 0, linetype = "dotted") + 
  scale_fill_viridis_d(option="inferno",
                       name = "Log(x+1) Abundance",
                       drop = F,
                       aesthetics = "fill") +
  ggtitle("Cyprids") +
  guides(fill = guide_colorsteps(show.limits = T,
                                 ticks = T)) + 
  theme(legend.key.height = unit(1.5, 'cm'))


ggarrange(loadings, pter, chaet,larv,copes,
          biv, gastr, plut, naup, cyp,
          nrow = 2, ncol = 5,
          common.legend = T,
          legend.grob = get_legend(pter),
          legend = "right")


image(grid)
abline(h=0, lty="dotted")
abline(v=0, lty="dotted")
contour(grid,add=TRUE)
points(x=data$PO_PC1, y=data$PO_PC2)







