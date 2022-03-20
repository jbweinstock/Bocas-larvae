## Bocas plankton community - pulling out % sat. DO
## Date created: 02.07.22
## Date updated: 02.15.22
## Run in R 4.1.1

library(ggplot2)
library(segmented)
#library(reshape2) #dcast
library(zoo) #na.approx, as.Date

# FIRST ATTEMPT, ABANDONED
########
# Read in data and tell R to read dates properly
sonde_data = read.csv("data/sonde_all.csv") #JBW manually combined sonde files for individual dates
sonde_data$Date_.MM.DD.YYYY. = as.Date(sonde_data$Date_.MM.DD.YYYY.,"%Y-%m-%d")

# Fix inconsistent naming
sonde_data$Site_cor = ifelse(sonde_data$Site == "Christobal" | 
                               sonde_data$Site == "CRISTOBAL" | 
                               sonde_data$Site == "Cristobal",
                             yes = "Cristobal", 
                             no = ifelse(sonde_data$Site == "Pastores" | 
                                           sonde_data$Site == "near Pastores" | 
                                           sonde_data$Site == "PASTORES", 
                                         yes = "Pastores", no = "STRI"))

# Add column for depth bin, taking depth in m and rounding to nearest integer
sonde_data$depth_bin = round(sonde_data$Depth_m,0)

# Manually create empty dataframe for 3 dates with no sonde data
missing_dates = as.Date(c("2017-01-24","2017-03-27","2017-06-07"),"%Y-%m-%d")
missing = as.data.frame(matrix(data=NA, nrow=length(missing_dates)*3,
                               ncol=length(sonde_data)))
colnames(missing) = colnames(sonde_data)
missing$Date_.MM.DD.YYYY. = rep(missing_dates,each=3)
missing$Site_cor = rep(unique(sonde_data$Site_cor),3)
missing$Site = missing$Site_cor
missing$Day_night = "day"
missing$depth_bin = 0

# Merge sonde data with empty rows for missing dates
sonde_data_2017 = rbind(sonde_data,missing)

# Average DO by depth bin, site, date, and time of cast (day or night)
# Then, take subset of day casts and split into list by site
DO_aggr = aggregate(ODO_perc_sat~depth_bin+Site_cor+Date_.MM.DD.YYYY.+Day_night,
                    data = sonde_data_2017, FUN = mean, 
                    na.rm = T, na.action = na.pass)
DO_aggr_day = subset.data.frame(DO_aggr,DO_aggr$Day_night=="day")
DO_by_site = split(DO_aggr_day, DO_aggr_day$Site_cor)

# Subset + average +/- 1 m around 11 and 21 m for each site
DO_S_11 = subset.data.frame(DO_by_site$STRI, 
                            DO_by_site$STRI$depth_bin == 10 | 
                              DO_by_site$STRI$depth_bin == 11 |
                              DO_by_site$STRI$depth_bin == 12)
DO_S_11_avg = aggregate(DO_S_11$ODO_perc_sat ~ DO_S_11$Date_.MM.DD.YYYY.,
                        FUN = mean)
DO_S_11_avg$Site = "STRI Point"
DO_S_11_avg$depth_ctgry = 10
colnames(DO_S_11_avg) = c("Date","perc_DO","Site","depth_ctgry")

DO_S_21 = subset.data.frame(DO_by_site$STRI, 
                            DO_by_site$STRI$depth_bin == 20 | 
                              DO_by_site$STRI$depth_bin == 21 |
                              DO_by_site$STRI$depth_bin == 22)
DO_S_21_avg = aggregate(DO_S_21$ODO_perc_sat ~ DO_S_21$Date_.MM.DD.YYYY.,
                        FUN = mean)
DO_S_21_avg$Site = "STRI Point"
DO_S_21_avg$depth_ctgry = 20
colnames(DO_S_21_avg) = c("Date","perc_DO","Site","depth_ctgry")

DO_C_11 = subset.data.frame(DO_by_site$Cristobal,
                            DO_by_site$Cristobal$depth_bin == 10 | 
                              DO_by_site$Cristobal$depth_bin == 11 |
                              DO_by_site$Cristobal$depth_bin == 12)
DO_C_11_avg = aggregate(DO_C_11$ODO_perc_sat ~ DO_C_11$Date_.MM.DD.YYYY.,
                        FUN = mean)
DO_C_11_avg$Site = "Cristobal"
DO_C_11_avg$depth_ctgry = 10
colnames(DO_C_11_avg) = c("Date","perc_DO","Site","depth_ctgry")

DO_C_21 = subset.data.frame(DO_by_site$Cristobal, 
                            DO_by_site$Cristobal$depth_bin == 20 | 
                              DO_by_site$Cristobal$depth_bin == 21 |
                              DO_by_site$Cristobal$depth_bin == 22)
DO_C_21_avg = aggregate(DO_C_21$ODO_perc_sat ~ DO_C_21$Date_.MM.DD.YYYY.,
                        FUN = mean)
DO_C_21_avg$Site = "Cristobal"
DO_C_21_avg$depth_ctgry = 20
colnames(DO_C_21_avg) = c("Date","perc_DO","Site","depth_ctgry")

DO_P_11 = subset.data.frame(DO_by_site$Pastores, 
                            DO_by_site$Pastores$depth_bin == 10 | 
                              DO_by_site$Pastores$depth_bin == 11 |
                              DO_by_site$Pastores$depth_bin == 12)
DO_P_11_avg = aggregate(DO_P_11$ODO_perc_sat ~ DO_P_11$Date_.MM.DD.YYYY.,
                        FUN = mean)
DO_P_11_avg$Site = "Pastores"
DO_P_11_avg$depth_ctgry = 10
colnames(DO_P_11_avg) = c("Date","perc_DO","Site","depth_ctgry")

DO_P_21 = subset.data.frame(DO_by_site$Pastores, 
                            DO_by_site$Pastores$depth_bin == 20 | 
                              DO_by_site$Pastores$depth_bin == 21 |
                              DO_by_site$Pastores$depth_bin == 22)
DO_P_21_avg = aggregate(DO_P_21$ODO_perc_sat ~ DO_P_21$Date_.MM.DD.YYYY.,
                        FUN = mean)
DO_P_21_avg$Site = "Pastores"
DO_P_21_avg$depth_ctgry = 20
colnames(DO_P_21_avg) = c("Date","perc_DO","Site","depth_ctgry")

perc_DO = rbind(DO_S_11_avg, DO_S_21_avg,
                DO_C_11_avg, DO_C_21_avg,
                DO_P_11_avg, DO_P_21_avg)

########

######
## Manually added perc. sat. to count data sheet
data = read.csv("data/combined_counts_percDO.csv",
                na.strings = c("NA",""))
data$Date = as.Date(data$Date,"%m/%d/%y")
data$Site = factor(data$Site,
                   levels = c("STRI Point",
                              "Cristobal",
                              "Pastores"))
data$Depth_ctgry = factor(data$Depth_ctgry)

shallow <- subset.data.frame(data, data$Depth_ctgry == 10)
deep <- subset.data.frame(data, data$Depth_ctgry == 20)
shallow_perc_DO <- shallow$DO
deep_perc_DO <- deep$DO


## Calculate + plot breakpoint for each group, per depth
par(mfrow=c(3,3))
# pteropods
y = log10(shallow$pteropods+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #break at 82%
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="pteropods")
plot(pter_mod_10_seg,add=T,col="blue")
min=5.121-0.425; max = 5.121+0.425
abline(v=min,lty="dotted",col="blue"); abline(v=max,lty="dotted",col="blue")

y = log10(deep$pteropods+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 48% = 3 mg/L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=3.056-0.615; max = 3.056+0.615
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")

# chaetognaths
y = log10(shallow$chaetognaths+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #break at 92% (but opposite dir.)
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="chaetogmaths")
#plot(pter_mod_10_seg,add=T,col="blue")

y = log10(deep$chaetognaths+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 33% = 2 mg/L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=2.048-0.2; max = 2.048+0.2
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")


# larvaceans
y = log10(shallow$larvaceans+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #break at 55% = 3.4 mg/L
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="larvaceans")
plot(pter_mod_10_seg,add=T,col="blue")
min=3.474-0.367; max = 3.474+0.367
abline(v=min,lty="dotted",col="blue"); abline(v=max,lty="dotted",col="blue")

y = log10(deep$larvaceans+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 43% = 2.7 mg/L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=2.856-0.736; max = 2.856+0.736
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")

# copepods
y = log10(shallow$copepods+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #break at 100% (and wrong dir.)
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="copepods")
#plot(pter_mod_10_seg,add=T,col="blue")

y = log10(deep$copepods+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 25% = 1.6 mg/L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=1.575-0.397; max = 1.575+0.397
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")

# bivalves
y = log10(shallow$bivalves+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #break at 96% (wrong dir.)
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="bivalves")
#plot(pter_mod_10_seg,add=T,col="blue")

y = log10(deep$bivalves+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 48% = 3 mg/L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=3.026-0.685; max = 3.026+0.685
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")

# gastropods
y = log10(shallow$gastropods+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #nonsense; break driven by 1 point
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="gastropods")
#plot(pter_mod_10_seg,add=T,col="blue")

y = log10(deep$gastropods+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 37% = 2.3 mg/L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=2.325-0.322; max = 2.325+0.322
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")


# plutei
y = log10(shallow$plutei+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #break at 105% (LOL)
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="plutei")
plot(pter_mod_10_seg,add=T,col="blue")
min=6.37-0.55; max = 6.37+0.55
abline(v=min,lty="dotted",col="blue"); abline(v=max,lty="dotted",col="blue")

y = log10(deep$plutei+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 67% (but steeper slope is >67) = 4.2 mg/L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=4.194-0.445; max = 4.194+0.445
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")


# barnacle nauplii
y = log10(shallow$barnacle_nauplii+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #break at 80% = 5 mg/L (but this is extrapolating)
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="nauplii")
#plot(pter_mod_10_seg,add=T,col="blue")

y = log10(deep$barnacle_nauplii+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 31% = 1.9 mg.L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=1.911-0.224; max = 1.911+0.224
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")

# barnacle cyprids
y = log10(shallow$barnacle_cyprids+1)/379
pter_mod_10 <- lm(y ~ shallow_perc_DO)
pter_mod_10_seg = segmented(pter_mod_10,seg.Z = ~shallow_perc_DO)
summary(pter_mod_10_seg) #break at 68% = 4.3 mg/L
plot(x=shallow_perc_DO,y=y, xlim=c(0,8),col="blue",ylab="cyprids")
#plot(pter_mod_10_seg,add=T,col="blue")

y = log10(deep$barnacle_cyprids+1)/379
pter_mod_20 <- lm(y ~ deep_perc_DO)
pter_mod_20_seg = segmented(pter_mod_20,seg.Z = ~deep_perc_DO)
summary(pter_mod_20_seg) #break at 40% = 2.5 mg/L
points(x=deep_perc_DO,y=y,xlim=c(0,8),col="red")
plot(pter_mod_20_seg,add=T, col="red")
min=2.495-0.383; max = 2.495+0.383
abline(v=min,lty="dotted",col="red"); abline(v=max,lty="dotted",col="red")


# make linear model to relate % DO to mg/L
test = subset.data.frame(data, data$perc_DO < 70)
plot(x=test$DO, y=test$perc_DO,xlim=c(0,5),ylim=c(0,75))
DO_mod <- lm(test$perc_DO ~ test$DO)
summary(DO_mod) # (% DO) = 0.37344 + 15.84125 * DO
DO_mod_x = seq(0,5,0.1)
DO_mod_y = 0.37344 + 15.84125 * DO_mod_x
points(DO_mod_x,DO_mod_y,col="red",type="l")


## Now re-do DO ggplots
#######
pter_mod_10 <- lm((log10(shallow$pteropods + 1)/379) ~ log10(shallow$perc_DO))
pter_mod_20 <- lm((log10(deep$pteropods + 1)/379) ~ log10(deep$perc_DO))

DO1 <- ggplot(data, aes(x=perc_DO, y=log10(pteropods + 1)/379)) + 
  geom_point(aes(fill=Depth_ctgry, shape=Site), 
             alpha=0.4,size=2,col="black") + 
  theme_classic() + 
  geom_smooth(method = "lm", se=F,
              formula = y ~ log(x),
              size=1.5,
              aes(col=Depth_ctgry)) + 
  scale_shape_manual(values = c(21,22,24)) + 
  ggtitle("Pteropods") + 
  ylab("Log( Abundance + 1 ) "~L^-1~"") + 
  annotate("text",label = paste("~R^2==~", "0.10"),
           x=15, y=0.0075, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.05"),
           x=15, y=0.0066, size=3,col="lightseagreen",parse=T) + 
  geom_vline(xintercept = 48, linetype = "dotted")

chaet_mod_10 <- lm((log10(shallow$chaetognaths + 1)/379) ~ log10(shallow$perc_DO))
chaet_mod_20 <- lm((log10(deep$chaetognaths + 1)/379) ~ log10(deep$perc_DO))

DO2 <- ggplot(data, aes(x=perc_DO, y=log10(chaetognaths + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.003"),
           x=110, y=0.0075, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.21"),
           x=110, y=0.0066, size=3,col="lightseagreen",parse=T) + 
  geom_vline(xintercept = 33, linetype = "dotted")


larv_mod_10 <- lm((log10(shallow$larvaceans + 1)/379) ~ log10(shallow$perc_DO))
larv_mod_20 <- lm((log10(deep$larvaceans + 1)/379) ~ log10(deep$perc_DO))

DO3 <- ggplot(data, aes(x=perc_DO, y=log10(larvaceans  + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.19"),
           x=15, y=0.0075, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.37"),
           x=15, y=0.0066, size=3,col="lightseagreen",parse=T) + 
  geom_vline(xintercept = 55, linetype = "dotted") + 
  geom_vline(xintercept = 43, linetype = "dotted")

cope_mod_10 <- lm((log10(shallow$copepods + 1)/379) ~ log10(shallow$perc_DO))
cope_mod_20 <- lm((log10(deep$copepods + 1)/379) ~ log10(deep$perc_DO))

DO4 <- ggplot(data, aes(x=perc_DO, y=log10(copepods + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.003"),
           x=100, y=0.002, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.32"),
           x=100, y=0.001, size=3,col="lightseagreen",parse=T) + 
  geom_vline(xintercept = 25, linetype = "dotted")

biv_mod_10 <- lm((log10(shallow$bivalves + 1)/379) ~ log10(shallow$perc_DO))
biv_mod_20 <- lm((log10(deep$bivalves + 1)/379) ~ log10(deep$perc_DO))

DO5 <- ggplot(data, aes(x=perc_DO, y=log10(bivalves + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.001"),
           x=115, y=0.0018, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.42"),
           x=115, y=0.001, size=3,col="lightseagreen",parse=T) + 
  geom_vline(xintercept = 48, linetype = "dotted")

gastr_mod_10 <- lm((log10(shallow$gastropods + 1)/379) ~ log10(shallow$perc_DO))
gastr_mod_20 <- lm((log10(deep$gastropods + 1)/379) ~ log10(deep$perc_DO))

DO6 <- ggplot(data, aes(x=perc_DO, y=log10(gastropods + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.02"),
           x=15, y=0.0082, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.45"),
           x=15, y=0.0073, size=3,col="lightseagreen",parse=T)+ 
  geom_vline(xintercept = 37, linetype = "dotted")

plut_mod_10 <- lm((log10(shallow$plutei + 1)/379) ~ log10(shallow$perc_DO))
plut_mod_20 <- lm((log10(deep$plutei + 1)/379) ~ (deep$perc_DO)^2)

DO7 <- ggplot(data, aes(x=perc_DO, y=log10(plutei + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.06"),
           x=15, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.22"),
           x=15, y=0.0061, size=3,col="lightseagreen",parse=T) + 
  geom_vline(xintercept = 67, linetype = "dotted")

naup_mod_10 <- lm((log10(shallow$barnacle_nauplii + 1)/379) ~ log10(shallow$perc_DO))
naup_mod_20 <- lm((log10(deep$barnacle_nauplii + 1)/379) ~ log10(deep$perc_DO))

DO8 <- ggplot(data, aes(x=perc_DO, y=log10(barnacle_nauplii + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.001"),
           x=110, y=0.007, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.18"),
           x=110, y=0.0063, size=3,col="lightseagreen",parse=T) + 
  geom_vline(xintercept = 31, linetype = "dotted")

cyp_mod_10 <- lm((log10(shallow$barnacle_cyprids + 1)/379) ~ log10(shallow$perc_DO))
cyp_mod_20 <- lm((log10(deep$barnacle_cyprids + 1)/379) ~ log10(deep$perc_DO))

DO9 <- ggplot(data, aes(x=perc_DO, y=log10(barnacle_cyprids + 1)/379)) + 
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
  annotate("text",label = paste("~R^2==~", "0.005"),
           x=120, y=0.0009, size=3,col="lightcoral",parse=T) + 
  annotate("text",label = paste("~R^2==~", "0.11"),
           x=120, y=0.0003, size=3,col="lightseagreen",parse=T) + 
  geom_vline(xintercept = 40, linetype = "dotted")



ggarrange(DO1, DO2, DO3,
          DO4, DO5, DO6,
          DO7, DO8, DO9,
          nrow=3, ncol=3,
          common.legend = T,
          legend = "bottom")


## Confirming sampling depth
plot(data$Date, data$Depth,ylim=c(24,0))
ggplot(data, aes(x=Date,y=Depth)) + 
  geom_point(aes(size=(copepods),col=Site),
             alpha=1,shape=1,stroke=1) + 
  theme_classic() + ylim(24,0)







