## Bocas plankton community - sonde data time series figure
## Date created: 07.08.21
## Date copied: 12.04.21
## Date updated: 02.27.22
## Run in R 4.1.1

library(ggplot2)
library(reshape2) #dcast
library(zoo) #na.approx, as.Date
library(ggpubr) ##ggarrange

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

# Remove Sept 13 data (corrupted)
for(i in 9:29){
  sonde_data_2017[,i] = ifelse(sonde_data_2017$Date_.MM.DD.YYYY. == as.Date(c("2017-09-13"),"%Y-%m-%d"),
                                  yes = NA, no = sonde_data_2017[,i])
}


# Average each parameter of interest by depth bin, site, date, and time of cast (day or night)
# Then, take subset of day casts and split into list by site
DO_aggr = aggregate(ODO_mg.L~depth_bin+Site_cor+Date_.MM.DD.YYYY.+Day_night,
                 data = sonde_data_2017, FUN = mean, 
                 na.rm = T, na.action = na.pass)
DO_aggr_day = subset.data.frame(DO_aggr,DO_aggr$Day_night=="day")
DO_by_site = split(DO_aggr_day, DO_aggr_day$Site_cor)

temp_aggr = aggregate(Temp_C~depth_bin+Site_cor+Date_.MM.DD.YYYY.+Day_night,
                   data = sonde_data_2017, FUN = mean, 
                   na.rm = T, na.action = na.pass)
temp_aggr_day = subset.data.frame(temp_aggr, temp_aggr$Day_night=="day")
temp_by_site = split(temp_aggr_day, temp_aggr_day$Site_cor)

sal_aggr = aggregate(Sal_psu~depth_bin+Site_cor+Date_.MM.DD.YYYY.+Day_night,
                   data = sonde_data_2017, FUN = mean, 
                   na.rm = T, na.action = na.pass)
sal_aggr_day = subset.data.frame(sal_aggr, sal_aggr$Day_night=="day")
sal_by_site = split(sal_aggr_day, sal_aggr_day$Site_cor)

chl_aggr = aggregate(Chlorophyll_microg.L~depth_bin+Site_cor+Date_.MM.DD.YYYY.+Day_night,
                   data = sonde_data_2017, FUN = mean, 
                   na.rm = T, na.action = na.pass)
chl_aggr_day = subset.data.frame(chl_aggr, chl_aggr$Day_night=="day")
chl_by_site = split(chl_aggr_day, chl_aggr_day$Site_cor)

DOM_aggr = aggregate(fDOM_QSU~depth_bin+Site_cor+Date_.MM.DD.YYYY.+Day_night,
                     data = sonde_data_2017, FUN = mean, 
                     na.rm = T, na.action = na.pass)
DOM_aggr_day = subset.data.frame(DOM_aggr, DOM_aggr$Day_night=="day")
DOM_by_site = split(DOM_aggr_day, DOM_aggr_day$Site_cor)

## Change from long to wide format (depths = rows, date OR day/night = columns, [PARAMETER] = values)
# First DO!
DO_S_wide = dcast(DO_by_site$STRI, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                    value.var = "ODO_mg.L")
DO_S_wide$`2017-01-24`[1] = NA
DO_S_wide$`2017-03-27`[1] = NA
DO_S_wide$`2017-06-07`[1] = NA
DO_C_wide = dcast(DO_by_site$Cristobal, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "ODO_mg.L")
DO_C_wide$`2017-01-24`[1] = NA
DO_C_wide$`2017-03-27`[1] = NA
DO_C_wide$`2017-06-07`[1] = NA
DO_P_wide = dcast(DO_by_site$Pastores, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "ODO_mg.L")
DO_P_wide$`2017-01-24`[1] = NA
DO_P_wide$`2017-03-27`[1] = NA
DO_P_wide$`2017-06-07`[1] = NA


## Interpolate NAs by row (depth) and column (date)
# STRI
for(i in 4:length(DO_S_wide)){
  DO_S_wide[,i] = na.approx(DO_S_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(DO_S_wide$depth_bin)){
  nums = as.matrix(DO_S_wide[i,4:length(DO_S_wide)])
  dim(nums) = c(length(nums),1)
  DO_S_wide[i,4:length(DO_S_wide)] = na.approx(nums,method="linear",na.rm=F)
}
# Cristobal
for(i in 4:length(DO_C_wide)){
  DO_C_wide[,i] = na.approx(DO_C_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(DO_C_wide$depth_bin)){
  nums = as.matrix(DO_C_wide[i,4:length(DO_C_wide)])
  dim(nums) = c(length(nums),1)
  DO_C_wide[i,4:length(DO_C_wide)] = na.approx(nums,method="linear",na.rm=F)
}
#Pastores
for(i in 4:length(DO_P_wide)){
  DO_P_wide[,i] = na.approx(DO_P_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(DO_P_wide$depth_bin)){
  nums = as.matrix(DO_P_wide[i,4:length(DO_P_wide)])
  dim(nums) = c(length(nums),1)
  DO_P_wide[i,4:length(DO_P_wide)] = na.approx(nums,method="linear",na.rm=F)
}

## And back to long form
DO_S_long = melt(DO_S_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
DO_S_long$variable = as.Date(DO_S_long$variable,"%Y-%m-%d")

DO_C_long = melt(DO_C_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
DO_C_long$variable = as.Date(DO_C_long$variable,"%Y-%m-%d")

DO_P_long = melt(DO_P_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
DO_P_long$variable = as.Date(DO_P_long$variable,"%Y-%m-%d")


## Plots
DO_STRI = levelplot(value ~ variable * depth_bin, data = DO_S_long,
               panel = panel.levelplot.points,
               ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                       as.Date("2017-12-14","%Y-%m-%d")),
               main = "STRI DO",cex=2, pch = 22,
               at = seq(0,8,length.out = 32),
               #at = seq(26,33,length.out = 32),
               #at = seq(29,37,length.out = 32),
               #at = seq(-4,4,length.out = 32),
               #at = seq(7.5,10,length.out = 32),
               col.regions = hcl.colors(32, palette = "PuOr",rev=F))

DO_Crist = levelplot(value ~ variable * depth_bin, data = DO_C_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "Cristobal DO",cex=2, pch = 22,
                    at = seq(0,8,length.out = 32),
                    #at = seq(26,33,length.out = 32),
                    #at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    #at = seq(7.5,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "PuOr",rev=F))

DO_Past = levelplot(value ~ variable * depth_bin, data = DO_P_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "Pastores DO",cex=2, pch = 22,
                    at = seq(0,8,length.out = 32),
                    #at = seq(26,33,length.out = 32),
                    #at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    #at = seq(7.5,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "PuOr",rev=F))

Sonde_DO <- ggarrange(DO_STRI,DO_Crist,DO_Past,
                      nrow = 1, ncol = 3)

# Temperature!
Temp_S_wide = dcast(temp_by_site$STRI, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Temp_C")
Temp_S_wide$`2017-01-24`[1] = NA
Temp_S_wide$`2017-03-27`[1] = NA
Temp_S_wide$`2017-06-07`[1] = NA
Temp_C_wide = dcast(temp_by_site$Cristobal, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Temp_C")
Temp_C_wide$`2017-01-24`[1] = NA
Temp_C_wide$`2017-03-27`[1] = NA
Temp_C_wide$`2017-06-07`[1] = NA
Temp_P_wide = dcast(temp_by_site$Pastores, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Temp_C")
Temp_P_wide$`2017-01-24`[1] = NA
Temp_P_wide$`2017-03-27`[1] = NA
Temp_P_wide$`2017-06-07`[1] = NA


## Interpolate NAs by row (depth) and column (date)
# STRI
for(i in 4:length(Temp_S_wide)){
  Temp_S_wide[,i] = na.approx(Temp_S_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Temp_S_wide$depth_bin)){
  nums = as.matrix(Temp_S_wide[i,4:length(Temp_S_wide)])
  dim(nums) = c(length(nums),1)
  Temp_S_wide[i,4:length(Temp_S_wide)] = na.approx(nums,method="linear",na.rm=F)
}
# Cristobal
for(i in 4:length(Temp_C_wide)){
  Temp_C_wide[,i] = na.approx(Temp_C_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Temp_C_wide$depth_bin)){
  nums = as.matrix(Temp_C_wide[i,4:length(Temp_C_wide)])
  dim(nums) = c(length(nums),1)
  Temp_C_wide[i,4:length(Temp_C_wide)] = na.approx(nums,method="linear",na.rm=F)
}
#Pastores
for(i in 4:length(Temp_P_wide)){
  Temp_P_wide[,i] = na.approx(Temp_P_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Temp_P_wide$depth_bin)){
  nums = as.matrix(Temp_P_wide[i,4:length(Temp_P_wide)])
  dim(nums) = c(length(nums),1)
  Temp_P_wide[i,4:length(Temp_P_wide)] = na.approx(nums,method="linear",na.rm=F)
}

## And back to long form
Temp_S_long = melt(Temp_S_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Temp_S_long$variable = as.Date(Temp_S_long$variable,"%Y-%m-%d")

Temp_C_long = melt(Temp_C_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Temp_C_long$variable = as.Date(Temp_C_long$variable,"%Y-%m-%d")

Temp_P_long = melt(Temp_P_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Temp_P_long$variable = as.Date(Temp_P_long$variable,"%Y-%m-%d")


## Plots
Temp_STRI = levelplot(value ~ variable * depth_bin, data = Temp_S_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "STRI Temperature",cex=2, pch = 22,
                    #at = seq(0,8,length.out = 32),
                    at = seq(26,33,length.out = 32),
                    #at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    #at = seq(7.5,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "Spectral",rev=T))

Temp_Crist = levelplot(value ~ variable * depth_bin, data = Temp_C_long,
                     panel = panel.levelplot.points,
                     ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                             as.Date("2017-12-14","%Y-%m-%d")),
                     main = "Cristobal Temperature",cex=2, pch = 22,
                     #at = seq(0,8,length.out = 32),
                     at = seq(26,33,length.out = 32),
                     #at = seq(29,37,length.out = 32),
                     #at = seq(-4,4,length.out = 32),
                     #at = seq(7.5,10,length.out = 32),
                     col.regions = hcl.colors(32, palette = "Spectral",rev=T))
Temp_Past = levelplot(value ~ variable * depth_bin, data = Temp_P_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "Pastores Temperature",cex=2, pch = 22,
                    #at = seq(0,8,length.out = 32),
                    at = seq(26,33,length.out = 32),
                    #at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    #at = seq(7.5,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "Spectral",rev=T))

Sonde_Temp <- ggarrange(Temp_STRI,Temp_Crist,Temp_Past,
                      nrow = 1, ncol = 3)

# Salinity!
Sal_S_wide = dcast(sal_by_site$STRI, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Sal_psu")
Sal_S_wide$`2017-01-24`[1] = NA
Sal_S_wide$`2017-03-27`[1] = NA
Sal_S_wide$`2017-06-07`[1] = NA
Sal_C_wide = dcast(sal_by_site$Cristobal, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Sal_psu")
Sal_C_wide$`2017-01-24`[1] = NA
Sal_C_wide$`2017-03-27`[1] = NA
Sal_C_wide$`2017-06-07`[1] = NA
Sal_P_wide = dcast(sal_by_site$Pastores, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Sal_psu")
Sal_P_wide$`2017-01-24`[1] = NA
Sal_P_wide$`2017-03-27`[1] = NA
Sal_P_wide$`2017-06-07`[1] = NA


## Interpolate NAs by row (depth) and column (date)
# STRI
for(i in 4:length(Sal_S_wide)){
  Sal_S_wide[,i] = na.approx(Sal_S_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Sal_S_wide$depth_bin)){
  nums = as.matrix(Sal_S_wide[i,4:length(Sal_S_wide)])
  dim(nums) = c(length(nums),1)
  Sal_S_wide[i,4:length(Sal_S_wide)] = na.approx(nums,method="linear",na.rm=F)
}
# Cristobal
for(i in 4:length(Sal_C_wide)){
  Sal_C_wide[,i] = na.approx(Sal_C_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Sal_C_wide$depth_bin)){
  nums = as.matrix(Sal_C_wide[i,4:length(Sal_C_wide)])
  dim(nums) = c(length(nums),1)
  Sal_C_wide[i,4:length(Sal_C_wide)] = na.approx(nums,method="linear",na.rm=F)
}
#Pastores
for(i in 4:length(Sal_P_wide)){
  Sal_P_wide[,i] = na.approx(Sal_P_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Sal_P_wide$depth_bin)){
  nums = as.matrix(Sal_P_wide[i,4:length(Sal_P_wide)])
  dim(nums) = c(length(nums),1)
  Sal_P_wide[i,4:length(Sal_P_wide)] = na.approx(nums,method="linear",na.rm=F)
}

## And back to long form
Sal_S_long = melt(Sal_S_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Sal_S_long$variable = as.Date(Sal_S_long$variable,"%Y-%m-%d")

Sal_C_long = melt(Sal_C_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Sal_C_long$variable = as.Date(Sal_C_long$variable,"%Y-%m-%d")

Sal_P_long = melt(Sal_P_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Sal_P_long$variable = as.Date(Sal_P_long$variable,"%Y-%m-%d")


## Plots
Sal_STRI = levelplot(value ~ variable * depth_bin, data = Sal_S_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "STRI salinity",cex=2, pch = 22,
                    #at = seq(0,8,length.out = 32),
                    #at = seq(26,33,length.out = 32),
                    at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    #at = seq(7.5,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "Blues",rev=T))

Sal_Crist = levelplot(value ~ variable * depth_bin, data = Sal_C_long,
                     panel = panel.levelplot.points,
                     ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                             as.Date("2017-12-14","%Y-%m-%d")),
                     main = "Cristobal salinity",cex=2, pch = 22,
                     #at = seq(0,8,length.out = 32),
                     #at = seq(26,33,length.out = 32),
                     at = seq(29,37,length.out = 32),
                     #at = seq(-4,4,length.out = 32),
                     #at = seq(7.5,10,length.out = 32),
                     col.regions = hcl.colors(32, palette = "Blues",rev=T))
Sal_Past = levelplot(value ~ variable * depth_bin, data = Sal_P_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "Pastores salinity",cex=2, pch = 22,
                    #at = seq(0,8,length.out = 32),
                    #at = seq(26,33,length.out = 32),
                    at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    #at = seq(7.5,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "Blues",rev=T))

Sonde_sal <- ggarrange(Sal_STRI,Sal_Crist,Sal_Past,
                      nrow = 1, ncol = 3)

# Chlorophyll!
Chl_S_wide = dcast(chl_by_site$STRI, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Chlorophyll_microg.L")
Chl_S_wide$`2017-01-24`[1] = NA
Chl_S_wide$`2017-03-27`[1] = NA
Chl_S_wide$`2017-06-07`[1] = NA
Chl_C_wide = dcast(chl_by_site$Cristobal, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Chlorophyll_microg.L")
Chl_C_wide$`2017-01-24`[1] = NA
Chl_C_wide$`2017-03-27`[1] = NA
Chl_C_wide$`2017-06-07`[1] = NA
Chl_P_wide = dcast(chl_by_site$Pastores, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "Chlorophyll_microg.L")
Chl_P_wide$`2017-01-24`[1] = NA
Chl_P_wide$`2017-03-27`[1] = NA
Chl_P_wide$`2017-06-07`[1] = NA


## Interpolate NAs by row (depth) and column (date)
# STRI
for(i in 4:length(Chl_S_wide)){
  Chl_S_wide[,i] = na.approx(Chl_S_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Chl_S_wide$depth_bin)){
  nums = as.matrix(Chl_S_wide[i,4:length(Chl_S_wide)])
  dim(nums) = c(length(nums),1)
  Chl_S_wide[i,4:length(Chl_S_wide)] = na.approx(nums,method="linear",na.rm=F)
}
# Cristobal
for(i in 4:length(Chl_C_wide)){
  Chl_C_wide[,i] = na.approx(Chl_C_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Chl_C_wide$depth_bin)){
  nums = as.matrix(Chl_C_wide[i,4:length(Chl_C_wide)])
  dim(nums) = c(length(nums),1)
  Chl_C_wide[i,4:length(Chl_C_wide)] = na.approx(nums,method="linear",na.rm=F)
}
#Pastores
for(i in 4:length(Chl_P_wide)){
  Chl_P_wide[,i] = na.approx(Chl_P_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(Chl_P_wide$depth_bin)){
  nums = as.matrix(Chl_P_wide[i,4:length(Chl_P_wide)])
  dim(nums) = c(length(nums),1)
  Chl_P_wide[i,4:length(Chl_P_wide)] = na.approx(nums,method="linear",na.rm=F)
}

## And back to long form
Chl_S_long = melt(Chl_S_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Chl_S_long$variable = as.Date(Chl_S_long$variable,"%Y-%m-%d")

Chl_C_long = melt(Chl_C_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Chl_C_long$variable = as.Date(Chl_C_long$variable,"%Y-%m-%d")

Chl_P_long = melt(Chl_P_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
Chl_P_long$variable = as.Date(Chl_P_long$variable,"%Y-%m-%d")


## Plots
Chl_STRI = levelplot((value) ~ variable * depth_bin, data = Chl_S_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "STRI chlorophyll",cex=2, pch = 22,
                    #at = seq(0,8,length.out = 32),
                    #at = seq(26,33,length.out = 32),
                    #at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    at = seq(0,15,length.out = 32),
                    #at = seq(7.5,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "viridis",rev=F))

Chl_Crist = levelplot((value) ~ variable * depth_bin, data = Chl_C_long,
                     panel = panel.levelplot.points,
                     ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                             as.Date("2017-12-14","%Y-%m-%d")),
                     main = "Cristobal chlorophyll",cex=2, pch = 22,
                     #at = seq(0,8,length.out = 32),
                     #at = seq(26,33,length.out = 32),
                     #at = seq(29,37,length.out = 32),
                     #at = seq(-4,4,length.out = 32),
                     at = seq(0,15,length.out = 32),
                     #at = seq(7.5,10,length.out = 32),
                     col.regions = hcl.colors(32, palette = "viridis",rev=F))

Chl_Past = levelplot((value) ~ variable * depth_bin, data = Chl_P_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "Pastores chlorophyll",cex=2, pch = 22,
                    #at = seq(0,8,length.out = 32),
                    #at = seq(26,33,length.out = 32),
                    #at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    at = seq(0,15,length.out = 32),
                    #at = seq(7.5,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "viridis",rev=F))

Sonde_Chl <- ggarrange(Chl_STRI,Chl_Crist,Chl_Past,
                      nrow = 1, ncol = 3)


# DOM!
DOM_S_wide = dcast(DOM_by_site$STRI, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "fDOM_QSU")
DOM_S_wide$`2017-01-24`[1] = NA
DOM_S_wide$`2017-03-27`[1] = NA
DOM_S_wide$`2017-06-07`[1] = NA
DOM_C_wide = dcast(DOM_by_site$Cristobal, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "fDOM_QSU")
DOM_C_wide$`2017-01-24`[1] = NA
DOM_C_wide$`2017-03-27`[1] = NA
DOM_C_wide$`2017-06-07`[1] = NA
DOM_P_wide = dcast(DOM_by_site$Pastores, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                  value.var = "fDOM_QSU")
DOM_P_wide$`2017-01-24`[1] = NA
DOM_P_wide$`2017-03-27`[1] = NA
DOM_P_wide$`2017-06-07`[1] = NA


## Interpolate NAs by row (depth) and column (date)
# STRI
for(i in 4:length(DOM_S_wide)){
  DOM_S_wide[,i] = na.approx(DOM_S_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(DOM_S_wide$depth_bin)){
  nums = as.matrix(DOM_S_wide[i,4:length(DOM_S_wide)])
  dim(nums) = c(length(nums),1)
  DOM_S_wide[i,4:length(DOM_S_wide)] = na.approx(nums,method="linear",na.rm=F)
}
# Cristobal
for(i in 4:length(DOM_C_wide)){
  DOM_C_wide[,i] = na.approx(DOM_C_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(DOM_C_wide$depth_bin)){
  nums = as.matrix(DOM_C_wide[i,4:length(DOM_C_wide)])
  dim(nums) = c(length(nums),1)
  DOM_C_wide[i,4:length(DOM_C_wide)] = na.approx(nums,method="linear",na.rm=F)
}
#Pastores
for(i in 4:length(DOM_P_wide)){
  DOM_P_wide[,i] = na.approx(DOM_P_wide[,i],method="linear",na.rm=F)
}
for(i in 1:length(DOM_P_wide$depth_bin)){
  nums = as.matrix(DOM_P_wide[i,4:length(DOM_P_wide)])
  dim(nums) = c(length(nums),1)
  DOM_P_wide[i,4:length(DOM_P_wide)] = na.approx(nums,method="linear",na.rm=F)
}

## And back to long form
DOM_S_long = melt(DOM_S_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
DOM_S_long$variable = as.Date(DOM_S_long$variable,"%Y-%m-%d")

DOM_C_long = melt(DOM_C_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
DOM_C_long$variable = as.Date(DOM_C_long$variable,"%Y-%m-%d")

DOM_P_long = melt(DOM_P_wide,id.vars = c("Day_night","depth_bin","Site_cor"))
DOM_P_long$variable = as.Date(DOM_P_long$variable,"%Y-%m-%d")


## Plots
DOM_STRI = levelplot((value) ~ variable * depth_bin, data = DOM_S_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "STRI DOM",cex=2, pch = 22,
                    #at = seq(0,8,length.out = 32),
                    #at = seq(26,33,length.out = 32),
                    #at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    #at = seq(-5.1,4.2,length.out = 32),
                    at = seq(-1,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "PuBuGn",rev=F))

DOM_Crist = levelplot((value) ~ variable * depth_bin, data = DOM_C_long,
                     panel = panel.levelplot.points,
                     ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                             as.Date("2017-12-14","%Y-%m-%d")),
                     main = "Cristobal DOM",cex=2, pch = 22,
                     #at = seq(0,8,length.out = 32),
                     #at = seq(26,33,length.out = 32),
                     #at = seq(29,37,length.out = 32),
                     #at = seq(-4,4,length.out = 32),
                     #at = seq(-5.1,4.2,length.out = 32),
                     at = seq(-1,10,length.out = 32),
                     col.regions = hcl.colors(32, palette = "PuBuGn",rev=F))

DOM_Past = levelplot((value) ~ variable * depth_bin, data = DOM_P_long,
                    panel = panel.levelplot.points,
                    ylim = c(25,-1), xlim=c(as.Date("2017-01-01","%Y-%m-%d"),
                                            as.Date("2017-12-14","%Y-%m-%d")),
                    main = "Pastores DOM",cex=2, pch = 22,
                    #at = seq(0,8,length.out = 32),
                    #at = seq(26,33,length.out = 32),
                    #at = seq(29,37,length.out = 32),
                    #at = seq(-4,4,length.out = 32),
                    #at = seq(-5.1,4.2,length.out = 32),
                    at = seq(-1,10,length.out = 32),
                    col.regions = hcl.colors(32, palette = "PuBuGn",rev=F))

Sonde_DOM <- ggarrange(DOM_STRI,DOM_Crist,DOM_Past,
                      nrow = 1, ncol = 3)

Sonde_DO
Sonde_Temp
Sonde_sal
Sonde_Chl
Sonde_DOM

