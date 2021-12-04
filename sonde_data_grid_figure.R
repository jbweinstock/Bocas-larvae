## Bocas plankton community - sonde data time series figure
## Date created: 07.08.21
## Date copied: 12.04.21
## Date updated: 12.04.21
## Run in R 4.1.1

library(ggplot2)
library(reshape2) #dcast
library(zoo) #na.approx, as.Date
library(latticeExtra) #levelplot

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

# Add column for deoth bin, taking depth in m and rounding to nearest integer
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


DO_P_wide = dcast(DO_Past_day, Day_night + depth_bin + Site_cor ~ Date_.MM.DD.YYYY.,
                    value.var = "ODO_mg.L")
# need long > wide format (depths = rows, date OR day/night = columns, DO = values)




