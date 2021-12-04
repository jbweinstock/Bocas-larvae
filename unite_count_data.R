## Bocas plankton community - unite datasets
## Date created: 12.04.21
## Date updated: 12.04.21

library(zoo) #as.Date

data = read.csv("data/Bocas_alldata_8.26.21.csv") #edited so "% Moon" is read as numeric
data$Date = as.Date(data$Date,"%m/%d/%y")

