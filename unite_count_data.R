## Bocas plankton community - unite datasets
## Date created: 12.04.21
## Date updated: 12.04.21

# Groups in 'data' were all counted by either JBW (fish, bivalves, gastropods, 
  # crabs, shrimp/mysids, plutei, barnacle cyprids) or LV (pteropods, chaetognaths,
  # larvaceans, barnacle nauplii). Copepods were counted later, by various RC Lab
  # members - initially split into 'calanoid copepods' and 'other copepods'. However,
  # due to possible discrepancies between counters, 'calanoids' and 'other copepods'
  # are combined for analyses.

# Load data
data = read.csv("data/Bocas_alldata_8.26.21.csv") #edited so "% Moon" is read as numeric
copepod_Isis_counts = read.csv("data/copepod_Isiscounts_08-2021.csv",
                               na.strings = c("","NA","dry"))
copepod_remaining_counts = read.csv("data/copepod_counts_09-2021_v2.csv",
                                    na.strings = c("","NA"))
  # Isis counted full volume, while all values in copepod_remaining_counts are split into
    # counts from subsample and counts for remainder of sample

# Rename columns to be less annoying
colnames(data) = c("Week","Code","Date","Site","Depth_ctgry","Depth","Depth_SE",
                   "fish","pteropods","chaetognaths","larvaceans","bivalves",
                   "gastropods", "crab_zoeae","shrimp_mysids","plutei",
                   "barnacle_nauplii","barnacle_cyprids","NOTES",
                   "perc_moon","tide_phase","DO","DO_SE","temp_C","temp_SE",
                   "salinity","sal_SE","chlorophyll","chlor_SE","pH","pH_SE",
                   "turbidity","turbidity_SE","DOM","DOM_SE","BGA","BGA_SE")

# Add columns for copepod counts in data (Jane and Lourdes counts)
data$copepods = NA; data$copepod_counter = NA

# Unite first 2 dataframes by matching sample code
for(i in 1:length(data$Code)){
  for(k in 1:length(copepod_remaining_counts$ï..Code)){
    if(data$Code[i]==copepod_remaining_counts$ï..Code[k]){
      data$copepods[i] = sum(copepod_remaining_counts$Calanoids.in.first.subsample[k], 
                             copepod_remaining_counts$Other.copepods.in.first.subsample[k],
                             copepod_remaining_counts$Total.copepods.in.the.rest.of.the.sample._.do.not.include.molts[k],
                             na.rm = T)
      data$copepod_counter[i] = copepod_remaining_counts$Who.counted.first.subsample[k]
    }
  }
}

# Bring in 3rd dataframe by sample code and copepod counter (AKA incorporate Isis' counts)
for(i in 1:length(data$Code)){
  for(k in 1:length(copepod_Isis_counts$Code)){
    if(data$Code[i]==copepod_Isis_counts$Code[k] & is.na(data$copepod_counter[i]) == F & data$copepod_counter[i] == "Isis"){
        # Counts by Isis were not divided into subsamples, so only have to add calanoids and others
      data$copepods[i] = 
        copepod_Isis_counts$Original.dataset.Calanoyds[k] + 
        copepod_Isis_counts$Original.dataset.Copepod[k]
    }
    if(data$Code[i] != copepod_Isis_counts$Code[k] | is.na(data$copepod_counter[i]) == T | data$copepod_counter[i] != "Isis"){
      data$copepods[i] = data$copepods[i]
    }
  }
}

# Clean up odds and ends
data$copepod_counter[140] = "Tania"
data$copepod_counter[42] = "Tania"
data$copepod_counter[29] = "Alisson"

# Write combo dataframe (only run once)
write.csv(data,"data/combined_counts.csv")
