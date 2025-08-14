# Setup -------------------------------------------------------------------
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace
library(ggplot2)
library(gridExtra)
library(dplyr)


# import data
lab0 <- read.csv('../1_input/2024LabResults.csv')
reg0 <- read.csv('../1_input/2024RegistrationEntry.csv')
well0 <- read.csv('../1_input/2024Wells.csv')



# Compile and format data -------------------------------------------------

# Add test year and test quarter to reg0
reg0$TestYear <- as.numeric(substr(reg0$YrQuarter, 1, 2)) + 2000
reg0$TestQr <- substr(reg0$YrQuarter, 3, 4)

# add reg fields to lab data
reg2lab <- reg0[,c("TrackNum2_primary", "TestYear", "County", "Share_County")]
lab <- merge(lab0,reg2lab, by = "TrackNum2_primary")

# Write 0 for non-detects
lab[lab$TResult == "ND", 'NResult'] <- 0




# Check for duplicate tracking numbers in reg sheet entries -----------------

# Each reg sheet should have a unique tracking number
# If there are duplicates, then the # of rows of reg0 > # of unique tracking numbers 

if (length(unique(reg0$TrackNum2_primary)) != nrow(reg0)) { 
  n_occur <- data.frame(table(reg0$TrackNum2_primary))
  duplicates <- reg0[reg0$TrackNum2_primary %in% n_occur$Var1[n_occur$Freq > 1],] %>% 
    select(c("NameLast", "NameFirst", "TrackNum2_primary"))
  print("Duplicate Primary Tracking Numbers:")
  print(duplicates)
  errorCondition("Duplicate tracking numbers in reg0 data frame")
# If no duplicates, print "No duplicates"
  } else if (length(unique(reg0$TrackNum2_primary)) == nrow(reg0)){
  print("No duplicate primary tracking numbers")
}



# Make sure all reg sheets have unique lab results and vice versa --------------

# Assuming no duplicate reg sheets...
# Each tracking no in reg sheets should map to a unique tracking number in lab results

# Create list of unique tracking nums in reg sheet
unique_track_reg <- unique(reg0$TrackNum2_primary)
# Create list of unique tracking nums in lab results
unique_track_lab <- unique(lab$TrackNum2_primary)

# For each tracking num in reg sheet, find corresponding track num in lab result
# After matching, remove tracking num from both lists
# Repeat until all tracking nums are checked
for (track in unique_track_reg){
  if (track %in% unique_track_lab){
    unique_track_reg <- unique_track_reg[unique_track_reg != track]
    unique_track_lab <- unique_track_lab[unique_track_lab != track]
  } else if (!(track %in% unique_track_lab)){
    print(paste("Tracking #", track, "not in lab results"))
  }
}

# And each unique tracking number in lab results should map to a tracking no in reg sheets
# same process as above
for (track in unique_track_lab){
  if (track %in% unique_track_reg){
    unique_track_lab <- unique_track_reg[unique_track_lab != track]
    unique_track_red <- unique_track_lab[unique_track_reg != track]
  } else if (!(track %in% unique_track_lab)){
    print(paste("Tracking #", track, "not in reg sheets"))
  }
}



# Make sure all wells have corresponding unique reg sheets ---------

# Create list of unique well nums in reg sheet
unique_wells_reg <- unique(reg0$TrackNum3_WellCode)
# Create list of unique well nums in well0
unique_wells <- unique(well0$TrackNum3_WellCode)

# For each well num in wells, find corresponding well num in reg0
# After matching, remove tracking num from both lists
# Repeat until all well nums are checked
for (track in unique_wells){
  if (track %in% unique_wells_reg){
    unique_wells_reg <- unique_wells_reg[unique_wells_reg != track]
    unique_wells <- unique_wells[unique_wells != track]
  } else if (!(track %in% unique_wells_reg)){
    print(paste("Well #", track, "not in reg sheets"))
  }
}



# Extract sample descriptions for outliers --------------------------------

for (analyte in unique(lab$Analyte)){
  if (analyte == "Coliform, E-Coli" | analyte == "Coliform, Total" | 
      analyte == "Nitrogen, Nitrate+Nitrite as N") next #skip
  analyte_df <- lab[lab$Analyte == analyte,]
  out_vals <- boxplot(analyte_df$NResult, plot=FALSE)$out
  out_tracks <- analyte_df[analyte_df$NResult %in% out_vals, "TrackNum2_primary"]
  out_descriptions <- reg0[reg0$TrackNum2_primary %in% out_tracks, "SampleDescription"]
  
  if (length(out_tracks) > 0){
    # Make an outliers data frame
    outliers <- data.frame(out_tracks, out_vals, out_descriptions)
    outliers$Analyte <- analyte
  
    # Save in outut folder
    write.csv(outliers, paste0("../3_output/Outliers/", analyte, ".csv"))
  }
  
}



