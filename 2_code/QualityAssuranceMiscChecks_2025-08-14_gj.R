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





# Make sure all reg sheets have unique lab results -------------------------

# Assuming no duplicate reg sheets...
# Each tracking no in reg sheets should map to a unique tracking number in lab results

# Create list of unique tracking nums in reg sheet
unique_track_reg <- unique(reg0$TrackNum2_primary)
# Create list of unique tracking nums in lab results
unique_track_lab <- unique(lab$TrackNum2_primary)

# Initialize list of missing tracking numbers
lab_missing_tracks <- c()

# For each tracking num in reg sheet, find corresponding track num in lab result
# After matching, remove tracking num from both lists
# Repeat until all tracking nums are checked
for (track in unique_track_reg){
  if (track %in% unique_track_lab){
    unique_track_reg <- unique_track_reg[unique_track_reg != track]
    unique_track_lab <- unique_track_lab[unique_track_lab != track]
  } else if (!(track %in% unique_track_lab)){
    # Add tracking number to missing_tracks list
    lab_missing_tracks[length(lab_missing_tracks)+1] <- track
  }
}

if(length(lab_missing_tracks)>0){
  print("Tracking #s not in lab results:")
  print(lab_missing_tracks)
} else if (length(lab_missing_tracks)==0){
  print("No tracking #s missing from lab results")
}





# Make sure all lab results have unique reg sheets -------------------------

# Each unique tracking number in lab results should map to a tracking no in reg sheets
# Same process as above

reg_missing_tracks <- c()

for (track in unique_track_lab){
  if (track %in% unique_track_reg){
    unique_track_lab <- unique_track_reg[unique_track_lab != track]
    unique_track_red <- unique_track_lab[unique_track_reg != track]
  } else if (!(track %in% unique_track_lab)){
    # Add tracking number to missing_tracks list
    reg_missing_tracks[length(reg_missing_tracks)+1] <- track
  }
}

if(length(reg_missing_tracks)>0){
  print("Tracking #s not in reg sheets:")
  print(reg_missing_tracks)
} else if (length(reg_missing_tracks)==0){
  print("No tracking #s missing from reg sheets")
}





# Make sure all wells have corresponding unique reg sheets ---------------------

# Create list of unique well nums in reg sheet
unique_wells_reg <- unique(reg0$TrackNum3_WellCode)
# Create list of unique well nums in well0
unique_wells <- unique(well0$TrackNum3_WellCode)

# Initialize list of missing tracking numbers
well_missing_tracks <- c()

# For each well num in wells, find corresponding well num in reg0
# After matching, remove tracking num from both lists
# Repeat until all well nums are checked
for (track in unique_wells){
  if (track %in% unique_wells_reg){
    unique_wells_reg <- unique_wells_reg[unique_wells_reg != track]
    unique_wells <- unique_wells[unique_wells != track]
  } else if (!(track %in% unique_wells_reg)){
    # Add tracking number to repeated_tracks list
    well_missing_tracks[length(well_missing_tracks)+1] <- track
  }
}

if(length(well_missing_tracks)>0){
  print("Tracking #s not in well results:")
  print(well_missing_tracks)
} else if (length(well_missing_tracks)==0){
  print("No tracking #s missing from well results")
}





# Check for multiple results for given tracking num / analyte combo ------------

# Initialize data frame to store duplicate results
dupe_results <- lab[0,]

# For each tracking number, see if there are any analytes with multiple results
for (track in unique(lab$TrackNum2_primary)){
  track_df <- lab[lab$TrackNum2_primary == track,]
  n_occur <- data.frame(table(track_df$Analyte))
  dupes <- track_df[track_df$Analyte %in% n_occur$Var1[n_occur$Freq > 1], ]
  if (nrow(dupes)>0){
    dupe_results <- rbind(dupe_results, dupes)
  }
}

if (nrow(dupe_results)>0){
  # Save in output folder
  write.csv(dupe_results, paste0("../3_output/duplicate_results.csv"))
  print("Duplicate lab results exist. See duplicate_results.csv in 3_output folder.")
} else if (nrow(dupe_results == 0)){
  print("There are no duplicate lab results.")
}


prim <- lab %>% group_by(TrackNum2_primary, SampleID, Analyte) %>% tally()





# Extract sample descriptions for outliers --------------------------------

get_description <- function(track){
  description <- reg0[reg0$TrackNum2_primary==track, "SampleDescription"]
  return(description)
}
  
  
for (analyte in unique(lab$Analyte)){
  if (analyte == "Coliform, E-Coli" | analyte == "Coliform, Total") next #skip
  analyte_df <- lab[lab$Analyte == analyte,]
  out_vals <- boxplot(analyte_df$NResult, plot=FALSE)$out
  out_tracks <- analyte_df[analyte_df$NResult %in% out_vals, "TrackNum2_primary"]
  out_descriptions <- sapply(out_tracks, get_description)
  
  if (length(out_tracks) > 0){
    # Make an outliers data frame
    outliers <- data.frame(out_tracks, out_vals, out_descriptions)
    outliers$Analyte <- analyte
  
    # Save in outut folder
    write.csv(outliers, paste0("../3_output/Outliers/", analyte, ".csv"))
  }
  
}





# Compile reg sheet details for those who marked “No treatment system” but then marked some kind of treatment 

no_treat <- reg0[reg0$SampleTreatedBeforeTesting == "No Treatment System",]
contradicting_treatment <- no_treat[no_treat$TreatedWith != "No Data",]

# Save
if (nrow(contradicting_treatment)>0){
  write.csv(contradicting_treatment,"../3_output/contradicting_treatment.csv")
  print('There are some "untreated" samples which might be treated. See contradicting_treatment.csv in 3_output folder.')
}





# Look for "spring", "city water", "pond", "ditch", "drainage", "cistern" in description ----------

flag_words <- c("spring", "city", "municipal", "pond", "ditch", "drainage", "cistern")

# Look at entries that are marked as being wells
wells <- reg0[reg0$NotAWell != "Not a Well",]

# Initialize data frame for suspicious reg sheets
maybe_not_wells <- wells[0,]

# If a flag word is found within a sample description, add the reg sheet info to prob_not_wells
for (word in flag_words){
  sus <- wells[grepl(word, wells$SampleDescription, fixed = TRUE),]
  maybe_not_wells <- rbind(maybe_not_wells, sus)
}

# Remove duplicate rows
maybe_not_wells <- maybe_not_wells[!duplicated(maybe_not_wells),]

# Save
if (nrow(maybe_not_wells)>0){
  write.csv(maybe_not_wells,"../3_output/maybe_not_wells.csv")
  print('There are some suspicious "wells." See maybe_not_wells.csv in 3_output folder.')
}

