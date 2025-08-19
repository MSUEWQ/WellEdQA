# Setup -------------------------------------------------------------------
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace
library(ggplot2)
library(tidyverse)
library(dplyr)
library(grid)
library(gridExtra)

# import data
lab0 <- read.csv('../1_input/2024LabResults.csv')
reg0 <- read.csv('../1_input/2024RegistrationEntry.csv')

# Assign new/old years
past.years <- c(seq(2015,2023,1))
new.year <- c(2024)


# Add quarters and years --------------------------------------------------
lab0$Year <- as.numeric(substr(lab0$YrQuarter, 1, 2)) + 2000
lab0$Quarter <- substr(lab0$YrQuarter, 3, stop = 4)

reg0$TestYear <- as.numeric(substr(reg0$YrQuarter, 1, 2)) + 2000
reg0$TestQr <- substr(reg0$YrQuarter, 3, 4)

# add reg fields to lab data
reg2lab <- reg0[,c("TrackNum2_primary", "TestYear", "County", "Share_County")]
lab <- merge(lab0,reg2lab, by = "TrackNum2_primary")

# Write 0 for result if not detected 
lab[lab$TResult == "ND", "NResult"] <- 0


# Generate stacked boxplots for each analyte with county on Y axis, NResult on X axis, and quarter as fill color----
# Past data on left, new data on right

for (analyte in unique(lab$Analyte)){
  
  # Set plot layout
  png(1200,600, filename = paste0("../3_output/Boxplots/StackedBoxplots/", analyte, ".png"))
  nf <- layout(matrix(c(1,2), nrow = 1, byrow = TRUE))
  layout.show(nf)
  
  analyte_df <- lab[lab$Analyte == analyte,] # Filter for analyte
  unit <- analyte_df$Runits[1] # Extract unit
  
  # Filter data for new/past year(s)
  analyte_new <- analyte_df[analyte_df$TestYear %in% new.year,]
  analyte_past <- analyte_df[analyte_df$TestYear %in% past.years,]
  
  # Boxplot for old data
  past <- ggplot(analyte_past , aes(x = County, y = NResult)) +
    geom_boxplot() +
    coord_flip() +
    ylab(paste(analyte, unit)) +
    ggtitle("Past Data")
  
  
  # Boxplot for new data
  new <- ggplot(analyte_new , aes(x = County, y = NResult)) +
    geom_boxplot() +
    coord_flip() +
    ylab(paste(analyte, unit))+
    ggtitle("New Data")
  
  # Save
  grid.arrange(past, new, widths = c(1,1))
  dev.off()
}

