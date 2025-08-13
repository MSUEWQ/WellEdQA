# Setup -------------------------------------------------------------------
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace
library(ggplot2)
library(tidyverse)
library(dplyr)

# import data
lab0 <- read.csv('../1_input/2024LabResults.csv')
reg0 <- read.csv('../1_input/2024RegistrationEntry.csv')



# Set quarters and years --------------------------------------------------
lab0$Year <- as.numeric(substr(lab0$YrQuarter, 1, 2)) + 2000
lab0$Quarter <- substr(lab0$YrQuarter, 3, stop = 4)

reg0$TestYear <- as.numeric(substr(reg0$YrQuarter, 1, 2)) + 2000
reg0$TestQr <- substr(reg0$YrQuarter, 3, 4)

# add reg fields to lab data
reg2lab <- reg0[,c("TrackNum2_primary", "TestYear", "County", "Share_County")]
lab <- merge(lab0,reg2lab, by = "TrackNum2_primary")

# Write 0 for result if not detected -----------------------------------------------
lab[lab$TResult == "ND", "NResult"] <- 0


# Generate stacked boxplots for each analyte with county on Y axis, NResult on X axis, and quarter as fill color----

for (analyte in unique(lab$Analyte)){
  png(paste0("../3_output/Boxplots/StackedBoxplots/", analyte, ".png"))
  analyte_df <- lab[lab$Analyte == analyte,]
  unit <- analyte_df$Runits[1]
  p <- ggplot(analyte_df , aes(x = County, y = NResult)) +
    geom_boxplot() +
    coord_flip() +
    ggtitle(paste(analyte, unit))
  print(p)
  dev.off()
}
