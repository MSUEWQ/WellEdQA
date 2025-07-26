# Setup -------------------------------------------------------------------
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace
library(ggplot2)
library(tidyverse)
library(dplyr)

# import data
lab0 <- read.csv('../1_input/2024LabResults.csv')
reg0 <- read.csv('../1_input/2024RegSheets.csv')



# Set quarters and years --------------------------------------------------
lab0$Year <- paste0("20", substr(lab0$YrQuarter, start = 1, stop = 2))
lab0$Quarter <- substr(lab0$YrQuarter, start = 3, stop = 4)


# Write 0 for result if not detected -----------------------------------------------
lab0[lab0$TResult == "ND", "NResult"] <- 0


# Group by quarter and year -----------------------------------------------

lab_grouped <- lab0 %>% group_by(Quarter, Year, Analyte)

df <- lab_grouped %>% summarise(NResult)

for (analyte in unique(df$Analyte)){
  png(paste0("../3_output/Boxplots/", analyte, ".png"))
  analyte_df <- df[df$Analyte == analyte,]
  p <- ggplot(analyte_df, aes(x = Quarter, y = NResult)) +
    geom_boxplot() +
    coord_flip() +
    ggtitle(analyte) 
  print(p)
  dev.off()
}

ggplot(diamonds , aes(x = cut, y = depth, fill = color)) +
  geom_boxplot() +
  coord_flip()
