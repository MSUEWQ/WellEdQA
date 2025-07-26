# Setup ####
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace

library(tidyverse)

# import data and set years ####
lab0 <- read.csv('../1_input/LabResults.csv')
reg0 <- read.csv('../1_input/RegSheets.csv')


# add reg fields to lab data
reg2lab <- reg0[,c("TrackNum2_primary", "TestYear", "County", "Share_County")]
lab <- merge(lab0,reg2lab, by = "TrackNum2_primary")

# Data Processing
lab[lab$TResult == "ND", 'NResult'] <- 0


# Check for duplicate primary tracknums ####
# Aggregate to get unique combinations of tracking number and sampleid
prim <- lab %>% group_by(TrackNum2_primary, SampleID, Analyte) %>% tally()

# Output table of tracking numbers with duplicates and their assigned sampleids
write.csv(prim %>% filter(n > 1),
          '../3_output/DuplicateAnalyteResults.csv')

# function for tallying data field

CheckEntries <- function(data, field)
{
  temp <- data %>% group_by_at(field) %>% tally()
  write.csv(temp, paste0('../3_output/DataFieldCounts/',colnames(temp)[1], '.csv'),
            row.names = F)
}

CheckEntries(reg0, colnames(reg0)[1])


for (f in colnames(reg0))
{
  CheckEntries(reg0,f)
}


for (f in colnames(lab))
{
  CheckEntries(lab,f)
}


nas <- c()

for (i in 1:length(colnames(lab)))
{
  nas <- c(nas, length(which(is.na(lab[i]))))
}

na.df <- data.frame(Field = colnames(lab), NACount = nas)
write.csv(na.df, '../3_output/NACounts_lab.csv')

nas <- c()

for (i in 1:length(colnames(reg0)))
{
  nas <- c(nas, length(which(is.na(reg0[i]))))
}

na.df <- data.frame(Field = colnames(reg0), NACount = nas)
write.csv(na.df, '../3_output/NACounts_reg.csv')



