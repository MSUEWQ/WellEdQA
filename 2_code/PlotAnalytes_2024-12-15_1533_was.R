# Setup ####
cat("\014") # clear console
rm(list=ls()); # remove all objects from workspace
library(ggplot2)
library(gridExtra)

# import data and set years ####
lab0 <- read.csv('../1_input/2024LabResults.csv')
reg0 <- read.csv('../1_input/2024RegSheets.csv')

past.years <- c(seq(2015,2021,1))
new.year <- c(2022,2023)

# add reg fields to lab data
reg2lab <- reg0[,c("TrackNum2_primary", "TestYear", "County", "Share_County")]
lab <- merge(lab0,reg2lab, by = "TrackNum2_primary")

# Data Processing ####
lab[lab$TResult == "ND", 'NResult'] <- 0

# Quick data checks ####
unique(reg0$NotAWell) # check only 2 result options
unique(reg0$Share_County) # check no lowercase
unique(sort(reg0$TestYear)) # check for zeros or other weird values
unique(sort(reg0$YrQuarter)) # check for zeros or other weird values

# filter only well data
WellTrackNums <- reg0[reg0$NotAWell == "", "TrackNum2_primary"]
lab.well <- lab[lab$TrackNum2_primary %in% WellTrackNums,] 

## data subsets by county etc. 
# gallatin.well <- lab.well[lab.well$County == "Gallatin",]
# gallatin.all <- lab[lab$County == "Gallatin",]
# gallatin.share.all <- lab[lab$County == "Gallatin" & lab$Share_County == "Yes",]
# gallatin.share.well <- gallatin.share.all[gallatin.share.all$TrackNum2_primary 
                                             # %in% WellTrackNums,]
# length(gallatin.share.all[,1])
# length(gallatin.share.well[,1])

# Data select and filter problematic parameters ####
input.data <- lab
# input.data <- lab.well

parameter.list <- unique(input.data$Analyte)
parameter.list <- parameter.list[!(parameter.list %in% 
                  c("Coliform, Total", "Coliform, E-Coli", 
                    "Corrosivity (Langelier Index)", 
                    "Radon 222 precision (\xb1)", 
                    "Bacteria, Iron Related"))]

# create blank dataframe for parameter stats ####
headings <- c("parameter", "past.n","new.n",
              "past.min", "new.min",
              "past.median", "new.median",
              "past.max", "new.max")
heading.count <- length(headings)
compile <- data.frame(matrix(ncol = heading.count, nrow = 0))
colnames(compile) <- headings

# loop to plot analytes ####
# i = 23
text.size <- 1.4

for(i in 1:length(parameter.list))
{
  parameter.data <- input.data[input.data$Analyte == parameter.list[i],]
  
  range.i <- range(parameter.data$NResult, na.rm = TRUE)
  
  # windows(1200,600)
  png(1200,600, filename = paste0('../3_output/Boxplots/', parameter.list[i],'.png'))
  
  layout(matrix(c(1,2), nrow = 1, byrow = TRUE))
  # layout.show(2)
  
  # plot old data
  boxplot(parameter.data[parameter.data$TestYear %in% past.years, 'NResult'],
          ylim = range.i,
          main = paste0(parameter.list[i], " (", range(past.years)[1], "-", 
                       range(past.years)[2], ")"))
  past.n <- length(parameter.data[parameter.data$TestYear %in% past.years, 'NResult'])
  past.max <- max(parameter.data[parameter.data$TestYear %in% past.years, 'NResult'])
  past.median <- median(parameter.data[parameter.data$TestYear %in% past.years, 'NResult'])
  past.min <- min(parameter.data[parameter.data$TestYear %in% past.years, 'NResult'])
  
  mtext(side = 1, line = 0, cex = text.size, paste0('n = ', past.n))
  mtext(side = 1, line = 1, cex = text.size, paste0('max = ', past.max))
  mtext(side = 1, line = 2, cex = text.size, paste0('median = ', past.median))
  mtext(side = 1, line = 3, cex = text.size, paste0('min = ', past.min))

  # plot new data
  boxplot(parameter.data[parameter.data$TestYear %in% new.year, 'NResult'],
        ylim = range.i,
        main = paste0(parameter.list[i]," (" , new.year, ")"))
  new.n <- length(parameter.data[parameter.data$TestYear %in% new.year, 'NResult'])
  new.max <- max(parameter.data[parameter.data$TestYear %in% new.year, 'NResult'])
  new.median <- median(parameter.data[parameter.data$TestYear %in% new.year, 'NResult'])
  new.min <- min(parameter.data[parameter.data$TestYear %in% new.year, 'NResult'])
  
  mtext(side = 1, line = 0, cex = text.size, paste0('n = ', new.n))
  mtext(side = 1, line = 1, cex = text.size, paste0('max = ', new.max))
  mtext(side = 1, line = 2, cex = text.size, paste0('median = ', new.median))
  mtext(side = 1, line = 3, cex = text.size, paste0('min = ', new.min))
  
  dev.off()
  
  # add parameter stats to compile dataframe
  compile[i,'parameter'] <- parameter.list[i]
  compile[i, 'past.n'] <- past.n
  compile[i, 'past.min'] <- past.min
  compile[i, 'past.median'] <- past.median
  compile[i, 'past.max'] <- past.max
  compile[i, 'new.n'] <- new.n
  compile[i, 'new.min'] <- new.min
  compile[i, 'new.median'] <- new.median
  compile[i, 'new.max'] <- new.max

  # print(i)
}
# parameter.list[46]

# export parameter stats as csv ####
compile <- compile[compile$new.n >0, ]
compile <- compile[with(compile,order(parameter)),] # sort alphabetical

#compile$newhigher <- 'test'

compile[,'newhigher'] <- compile[,'new.max'] > compile[,'past.max']

compile[compile$newhigher == 'TRUE','newhigher'] <- 'Higher'
compile[compile$newhigher == 'FALSE','newhigher'] <- 'Lower'

write.csv(x = compile, file = '../3_output/StatsByAnalyte.csv')

# Reg Sheets - lists of unique attributes ####
# county list
# counties <- data.frame(test = unique(reg0$County))
# png("../3_output/uniques/counties.png", height=1500, width=200)
# p<-tableGrob(counties)
# grid.arrange(p)
# dev.off()
# 
# # Test Years
# years <- data.frame(text.result = sort(unique(reg0$TestYear)))
# png("../3_output/uniques/years.png", height=300, width=200)
# p<-tableGrob(years)
# grid.arrange(p)
# dev.off()
# 
# # Lab Results - lists of unique attributes ####
# # Text Results
# t.result <- data.frame(text.result = unique(lab0$TResult))
# png("../3_output/uniques/text.result.png", height=200, width=200)
# p<-tableGrob(t.result)
# grid.arrange(p)
# dev.off()
# 


# End ####


