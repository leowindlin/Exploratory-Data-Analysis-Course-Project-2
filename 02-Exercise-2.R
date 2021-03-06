#######################################################
# SETTING WORKING DIRECTORY                           #

# Adjust the path below to fit your choice ofworking
# directory.
path  <- "C:/Users/Leonardo/Documents/GitHub/Exploratory-Data-Analysis-Course-Project-2"
setwd(path)


#######################################################
# LOADING LIBRARIES                                   #
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)

#######################################################
# DOWNLOADING FILES                                   #

# Download and unzip the file
fileUrl<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url=fileUrl,destfile = "./source.zip", mode='wb')
unzip(zipfile = "./source.zip", exdir = ".")


#######################################################
# READING FILES                                       #

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#######################################################
# EXERCISE 2 - Have total emissions from PM2.5        #
# decreased in the Baltimore City, Maryland           #
# (fips == "24510") from 1999 to 2008? Use the base   #
# plotting system to make a plot answering this       #
# question.                                           #

# Subset the data set to select only the city we need
dt<-subset(NEI,fips == "24510")

# Select only the columns we will need
dt<-select(dt,year,Emissions)

# Make the year.fact, a factor column
dt<-mutate(dt,year.fact=factor(year))

# Group by the year factor
grouped<-group_by(dt,year.fact)

# Calculate the sum year by year
summarized<-summarize(grouped,
                      sum.emissions=sum(Emissions,na.rm = TRUE))

# Plot the means in a bar plot
barplot(summarized$sum.emissions,
        main      = 'Total PM2.5 emitted (tons) - Baltimore City, Maryland',
        names.arg = summarized$year.fact)

#Save it to a png file
dev.copy(png, 
         file   = "plot2.png",
         width  = 640,
         height = 480)
dev.off()