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
# EXERCISE 5 - How have emissions from motor vehicle  #
# sources changed from 1999-2008 in Baltimore City?   #

# Prepare the SCC data set for merging
SCC2<-mutate(SCC,SCC.fact=factor(SCC))
SCC2<-select(SCC2,SCC.fact,EI.Sector)
SCC2<-group_by(SCC2,SCC.fact,EI.Sector)

# Prepare the NEI data set for merging
NEI2<-subset(NEI,fips == "24510")
NEI2<-select(NEI2,SCC,Emissions,year)
NEI2<-mutate(NEI2,
             year.fact = factor(year),
             SCC.fact  = factor(SCC))
NEI2<-group_by(NEI2,year.fact,SCC.fact)
NEI2<-summarize(NEI2,
                sum.emissions=sum(Emissions,na.rm = TRUE))

# Merge the two data sets
merged<-join(NEI2,SCC2)

# Select only the observations of coal combustion
subsetted<-subset(merged,
                  !is.na(str_match(merged$EI.Sector,"Vehicles")))

# Group by the year factor and EI.Sector
grouped<-group_by(subsetted,year.fact,EI.Sector)

# Calculate the sum
summarized<-summarize(grouped,
                      sum.emissions=sum(sum.emissions,na.rm = TRUE))

# Make the plot
# Make the base plot
g<-ggplot(summarized,aes(year.fact,sum.emissions))

# Add the other features to the plot
g+geom_point()+
  facet_grid(.~EI.Sector)+
  geom_smooth(method='lm',aes(group=1),se=FALSE)+
  labs(x     = "Year")+
  labs(y     = "PM2.5 emissions (ton)")+
  labs(title = "PM2.5 emissions from vehicle sources - Baltimore City, Maryland")

#Save it to a png file
dev.copy(png, 
         file   = "plot5.png",
         width  = 640,
         height = 480)
dev.off()