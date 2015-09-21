#######################################################
# EXERCISE 6 - Compare emissions from motor vehicle   #
# sources in Baltimore City with emissions from motor #
# vehicle sources in Los Angeles County, California   #
# (fips == "06037"). Which city has seen greater      #
# changes over time in motor vehicle emissions?       #

# Prepare the SCC data set for merging
SCC2<-mutate(SCC,SCC.fact=factor(SCC))
SCC2<-select(SCC2,SCC.fact,EI.Sector)
SCC2<-group_by(SCC2,SCC.fact,EI.Sector)

# Prepare the NEI data set for merging
NEI2<-select(NEI,SCC,Emissions,year,fips)
NEI2<-subset(NEI,fips == "24510"| fips == "06037")
NEI2<-mutate(NEI2,
             year.fact = factor(year),
             SCC.fact  = factor(SCC),
             fips.fact = factor(fips))
NEI2<-group_by(NEI2,
               year.fact,
               SCC.fact,
               fips.fact)
NEI2<-summarize(NEI2,
                sum.emissions=sum(Emissions,na.rm = TRUE))

# Merge the two data sets
merged<-join(NEI2,SCC2)

# Select only the observations of coal combustion
subsetted<-subset(merged,
                  !is.na(str_match(merged$EI.Sector,"Vehicles")))

# Group by the year and fips.fact
grouped<-group_by(subsetted,year.fact,fips.fact)

# Calculate the sum
summarized<-summarize(grouped,
                      sum.emissions=sum(sum.emissions,na.rm = TRUE))

# Add a new column with the city name
summarized$Locale<-ifelse(summarized$fips.fact=="06037",
                        "Los Angeles County, California",
                        "Baltimore City, Maryland")

# Make the plot
ggplot(summarized,
       aes(year.fact,sum.emissions,fill=Locale))+
geom_bar(stat="identity", position = "dodge")+
labs(x     = "Year")+
labs(y     = "PM2.5 emissions (ton)")+
labs(title = "PM2.5 emissions from vehicle sources - Baltimore City, Maryland x Los Angeles County, California")