#######################################################
# EXERCISE 4 - Across the United States, how have     #
# emissions from coal combustion-related sources      #
# changed from 1999-2008?                             #

# Prepare the SCC data set for merging
SCC2<-mutate(SCC,SCC.fact=factor(SCC))
SCC2<-select(SCC2,SCC.fact,EI.Sector)
SCC2<-group_by(SCC2,SCC.fact,EI.Sector)

# Prepare the NEI data set for merging
NEI2<-select(NEI,SCC,Emissions,year)
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
                  !is.na(str_match(merged$EI.Sector,"Coal")))

# Group by the year factor
grouped<-group_by(subsetted,year.fact)

# Calculate the sum year by year
summarized<-summarize(grouped,
                      sum.emissions=sum(sum.emissions,na.rm = TRUE))

# Make the plot
barplot(summarized$sum.emissions,
        main      = 'Total PM2.5 emitted (tons) - Coal combustion-related sources',
        names.arg = summarized$year.fact)