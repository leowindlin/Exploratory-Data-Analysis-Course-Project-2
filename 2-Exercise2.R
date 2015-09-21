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