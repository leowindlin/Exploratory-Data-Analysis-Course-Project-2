#######################################################
# EXERCISE 1 - Have total emissions from PM2.5        #
# decreased in the United States from 1999 to 2008?   #

# Select only the columns we will need
dt<-select(NEI,year,Emissions)

# Make the year.fact, a factor column
dt<-mutate(dt,year.fact=factor(year))

# Group by the year factor
grouped<-group_by(dt,year.fact)

# Calculate the sum year by year
summarized<-summarize(grouped,
                      sum.emissions=sum(Emissions,na.rm = TRUE))

# Plot the means in a bar plot
barplot(summarized$sum.emissions,
        main      = 'Total PM2.5 emitted (tons)',
        names.arg = summarized$year.fact)