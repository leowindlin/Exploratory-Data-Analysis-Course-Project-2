#######################################################
# EXERCISE 3 - Of the four types of sources indicated #
# by the type (point, nonpoint, onroad, nonroad)      #
# variable, which of these four sources have seen     #
# decreases in emissions from 1999-2008 for Baltimore #
# City? Which have seen increases in emissions from   #
# 1999-2008? Use the ggplot2 plotting system to make a#
# plot answer this question.                          #

# Select only the columns we will need
dt<-select(NEI,year,type,Emissions)

# Make the year.fact and type.fact,
# factor columns
dt<-mutate(dt,
           year.fact=factor(year),
           type.fact=factor(type))

# Group by the year factor and type factor
grouped<-group_by(dt,year.fact,type.fact)

# Calculate the sum year by year
summarized<-summarize(grouped,
                      sum.emissions=sum(Emissions,na.rm = TRUE))

# Make the base plot
g<-ggplot(summarized,aes(year.fact,sum.emissions))

# Add the other features to the plot
g+geom_point()+
facet_grid(.~type.fact)+
geom_smooth(method='lm',aes(group=1),se=FALSE)+
labs(x     = "Year")+
labs(y     = "PM2.5 emissions (ton)")+
labs(title = "PM2.5 emissions by type")
  