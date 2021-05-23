########################################################
# Road & Crash data
# ALY 4000 - Module 2 Assignment - John King
library(ggplot2)
library(RColorBrewer)
theme_set(theme_minimal())

# Read the csv files and view the data
roads <- read.csv(paste0("~/Documents/Education/Northeastern/Analytics BE/",
                         "ALY4000/Assignment2/roadscrashes-1/roads.csv"))
crashes <- read.csv(paste0("~/Documents/Education/Northeastern/Analytics BE/",
                           "ALY4000/Assignment2/roadscrashes-1/crashes.csv"))
head(roads)
head(crashes, 10)

# Create the derived column Date as a date type
crashes$Date = as.Date(paste0(as.character(crashes$Year), '-1-1'), 
                       format='%Y-%m-%d')
str(crashes)

# Aggregate the total number of crashes and plot the time series
af <- aggregate(data=crashes, N_Crashes~Year, FUN=sum)
ggplot(af, aes(x=Year, y=N_Crashes)) + 
  geom_line(color='steelblue') + 
  geom_point() +
  ggtitle('Total number of year crashes') + 
  theme(plot.title = element_text(hjust = 0.5))


df <- crashes[c('Year', 'Road', 'N_Crashes')]
ggplot(df, aes(x=Year, y=N_Crashes, group=Road, color=Road)) + 
  geom_line() + 
  geom_point() + 
  scale_color_brewer(palette='Dark2') +
  ggtitle('Yearly number of crashes by road') + 
  theme(plot.title = element_text(hjust = 0.5))


# Visualize the average volume by road
# Do higher volume roads have a larger Number of Crashes values?
ff <- aggregate(data=crashes, Volume~Road, FUN=mean)
ggplot(ff, aes(x=Road, y=Volume, fill=Road)) + 
  geom_bar(stat='identity') + 
  scale_fill_brewer(palette='Dark2') +
  ggtitle('Average Volume by Road') + 
  theme(plot.title = element_text(hjust = 0.5))


# Scatter plot to visualize the relationship between Volume and Number of crashes by Road
ggplot(crashes, aes(x=Volume, y=N_Crashes, color=Road)) + 
  geom_point(size=2) + 
  ggtitle('Volume x Number of crashes') + 
  theme(plot.title = element_text(hjust = 0.5))

