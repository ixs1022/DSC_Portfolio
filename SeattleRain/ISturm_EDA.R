# Isabella Sturm
# DSC630 Project
# Data Exploration
# 2021-09-21

# Load libraries
library(ggplot2)

# Load data
setwd('./Documents/DataScience/DSC630/Project/')
weather_df <- read.csv('seattleWeather_1948-2017.csv')
head(weather_df)

# Get weekday from date in new column
weather_df$DAY <- weekdays(as.Date(weather_df$DATE))
head(weather_df)

# Get week of year from date in new column
weather_df$WEEK <- strtoi(strftime(weather_df$DATE, format='%U')) + 1 # Switch to index 1, not 0
head(weather_df)

# Get month from date in new column
weather_df$MONTH <- months(as.Date(weather_df$DATE))
head(weather_df)

# Get day of month in new column
weather_df$DAY_OF_MONTH <- strtoi(format(as.Date(weather_df$DATE, format='%Y-%m-%d'), format='%d'))
head(weather_df)

# Get day of year in new column
weather_df$DAY_OF_YEAR <- strtoi(strftime(weather_df$DATE, format='%j'))
head(weather_df)
tail(weather_df)

# Get summary statistics
summary(weather_df)

## HISTOGRAMS
rain_df <- weather_df[weather_df$RAIN == TRUE,]
head(rain_df)
norain_df <- weather_df[weather_df$RAIN == FALSE,]
head(norain_df)
# Min and Max days with rain
ggplot() +
  geom_histogram(data=rain_df, aes(TMIN), col='red')
ggplot() +
  geom_histogram(data=rain_df, aes(TMAX), col='blue')

# Min and Max days without rain
ggplot() +
  geom_histogram(data=norain_df, aes(TMIN), col='red')
ggplot() +
  geom_histogram(data=norain_df, aes(TMAX), col='blue')

# Date histograms: DAY_OF_MONTH, DAY_OF_YEAR, WEEK - with and without rain
ggplot() + 
  geom_histogram(data=rain_df, aes(DAY_OF_MONTH), col='red')
ggplot() + 
  geom_histogram(data=norain_df, aes(DAY_OF_MONTH), col='blue')

ggplot() + 
  geom_histogram(data=rain_df, aes(DAY_OF_YEAR), col='red')
ggplot() + 
  geom_histogram(data=norain_df, aes(DAY_OF_YEAR), col='blue')

ggplot() + 
  geom_histogram(data=rain_df, aes(WEEK), col='red')
ggplot() + 
  geom_histogram(data=norain_df, aes(WEEK), col='blue')

# Date bar charts: MONTH, DAY
month_order <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
ggplot() + 
  geom_bar(data=rain_df, aes(x=factor(MONTH, level=month_order)), col='red') +
  coord_flip() +
  xlab('MONTH')
ggplot() + 
  geom_bar(data=norain_df, aes(x=factor(MONTH, level=month_order)), col='blue') +
  coord_flip() +
  xlab('MONTH')

day_order <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
ggplot() + 
  geom_bar(data=rain_df, aes(x=factor(DAY, level=day_order)), col='red') +
  coord_flip() +
  xlab('DAY OF WEEK')
ggplot() + 
  geom_bar(data=norain_df, aes(x=factor(DAY, level=day_order)), col='blue') +
  coord_flip() +
  xlab('DAY OF WEEK')

# Line graph: Date vs Precipitation
ggplot(data=weather_df, aes(DATE, PRCP))

# Export new weather data frame to csv
write.csv(weather_df,'seattleWeather_new.csv', row.names=FALSE)
