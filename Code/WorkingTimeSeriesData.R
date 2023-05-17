library(dplyr)
library(lubridate)
library(zip)


######load and prep data. 
#you need to create a sub folder called exports for the .csv's in your WD
setwd("~/Documents/MyR/TempDataRB9")
tmpdata <- read.csv("Continuous temperature for All Data_051623.csv", header = TRUE)
#tmpdata <- read.csv("Continuous temperature for RB9.csv", header = TRUE)
# Convert timestamp column to a POSIXct object
tmpdata$timestamp <- mdy_hms(tmpdata$sampledatetime)
# Use distinct() to remove duplicate rows from tmpdata based on stationcode column
tmpdata_distinct <- distinct(tmpdata, stationcode, .keep_all = TRUE)


######Group by months
# Summerize by month
monthly_data <- tmpdata %>% 
  group_by(stationcode, month = floor_date(timestamp, "month")) %>% 
  summarize(mean_value = mean(result))
#add Lat and Long to the monthly data
monthly_data <- left_join(monthly_data, tmpdata_distinct %>% select("stationcode", "latitude", "longitude"), by = "stationcode", multiple = "all")
# Rename the "month" column to "date"
colnames(monthly_data)[colnames(monthly_data) == "month"] <- "date"
# Create a new column called "month" with just the month
monthly_data$month <- format(as.Date(monthly_data$date), "%m")
# Create a new column 'year' with the year extracted from the 'date' column
monthly_data$year <- year(monthly_data$date)


######group by Year
# Summerize by year and add Lat and Long to the yearly data and do some formatting
yearly_data <- tmpdata %>% 
  group_by(stationcode, year = floor_date(timestamp, "year")) %>% 
  summarize(mean_value = mean(result))
#add Lat and Long to the monthly data
yearly_data <- left_join(yearly_data, tmpdata_distinct %>% select("stationcode", "latitude", "longitude"), by = "stationcode", multiple = "all")
# Rename the "year" column to "date"
colnames(yearly_data)[colnames(yearly_data) == "year"] <- "date"
# Create a new column called "year" with just the year
yearly_data$year <- format(as.Date(yearly_data$date), "%Y")
#delete the date column since we no longer need it
yearly_data <- subset(yearly_data, select = -date)


######Group by quarter aka seasons
# Add a new column to tmpdata with the quarter of each month. 1 = winter, 2 = spring, 3 = summer and 4 = fall
tmpdata$quarter <- quarter(tmpdata$timestamp)
#Summerize by quarter
seasonal_data <- tmpdata %>% 
  group_by(stationcode, quarter) %>% 
  summarize(mean_value = mean(result))
#add Lat and Long to the quarter data
seasonal_data <- left_join(seasonal_data, tmpdata_distinct %>% select("stationcode", "latitude", "longitude"), by = "stationcode", multiple = "all")
#transpose the quarter column and save to new column named season
# Define a function to map quarter to season
map_quarter_to_season <- function(x) {
  ifelse(x == 1, "winter",
         ifelse(x == 2, "spring",
                ifelse(x == 3, "summer",
                       ifelse(x == 4, "fall", NA))))
}
# Add a new column "season" using mutate() and the defined function
seasonal_data <- seasonal_data %>% 
  mutate(season = map_quarter_to_season(quarter))


#view what stations have data from what seasons
table(seasonal_data$stationcode, seasonal_data$quarter)


#######Generate some data summeries
# Group the data by 'stationcode' and calculate the minimum and maximum years
period_of_record <- monthly_data %>%
  group_by(stationcode) %>%
  summarize(start_year = min(year),
            end_year = max(year),
            period_of_record = paste0(min(year), "-", max(year)))

#find the min and max sampling intervals for each site
data_summary <- tmpdata %>% 
  group_by(stationcode) %>% 
  summarize(min_interval = min(timeinterval),
            max_interval = max(timeinterval))

#join data summary and period of record
data_summary <- left_join(data_summary, period_of_record %>% select("stationcode", "start_year", "end_year", "period_of_record"), by = "stationcode", multiple = "all")
#add lat and long
data_summary <- left_join(data_summary, tmpdata_distinct %>% select("stationcode", "latitude", "longitude"), by = "stationcode", multiple = "all")
# remove the data frame "period of record" as its no longer needed
rm(period_of_record)


######Export dataframes as csv
write.csv(data_summary, file = "exports/Ca_data_summary.csv", row.names = FALSE)
write.csv(seasonal_data, file = "exports/Ca_seasonal_data.csv", row.names = FALSE)
write.csv(monthly_data, file = "exports/Ca_monthly_data.csv", row.names = FALSE)
write.csv(yearly_data, file = "exports/Ca_yearly_data.csv", row.names = FALSE)
#zip all files that were exported
folder <- "exports"
date_format <- "%m-%d-%Y"
zipname <- paste0(folder, "_", format(Sys.Date(), date_format), ".zip")
file_list <- list.files(path = folder, full.names = TRUE)
zip(zipfile = zipname, files = list.files("exports", full.names = TRUE))
