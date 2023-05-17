library(readxl)
library(tidyverse)
library(dplyr)

#create a variable with all the sheet names in the excel workbook
sheets <- excel_sheets("Water_Temp_sources_FullList.xlsx")

#combine all the Source name, Lat and Long columns from all the sheets and add a column with the sheets number. 
##some lat and longs were in the wrong format so we made them all character class
combined_data <- map_df(sheets, ~ read_excel("Water_Temp_sources_FullList.xlsx", sheet = .x) %>%
                          select(Source_name, Latitude, Longitude) %>% 
                          mutate(Latitude = as.character(Latitude), 
                                 Longitude = as.character(Longitude)),
                        .id = "Sheet")


# Filter the rows with NA values in the Latitude column
na_lat_entries <- combined_data %>%
  filter(is.na(Latitude))
# View the entries with NA in the Latitude column
na_lat_entries
#delete the table after review
#rm(na_lat_entries)

#export the combined data as csv
write.csv(combined_data, file = "exports/combined_data.csv", row.names = FALSE)
