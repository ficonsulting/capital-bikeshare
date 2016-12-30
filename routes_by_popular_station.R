# -----------------------------------------------------------------
# Create bicycle routes for most popular station
# -----------------------------------------------------------------

library(dplyr)

if (!exists('trip_data')) {
    load(paste0(wd, '/trip_data.RData'))
}

source(paste0(wd, '/find_routes_geojson_function.R'))

count_by_station <- trip_data %>%
    group_by(Start.Station) %>%
    summarise(Trip.count = n())

most_popular_station <- count_by_station[which.max(count_by_station$Trip.count), ]$Start.Station

most_popular_station_day <- trip_data %>%
    filter(Start.Station == most_popular_station) %>%
    group_by(Start.date) %>%
    summarise(Trip.count = n()) %>%
    filter(!is.na(Start.date))

most_popular_station_and_day <- most_popular_station_day[which.max(most_popular_station_day$Trip.count), ]$Start.date

busiest <- trip_data %>%
    filter(Start.date == most_popular_station_and_day & Start.Station == most_popular_station)

busiest$Start.station.number <- as.numeric(busiest$Start.station.number)
busiest$End.station.number <- as.numeric(busiest$End.station.number)

# Combine trip and station data

# busiest <- busiest %>%
#   left_join(station_data %>% select(name, terminalName, lat, long), by = c('Start.Station' = 'name'))
# 
# colnames(busiest)[which(names(busiest) == 'lat')] <- 'lat_start'
# colnames(busiest)[which(names(busiest) == 'long')] <- 'long_start'
# 
# busiest <- busiest %>%
#   left_join(station_data %>% select(name, terminalName, lat, long), by = c('End.Station' = 'name'))
# 
# colnames(busiest)[which(names(busiest) == 'lat')] <- 'lat_end'
# colnames(busiest)[which(names(busiest) == 'long')] <- 'long_end'
# 
# busiest <- busiest %>%
#     filter(lat_end != 'NULL')

find_routes(busiest, 'busiest_station')
