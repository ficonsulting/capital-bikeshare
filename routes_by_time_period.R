# -----------------------------------------------------------------
# Create bicycle routes for a specified time interval 
# -----------------------------------------------------------------

day <- '2011-01-01'
start_time <- '13:00'
end_time <- '19:00'

if (!exists('trip_data')) {
    load(paste0(wd, '/trip_data.RData'))
}

source(paste0(wd, '/R code/find_routes_geojson_function.R'))

selected_trips <- trip_data %>%
    filter(Start.date == day) %>%
    filter(Start.time > start_time & Start.time < end_time)

find_routes(selected_trips, 'new_years')
