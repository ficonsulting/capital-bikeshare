# -----------------------------------------------------------------
# Function for creating geojson file with routes from Google Maps API
# -----------------------------------------------------------------

find_routes <- function(dataframe, output_name) {
    
    library(httr)
    library(gepaf)
    library(rgdal)
    
    google_api_key <- google_api_key # from macro_variables.R
    google_url <- 'https://maps.googleapis.com/maps/api/directions/json?'
    
    # Combine data sets
    dataframe <- dataframe %>%
        left_join(station_data %>% select(name, terminalName, lat, long), by = c('Start.Station' = 'name'))
    
    colnames(dataframe)[which(names(dataframe) == 'lat')] <- 'lat_start'
    colnames(dataframe)[which(names(dataframe) == 'long')] <- 'long_start'
    
    dataframe <- dataframe %>%
        left_join(station_data %>% select(name, terminalName, lat, long), by = c('End.Station' = 'name'))
    
    colnames(dataframe)[which(names(dataframe) == 'lat')] <- 'lat_end'
    colnames(dataframe)[which(names(dataframe) == 'long')] <- 'long_end'
    
    dataframe <- dataframe %>%
        filter(lat_end != 'NULL')
    
    # Time delay and trip length variables
    dataframe$trip_length <- as.numeric(difftime(dataframe$End.date.time, dataframe$Start.date.time, units = 'mins'))
    dataframe$time_delay <- as.numeric(difftime(dataframe$Start.date.time, min(dataframe$Start.date.time), units = 'mins'))
    
    route <- c()
    l <- list()
    skip <- 0
    type <- c()
    time_delay <- c()
    trip_length <- c()
    number_of_trips <- nrow(dataframe)
    
    for (i in 1:number_of_trips) {
        
        rider_number <- i
        origin       <- paste0(dataframe[rider_number, 'lat_start'], ',', dataframe[rider_number, 'long_start'])
        destination  <- paste0(dataframe[rider_number, 'lat_end'], ',', dataframe[rider_number, 'long_end'])
        
        url <- paste0(google_url,
                      'origin=', origin,
                      '&destination=', destination,
                      '&key=', google_api_key,
                      '&mode=bicycling')
        
        get <- GET(url)
        cg <- content(get)
        
        if (cg$status == 'ZERO_RESULTS') {skip <- skip + 1}
        if (cg$status == 'ZERO_RESULTS') next
        
        enc_polyline <- cg$routes[[1]]$overview_polyline$points
        
        coords <- decodePolyline(enc_polyline = enc_polyline)
        coords$lat  <- as.numeric(coords$lat)
        coords$lon <- as.numeric(coords$lon)
        
        coords <- coords[, 2:1]
        
        l[[i]] <- Lines(list(Line(coords)), ID = i - skip - 1)
        print(paste(i, ',', skip))
        
        type <- c(type, as.character(dataframe[i, 'Subscription.Type']))
        time_delay <- c(time_delay, dataframe[i, 'time_delay'])
        trip_length <- c(trip_length, dataframe[i, 'trip_length'])
        
    }
    
    l <- l[ ! sapply(l, is.null) ]
    
    for (r in 1:length(l)) {
        rt <- paste0('leg_', r - 1)
        route <- c(route, rt)
    }
    
    route_df <- data.frame(route = route,
                           type = type,
                           time_delay = time_delay,
                           trip_length = trip_length)
    
    row.names(route_df) <- seq(0, length(l) - 1)
    route_sl <- SpatialLines(l)
    
    route_sldf  <- SpatialLinesDataFrame(route_sl, route_df)
    writeOGR(route_sldf, paste0(output_name, '.geojson'), 'coords', driver='GeoJSON')
}

