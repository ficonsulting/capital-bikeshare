# -----------------------------------------------------------------
# Read in and process Capital Bikeshare raw data found at
# https://s3.amazonaws.com/capitalbikeshare-data/index.html
# -----------------------------------------------------------------

library(dplyr)
library(zoo)
library(lubridate)
library(XML)

# Read in trip data
# -----------------

file_list <- list.files(wd)

colnames <- c('Duration', 'Start.date', 'End.date', 'Start.station.number', 'Start.Station',   
              'End.station.number', 'End.Station', 'Bike', 'Subscription.Type')

trip_data <- data.frame()
for (file in file_list) {
    print(file)
    tmp <- read.csv(paste0(wd, '/raw_data/', file), stringsAsFactors = FALSE)
    if (ncol(tmp) == 7) {
        tmp <- tmp %>%
            mutate(Start.station.number = '',
                   End.station.number = '')
        # Account for the changing file format over time
        if (colnames(tmp)[3] == 'End.date') {
            tmp <- tmp[, c(1, 2, 3, 8, 4, 9, 5, 6, 7)]
        } else {
            tmp <- tmp[, c(1, 2, 4, 8, 3, 9, 5, 6, 7)]
        }
    }
    
    colnames(tmp) <- colnames
    
    if (file == '2014-Q4-cabi-trip-history-data.csv') {
        tmp$Start.date.time <- as.POSIXct(tmp$Start.date, format = "%Y-%m-%d %H:%M")
        tmp$End.date.time <- as.POSIXct(tmp$End.date, format = "%Y-%m-%d %H:%M")
    } else {
        tmp$Start.date.time <- as.POSIXct(tmp$Start.date, format = "%m/%d/%Y %H:%M")
        tmp$End.date.time <- as.POSIXct(tmp$End.date, format = "%m/%d/%Y %H:%M")
    }
    
    tmp$Start.time <- strftime(tmp$Start.date.time, format = "%H:%M")
    tmp$Start.date <- as.Date(tmp$Start.date.time, format = '%m/%d/%Y')
    
    tmp$End.time <- strftime(tmp$End.date.time, format = "%H:%M")
    tmp$End.date <- as.Date(tmp$End.date.time, format = '%m/%d/%Y')
    
    trip_data <- rbind(trip_data, tmp)
}

trip_data$Subscription.Type <- ifelse(trip_data$Subscription.Type == 'Casual', 'Casual', 'Registered')

# Split station number and station name where needed
trip_data$Start.station.number <- ifelse(grepl('[\\(\\)]', trip_data$Start.Station),
                                         gsub(".*\\((.*)\\).*", "\\1", trip_data$Start.Station),
                                         trip_data$Start.station.number)

trip_data$Start.Station <- ifelse(grepl('[\\(\\)]', trip_data$Start.Station),
                                  gsub("\\s*\\([^\\)]+\\)","", as.character(trip_data$Start.Station)),
                                  trip_data$Start.Station)

trip_data$End.station.number <- ifelse(grepl('[\\(\\)]', trip_data$End.Station),
                                         gsub(".*\\((.*)\\).*", "\\1", trip_data$End.Station),
                                       trip_data$End.station.number)

trip_data$End.Station <- ifelse(grepl('[\\(\\)]', trip_data$End.Station),
                                  gsub("\\s*\\([^\\)]+\\)","", as.character(trip_data$End.Station)),
                                trip_data$End.Station)

trip_data$Start.station.number <- as.numeric(trip_data$Start.station.number)
trip_data$End.station.number <- as.numeric(trip_data$End.station.number)

# Save dataframe
save(trip_data, file = paste0(wd, '/trip_data.RData'))

# Read in station data
# --------------------

xmlfile <- xmlTreeParse('http://feeds.capitalbikeshare.com/stations/stations.xml')

topxml <- xmlRoot(xmlfile)
topxml <- xmlSApply(topxml, function(x) xmlSApply(x, xmlValue))
station_data <- data.frame(t(topxml), row.names=NULL)

station_data$terminalName <- as.numeric(station_data$terminalName)
station_data$lat <- as.numeric(station_data$lat)
station_data$long <- as.numeric(station_data$long)
station_data$name <- as.character(station_data$name)
