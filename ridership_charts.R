# -----------------------------------------------------------------
# Create charts to display Capital Bikeshare usage
# -----------------------------------------------------------------

library(dplyr)
library(plotly)

if (!exists('trip_data')) {
    load(paste0(wd, '/trip_data.RData'))
}

trip_by_date <- trip_data %>%
    group_by(Start.date) %>%
    summarise(Trip.count = n()) %>%
    filter(!is.na(Start.date))

# Daily ridership
roll_avg <- rollmean(trip_by_date$Trip.count, 30)

trip_by_date2 <- trip_by_date[30:nrow(trip_by_date), ]
trip_by_date2$roll_avg <- roll_avg                       

plot_ly(trip_by_date2, type = 'scatter', x = ~Start.date, y = ~roll_avg, mode = 'lines') %>%
    layout(title = "Daily riders",
           xaxis = list(title = "Date"),
           yaxis = list (title = "30-day rolling average daily riders"))

# Ridership by month
trip_by_month <- trip_by_date %>%
    mutate(Month_Yr = format(as.Date(Start.date), "%Y-%m")) %>%
    mutate(year = substring(Month_Yr, 1, 4),
           month = substring(Month_Yr, 6, 7)) %>%
    group_by(year, month) %>%
    summarise(Monthly.avg.trips = mean(Trip.count))

# Ridership by season
trip_by_season <- trip_by_date %>%
    mutate(Month_Yr = format(as.Date(Start.date), "%Y-%m")) %>%
    mutate(year = substring(Month_Yr, 1, 4),
           month = substring(Month_Yr, 6, 7)) %>%
    filter(year > 2010 & (year != 2016 | month != 10)) %>%
    mutate(season = ifelse(month %in% c('03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), 'summer', 'winter')) %>%
    group_by(year, month, season) %>%
    summarise(Avg.trips = mean(Trip.count)) %>%
    group_by(year, season) %>%
    summarise(Monthly.avg.trips = mean(Avg.trips))

trip_by_season <- data.frame(trip_by_season)

plot_ly(trip_by_season, x=~year, y=~Monthly.avg.trips, color=~season, colors = 'Set1', 
        type='scatter', mode='lines+markers') %>%
    layout(title = "Average monthly riders by season",
           xaxis = list(title = "Year"),
           yaxis = list (title = "Average riders per month"))

# Ridership by subscriber
trip_by_date_subscription <- trip_data %>%
    group_by(Start.date, Subscription.Type) %>%
    summarise(Trip.count = n()) %>%
    filter(!is.na(Start.date))

trip_by_month_subscription <- trip_by_date_subscription %>%
    mutate(Month_Yr = format(as.Date(Start.date), "%Y-%m")) %>%
    mutate(year = substring(Month_Yr, 1, 4),
           month = substring(Month_Yr, 6, 7)) %>%
    filter(year != 2016 | month != 10) %>%
    group_by(year, month, Subscription.Type) %>%
    summarise(Avg.trips = mean(Trip.count)) %>%
    group_by(year, Subscription.Type) %>%
    summarise(Monthly.avg.trips = mean(Avg.trips))

trip_by_month_subscription <- data.frame(trip_by_month_subscription)

plot_ly(trip_by_month_subscription, x=~year, y=~Monthly.avg.trips, color=~Subscription.Type, colors='Set1',
        type='scatter', mode='lines+markers') %>%
    layout(title = "Average monthly riders by rider type",
           xaxis = list(title = "Year"),
           yaxis = list (title = "Average riders per month"))

# Ridership by season and subscriber
trip_by_date_subscription <- trip_data %>%
    group_by(Start.date, Subscription.Type) %>%
    summarise(Trip.count = n()) %>%
    filter(!is.na(Start.date))

trip_by_season_subscription <- trip_by_date_subscription %>%
    mutate(Month_Yr = format(as.Date(Start.date), "%Y-%m")) %>%
    mutate(year = substring(Month_Yr, 1, 4),
           month = substring(Month_Yr, 6, 7)) %>%
    filter(year != 2016 | month != 10) %>%
    filter(year > 2010) %>%
    mutate(season = ifelse(month %in% c('03', '04', '05', '06', '07', '08', '09', '10', '11', '12'), 'summer', 'winter')) %>%
    group_by(year, month, season, Subscription.Type) %>%
    summarise(Avg.trips = mean(Trip.count)) %>%
    group_by(year, season, Subscription.Type) %>%
    summarise(Monthly.avg.trips = mean(Avg.trips)) %>%
    mutate(category = paste(Subscription.Type, '-', season))

trip_by_season_subscription <- as.data.frame(trip_by_season_subscription)

plot_ly(trip_by_season_subscription, x=~year, y=~Monthly.avg.trips, color=~category, 
        type = 'scatter', mode = 'lines+markers', colors = 'Set1') %>%
    layout(title = "Average monthly riders by season and rider type",
           xaxis = list(title = "Year"),
           yaxis = list (title = "Average riders per month"))