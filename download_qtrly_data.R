# -----------------------------------------------------------------
# Download and unzip quarterly Capital Bikeshare raw data found at
# https://s3.amazonaws.com/capitalbikeshare-data/index.html
# Save to raw_data dir in wd
# -----------------------------------------------------------------

get_qtrly_data <- function(wd = getwd(),
                           dest_subdir = 'raw_data',
                           beg_yr = 2010,
                           beg_qtr = 4,
                           end_yr = 2016,
                           end_qtr = 3){
  
  save_dir <- file.path(wd, dest_subdir)
  
  #check if dest_subdir exists; if not, create it
  if (!dir.exists(save_dir)) {dir.create(save_dir)}
  
  base_url <- 'https://s3.amazonaws.com/capitalbikeshare-data/'
  end_url <- '-cabi-trip-history-data.zip'
  
  #get urls for first and last years
  beg_yr_urls <- paste0(base_url, beg_yr, '-Q', beg_qtr:4, end_url)
  end_yr_urls <- paste0(base_url, end_yr, '-Q', 1:end_qtr, end_url)
  
  #get urls for all years in between
  years <- (beg_yr + 1):(end_yr - 1)
  qtrs <- 1:4
  yr_urls <- paste0(base_url, years) 
  qtr_urls <- paste0('-Q', qtrs, end_url)
  
  urls <- apply(expand.grid(yr_urls, qtr_urls), 1, paste, collapse='')
  urls <- c(beg_yr_urls, urls, end_yr_urls)
  
  #adjust for different name starting in Q2 of 2016
  adjust_urls <- function(x){
    filename <- strsplit(x, '/')[[1]][5]
    split_name <- strsplit(filename, '-')[[1]]
    yr <- as.numeric(split_name[1])
    qtr <- as.numeric(substr(split_name[2], 2, 2))
    
    if ((yr==2016 & qtr>=2) | (yr>2016)){
      return (sub('-trip-', '-trips-', x))
    } else {
      return (x)
    }
  }
  
  urls <- sapply(urls, adjust_urls, USE.NAMES = F)
  dest_files <- file.path(save_dir, sapply(urls, function(x) strsplit(x, '/')[[1]][5], USE.NAMES = F))
  
  mapply(download.file, urls, destfile=dest_files)
  
  #unzip each file and delete original zip
  lapply(dest_files, unzip, exdir = save_dir)
  file.remove(dest_files)
  
}




