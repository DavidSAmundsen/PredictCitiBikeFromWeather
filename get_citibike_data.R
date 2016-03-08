# Based on
# http://blue.for.msu.edu/lab-notes/NOAA_0.1-1/NOAA-ws-data.pdf

library(dplyr)
library(lubridate)

t_begin = ymd("2013-06-01")
t_end = ymd("2016-02-01")

t_cur <- t_begin
file_status <- c()
i_t = 0
while (t_cur <= t_end) {
  t_cur <- t_cur + months(1)
  i_t = i_t + 1
  
  file_name <- paste(year(t_cur), sprintf("%02g", month(t_cur)),
                     "-citibike-tripdata.zip", sep = "")
  wget_cmd <- paste("wget -P data/raw https://s3.amazonaws.com/tripdata",
                    "/", file_name, sep = "")
  file_status[i_t] <- try(system(wget_cmd, intern = FALSE, ignore.stderr = TRUE))
}

if (sum(file_status != 0)) {
  print("Could not download all files.")
}

# Unzip data files
system("unzip 'data/raw/*.zip' -d data/csv && rm data/raw/*trip*.zip",
       intern = FALSE, ignore.stderr = TRUE)

# Loop through all files, get number of trips per hour and write to CSV
df_citibike_trips <- data.frame()
citibike_files <- list.files("data/csv", pattern = "trip")
file_date_format_change <- "201409-citibike-tripdata.csv" # Date format changes from this file
date_format <- "ymd"
for (citibike_file in citibike_files) {
  print(citibike_file)
  
  # Determine format of date
  if (citibike_file == file_date_format_change) {
    date_format <- "mdy"
  }
  df_citibike_file <- read.csv(file = paste("data/csv/", citibike_file, sep = ""))
  
  # Determine format of time
  date_time_example <- strsplit(as.character(df_citibike_file$starttime[1]), split = " ")
  if (nchar(date_time_example[[1]][2]) > 5) {
    time_format <- "hms"
  } else {
    time_format <- "hm"
  }
  
  # Calculate number of trips per hour
  if (date_format == "ymd" & time_format == "hms") {
    hist_trips <- df_citibike_file %>%
      mutate(starttime = ymd_hms(starttime)) %>%
      mutate(starttime = round_date(starttime, unit = "hour")) %>%
      select(starttime) %>%
      table()
  } else if (date_format == "mdy" & time_format == "hms") {
    hist_trips <- df_citibike_file %>%
      mutate(starttime = mdy_hms(starttime)) %>%
      mutate(starttime = round_date(starttime, unit = "hour")) %>%
      select(starttime) %>%
      table()
  } else if (date_format == "mdy" & time_format == "hm") {
    hist_trips <- df_citibike_file %>%
      mutate(starttime = mdy_hm(starttime)) %>%
      mutate(starttime = round_date(starttime, unit = "hour")) %>%
      select(starttime) %>%
      table()
  } else {
    stop("Date format not recognized.")
  }
  
  # Add data from current file to data frame
  df_citibike_trips_file <- as.data.frame(hist_trips)
  colnames(df_citibike_trips_file) <- c("time", "numtrips")
  df_citibike_trips_file <- df_citibike_trips_file %>%
    mutate(time = ymd_hms(time))
  df_citibike_trips <- bind_rows(df_citibike_trips, df_citibike_trips_file)
}

# Add up observations with same time stamps at intersection between data files
df_citibike_trips_final <- df_citibike_trips %>%
  group_by(time) %>%
  summarise(sum(numtrips))

write.csv(df_citibike_trips_final, file = "data/csv/citibike-trips.csv", row.names = FALSE)