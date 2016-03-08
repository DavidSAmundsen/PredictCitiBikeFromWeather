# Based on
# http://blue.for.msu.edu/lab-notes/NOAA_0.1-1/NOAA-ws-data.pdf

library(dplyr)

# Set longitude and latitude of point to get weather at

# New York Stock Exchange
# lon_obs <- -74.011242
# lat_obs <- 40.706989

# Central Park
lon_obs <- -73.966556
lat_obs <- 40.782173

# Set years to get data for
year_begin <- 2013
year_end <- 2016

# Download NOAA weather history file with data about weather stations etc.
st_file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
repeat {
  try(download.file(st_file, "data/ish-history.csv", quiet = TRUE))
  if (file.info("data/ish-history.csv")$size > 0) {
    break
  }
}
st <- read.csv("data/ish-history.csv")
head(st)

# Chop off month and date from operating times, not needed
st <- st %>%
  mutate(BEGIN = as.numeric(substr(BEGIN, 1, 4))) %>%
  mutate(END = as.numeric(substr(END, 1, 4)))

head(st)

# Add a new column to the station data: the distance to (lon_obs, lat_obs) and order stations in
# increasing distance to (lon_obs, lat_obs)
st <- st %>%
  mutate(DIST = sqrt((LON - lon_obs)^2 + (LAT - lat_obs)^2)) %>%
  arrange(DIST)

# Find closest weather station with data for desired period
i_st <- 1
while (TRUE) {
  if (st[i_st,'BEGIN'] <= year_begin & st[i_st,'END'] >= year_end) {
    break
  }
  else {
    i_st <- i_st + 1
  }
}
# i_st <- 4 # Hard-coded to La Guardia Airport
st_id <- paste(st[i_st, 1], "-", st[i_st, 2], sep = "")
print(paste(st[i_st, "STATION.NAME"], ": ", st_id, sep = ""))

# Download weather data
file_status <- as.data.frame(matrix(NA, year_end - year_begin + 1, 2))
names(file_status) <- c("FILE", "STATUS")
years <- year_begin:year_end
for (i_year in 1:length(years)) {
  file_status[i_year, 1] <- paste(sprintf("%06d", st[i_st,1]), "-",
                                  sprintf("%05d", st[i_st,2]), "-",
                                  years[i_year], ".gz", sep = "")
  wget_cmd <- paste("wget -P data/raw ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",
                    "/", toString(years[i_year]), "/", file_status[i_year, 1], sep = "")
  file_status[i_year, 2] <- try(system(wget_cmd, intern = FALSE, ignore.stderr = TRUE))
}

# Unzip weather data files
system("gunzip -r data/raw", intern = FALSE, ignore.stderr = TRUE)

# Extract weather data from files
files <- list.files("data/raw", pattern = st_id)
column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6,
                   1, 1, 1, 5, 1, 5, 1, 5, 1, 3, 3, 2, 4, 1, 1)
data <- data.frame()
for (i in 1:length(files)) {
  data_file <- read.fwf(paste("data/raw/", files[i], sep = ""), column.widths)
  data_file <- data_file[, c(4:8, 16, 19, 29, 31, 33, 36, 37, 38)]
  
  # For observations with no precipitation information, set precipitation to NA
  with_precip <- substr(data_file$V36, 1, 2) == "AA"
  data_file[!with_precip,"V37"] <- NA
  data_file[!with_precip,"V38"] <- NA
  
  # Append data from current file to complete data frame
  data <- bind_rows(data, data_file)
}

# Set more appropriate column names
names(data) <- c("YR", "M", "D", "HR", "MIN",
                 "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT", "ATM.PRES",
                 "ADD.ID", "PRECIP.PRD", "PRECIP.DPTH")

# Convert weather data to normal units
data <- data %>%
  mutate(WIND.SPD = WIND.SPD/10) %>%
  mutate(TEMP = TEMP/10) %>%
  mutate(DEW.POINT = DEW.POINT/10) %>%
  mutate(ATM.PRES = ATM.PRES/10000) %>%
  mutate(PRECIP.DPTH = as.numeric(PRECIP.DPTH)/10) %>%
  select(-ADD.ID)
write.csv(data, file = paste("data/csv/weather-", st_id, ".csv", sep = ""), row.names = FALSE)
