library(lubridate)
library(dplyr)
library(quantmod)m

# Load weather data from CSV files
st_id <- "725033-94728" # Central Park NYC
weather_data <- read.csv(file = paste("data/csv/weather-", st_id, ".csv", sep = ""))

# Set invalid measurements to NA (can I do this easly with dplyr?)
weather_data$WIND.DIR[weather_data$WIND.DIR == 999] <- NA
weather_data$WIND.SPD[weather_data$WIND.SPD == 999.9] <- NA
weather_data$TEMP[weather_data$TEMP == 999.9] <- NA
weather_data$DEW.POINT[weather_data$DEW.POINT == 999.9] <- NA
weather_data$ATM.PRES[weather_data$ATM.PRES == 9.9999] <- NA
weather_data$PRECIP.PRD[weather_data$PRECIP.PRD == 99] <- NA
weather_data$PRECIP.DPTH[weather_data$PRECIP.DPTH == 999.9] <- NA

# Remove all observations with NA
weather_data <- na.omit(weather_data)

# Clean up dates and times
weather_data <- weather_data %>%
  arrange(YR, M, D, HR, MIN) %>%
  mutate(DATE = ymd_hm(paste(YR, M, D, HR, MIN, sep = "-"))) %>%
  select(-YR, -M, -D, -HR, -MIN)
dates <- weather_data$DATE
weather_data <- weather_data %>%
  select(-DATE) %>%
  as.xts(order.by = dates)
head(weather_data)

# Compute daily averages
weather_daily <-
  apply.daily(weather_data[,c("WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT", "ATM.PRES")], mean)
presip.dpth_daily <- apply.daily(weather_data[,c("PRECIP.DPTH")], sum)
weather_daily <- merge.xts(weather_daily, presip.dpth_daily)
index(weather_daily) <- as.Date(index(weather_daily))

# Get stock prices
getSymbols("^DJI",src="yahoo")
names(DJI) <- tolower(substr(names(DJI), 5, 100))
prices <- DJI

# Create column with relative change and variation in stock prices
n_obs <- dim(prices)[1]
prices$change <- diff(prices$close, lag = 1)/prices$close
prices$variation <- (prices$high - prices$low)/prices$open

# Merge weather and prices to a single time series
ts <- na.omit(merge(weather_daily, prices))
head(ts)
