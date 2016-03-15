library(lubridate)
library(dplyr)

###############################################################
# Weather
###############################################################

# Get weather data from cvs file
st_id <- "725053-94728" # Central Park NYC
df_weather <- read.csv(file = paste("data/csv/weather-", st_id, ".csv", sep = ""))
head(df_weather)

# Set invalid measurements to NA (can I do this easly with dplyr?)
df_weather$WIND.DIR[df_weather$WIND.DIR == 999] <- NA
df_weather$WIND.SPD[df_weather$WIND.SPD == 999.9] <- NA
df_weather$TEMP[df_weather$TEMP == 999.9] <- NA
df_weather$DEW.POINT[df_weather$DEW.POINT == 999.9] <- NA
df_weather$ATM.PRES[df_weather$ATM.PRES == 9.9999] <- NA
df_weather$PRECIP.PRD[df_weather$PRECIP.PRD == 99] <- NA
df_weather$PRECIP.DPTH[df_weather$PRECIP.DPTH == 999.9] <- NA

# Remove all observations with NA
df_weather <- na.omit(df_weather)

# Clean up dates and times
df_weather <- df_weather %>%
  arrange(YR, M, D, HR, MIN) %>%
  mutate(time = ymd_hm(paste(YR, M, D, HR, MIN, sep = "-", tz = "UTC"))) %>%
  select(-YR, -M, -D, -HR, -MIN) %>%
  mutate(time = with_tz(time, tzone = "America/New_York"))

# Clean up wind direction into north and south components
df_weather <- df_weather %>%
  mutate(WIND.NOTRH = -sin(pi/2 - WIND.DIR/360*2*pi)) %>%
  mutate(WIND.WEST = cos(pi/2 - WIND.DIR/360*2*pi)) %>%
  select(-WIND.DIR)

# Only use hourly observations
df_weather <- df_weather %>%
  filter(PRECIP.PRD == 1) %>%
  select(-PRECIP.PRD)

head(df_weather)

###############################################################
# Citibike
###############################################################

# Load trip data
df_citibike <- read.csv("data/csv/citibike-trips.csv")
head(df_citibike)

# Clean citibike data
df_citibike <- df_citibike %>%
  transmute(time = ymd_hms(time, tz = "America/New_York"), numtrips = sum.numtrips.)

# Interpolate weather data onto starttime in df_citibike
df_weather_interp <- data.frame(matrix(nrow = dim(df_citibike)[1], ncol = dim(df_weather)[2]))
colnames(df_weather_interp) <- colnames(df_weather)
for (i in 1:(dim(df_weather)[2])) {
  if (names(df_weather)[i] != "time") {
    f <- approxfun(df_weather$time, df_weather[,i])
    print(names(df_weather)[i])
    df_weather_interp[,i] <- f(df_citibike$time)
  }
}
df_weather_interp$time <- df_citibike$time
head(df_weather_interp)

# Merge data frames, and replace time array by hour of day
df_full <- df_weather_interp %>%
  select(-time) %>%
  bind_cols(df_citibike) %>%
  na.omit()

# Use a random day in April 2015 used for testing the prediction
df_day_test <- df_full %>%
  filter(year(time) == 2015 & month(time) == 4 & day(time) == 13) %>%
  mutate(hour = as.factor(hour(time))) %>%
  mutate(weekday = wday(time, label = TRUE)) %>%
  mutate(weekday = factor(weekday, ordered = FALSE)) %>%
  select(-time)

df <- df_full %>%
  filter(year(time) != 2015 & month(time) != 4 & day(time) != 13) %>%
  mutate(hour = as.factor(hour(time))) %>%
  mutate(weekday = wday(time, label = TRUE)) %>%
  select(-time) %>%
  mutate(weekday = factor(weekday, ordered = FALSE))

head(df)

set.seed(1)
ind_train <- createDataPartition(df$numtrips, p = .8, list = FALSE)
df_train <- df[ind_train,]
df_test <- df[-ind_train,]

save(df_weather, df_citibike, df_full, df, df_day_test, df_train, df_test,
     file = "data/data_frames.dat")

################################################################################
# Some nice plots here maybe?
################################################################################

# Citi Bike trips
df_plot <- df_full %>%
  filter(year(time) == 2015 & month(time) == 10 & (day(time) >= 1 & day(time) <= 5))
ggplot(data = df_plot, aes(x = time, y = numtrips)) + geom_bar(stat = "identity")
ggplot(data = df_plot, aes(x = time, y = PRECIP.DPTH)) + geom_bar(stat = "identity")

# Check interpolation
df_plot_1 <- df_full %>%
  filter(year(time) == 2015 & month(time) == 12 & (day(time) >= 31 & day(time) <= 31))
df_plot_2 <- df_weather %>%
  filter(year(time) == 2015 & month(time) == 12 & (day(time) >= 31 & day(time) <= 31))
ggplot() +
  geom_point(aes(x = df_plot_1$time, y = df_plot_1$TEMP), colour = "blue") +
  geom_point(aes(x = df_plot_2$time, y = df_plot_2$TEMP), colour = "red")
