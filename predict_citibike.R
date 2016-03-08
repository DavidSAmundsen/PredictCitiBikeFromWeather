library(lubridate)
library(dplyr)

###############################################################
# Weather
###############################################################

# Get weather data from cvs file
st_id <- "725053-94728" # Central Park NYC
df_weather <- read.csv(file = paste("data/csv/weather-", st_id, ".csv", sep = ""))

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
  mutate(DATE = ymd_hm(paste(YR, M, D, HR, MIN, sep = "-"))) %>%
  select(-YR, -M, -D, -HR, -MIN)

# Clean up wind direction into north and south components
df_weather <- df_weather %>%
  mutate(WIND.NOTRH = -sin(pi/2 - WIND.DIR/360*2*pi)) %>%
  mutate(WIND.WEST = cos(pi/2 - WIND.DIR/360*2*pi)) %>%
  select(-WIND.DIR) %>%
  mutate(time = DATE) %>%
  select(-DATE)

# Only use hourly observations
df_weather <- df_weather %>%
  filter(PRECIP.PRD == 1) %>%
  select(-PRECIP.PRD)

head(df_weather)

###############################################################
# Citibike
###############################################################

# Citibike data is huge, only load one month for now
df_citibike <- read.csv("data/csv/201504-citibike-tripdata.csv")
head(df_citibike)

# Clean citibike data
df_citibike <- df_citibike %>%
  mutate(tripduration = as.numeric(tripduration)) %>%
  mutate(starttime = mdy_hms(starttime)) %>%
  mutate(stoptime = mdy_hms(stoptime)) %>%
  mutate(age = year(starttime) - birth.year) %>%
  select(-start.station.id, -end.station.id,
         -start.station.longitude, -start.station.latitude,
         -end.station.longitude, -end.station.latitude,
         -bikeid, -birth.year, -stoptime)
gender_factor <- as.factor(df_citibike$gender)
levels(gender_factor) <- c("unknown", "male", "female")
df_citibike <- df_citibike %>%
  mutate(gender = gender_factor) %>%
  select(-usertype) %>%
  na.omit()
head(df_citibike)

# Interpolate weather data onto starttime in df_citibike
df_weather_interp <- data.frame(matrix(nrow = dim(df_citibike)[1], ncol = dim(df_weather)[2]))
colnames(df_weather_interp) <- colnames(df_weather)
for (i in 1:(dim(df_weather)[2]-1)) {
  f <- approxfun(df_weather$time, df_weather[,i])
  df_weather_interp[,i] <- f(df_citibike$starttime)
}
df_weather_interp$time <- df_citibike$starttime

# Merge data frames
df <- bind_cols(df_weather_interp, df_citibike) %>%
  select(-starttime)

# Calculate number of rentals per hour
hist_trips <- df %>%
  mutate(time = round_date(time, unit = "hour")) %>%
  select(time) %>%
  table()
df_trips <- data.frame(time = unique(round_date(df$time, unit = "hour")),
                       numtrips = as.numeric(hist_trips))

# Interpolate weather
df_weather_ip <- data.frame(matrix(nrow = dim(df_trips)[1], ncol = dim(df_weather)[2]))
colnames(df_weather_ip) <- colnames(df_weather)
for (i in 1:(dim(df_weather)[2]-1)) {
  f <- approxfun(df_weather$time, df_weather[,i])
  df_weather_ip[,i] <- f(df_trips$time)
}
df_weather_ip$time <- df_trips$time

# Merge columns
df_weather_trips <- merge(df_weather_ip, df_trips, by = "time") %>%
  mutate(hour = hour(time)) %>%
  select(-time)

# Retain only hours since midnight and month of the time column
df <- df %>%
  mutate(hour = hour(time) + minute(time)/60 + second(time)/60/60) %>%
  mutate(month = month(time)) %>%
  select(-time)

################################################################################
# Start data modelling
################################################################################

library(caret)

set.seed(1)
ind_train <- createDataPartition(df_weather_trips$numtrips, p = .8, list = FALSE)
df_train <- df_weather_trips[ind_train,]
df_test <- df_weather_trips[-ind_train,]

al_grid <- expand.grid(.alpha = c(0, 0.1, 0.5, 0.7, 1), 
                       .lambda = seq(0, 20, by = 0.1))
ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

lm_fit <- train(numtrips ~ .,
                data = df_train,
                method = "lm", 
                # tuneGrid = al_grid,
                trControl = ctrl)

lm_coeff <- predict(lm_fit$finalModel, s = lm_fit$bestTune$lambda, type = "coef")
pr_numtrips <- predict(lm_fit, newdata = df_test)
RMSE(pr_numtrips, df_test$numtrips)
varImp(lm_fit)
