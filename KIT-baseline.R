# Simple baseline forecast for COVID19 death numbers in Germany

setwd("/home/johannes/Documents/COVID/KIT-baseline")
source("functions.R") # read in helper functions
Sys.setlocale("LC_ALL", "en_US.utf8") # Linux
# Sys.setlocale("LC_ALL","English") # Windows

# select forecast_date for which to generate forecasts
forecast_date <- as.Date("2020-07-06")
last_obs_week <- MMWRweek::MMWRweek(forecast_date)$MMWRweek - 1

# Define which quantiles are to be stored:
q <- c(0.01, 0.025, 1:19/20, 0.975, 0.99)

# path where the covid19-forecast-hub-de repository is stored
path_hub <- "/home/johannes/Documents/COVID/covid19-forecast-hub-de"

# read in incident and cumulative death data:
dat_inc <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-Incident Deaths_Germany.csv"),
                            colClasses = c(date = "Date"), stringsAsFactors = FALSE)
dat_inc_poland <- read.csv(paste0(path_hub, "/data-truth/ECDC/truth_ECDC-Incident Deaths_Poland.csv"),
                           colClasses = c(date = "Date"), stringsAsFactors = FALSE)
dat_inc <- rbind(dat_inc, dat_inc_poland)

dat_cum <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-Cumulative Deaths_Germany.csv"),
                            colClasses = c(date = "Date"), stringsAsFactors = FALSE)
dat_cum_poland <- read.csv(paste0(path_hub, "/data-truth/ECDC/truth_ECDC-Cumulative Deaths_Poland.csv"),
                           colClasses = c(date = "Date"), stringsAsFactors = FALSE)
dat_cum <- rbind(dat_cum, dat_cum_poland)

# check that truth data are up to date:
if(max(dat_inc$date) < forecast_date - 1) warning("Please update the covid19-forecast-hub-de repository.")

# handle weekdays and epidemic weeks:
dat_inc$weekday <- weekdays(dat_inc$date)
dat_inc$week <- MMWRweek::MMWRweek(dat_inc$date)$MMWRweek
dat_inc$year <- MMWRweek::MMWRweek(dat_inc$date)$MMWRyear

# aggregate to weekly data:
dat_inc_weekly <- aggregate(dat_inc[, c("value")],
                                    by = list(week = dat_inc$week,
                                              year = dat_inc$year,
                                              location = dat_inc$location,
                                              location_name = dat_inc$location_name),
                                    FUN = sum)
colnames(dat_inc_weekly)[5] <- "inc_death"

# add target_end_date variable:
target_end_dates <- aggregate(dat_inc[, c("date")],
                              by = list(week = dat_inc$week,
                                        location = dat_inc$location,
                                        location_name = dat_inc$location_name),
                              FUN = max)
colnames(target_end_dates)[4] <- "target_end_date"

dat_inc_weekly <- merge(dat_inc_weekly, target_end_dates,
                                by = c("week", "location", "location_name"))


# put everything together:
dat_weekly <- merge(dat_inc_weekly, dat_cum,
                            by.x = c("target_end_date", "location", "location_name"),
                            by.y = c("date", "location", "location_name"))
colnames(dat_weekly)[colnames(dat_weekly) == "value"] <- "cum_death"

tail(dat_weekly)

all_forecasts <- NULL

# run through locations in Germany:
locations_germany <- unique(dat_weekly$location)
locations_germany <- locations_germany[grepl("GM", locations_germany)]

for(loc in locations_germany){
  cat("Starting", loc, "\n")

  # put everything together:
  forecasts_location <- baseline_forecast(dat_weekly = dat_weekly, loc = loc, forecast_date = forecast_date)

  # and add to large table:
  if(is.null(all_forecasts)){
    all_forecasts <- forecasts_location
  }else{
    all_forecasts <- rbind(all_forecasts, forecasts_location)
  }
}

if(!(all(weekdays(all_forecasts$target_end_date) == "Saturday"))){
  warning("target_end_days are not all Saturdays!")
  } else{
  # store:
  write.csv(all_forecasts, file = paste0("forecasts/", forecast_date, "-Germany-KIT-baseline.csv"), row.names = FALSE)
}

# add forecast for Poland:
forecasts_poland <- baseline_forecast(dat_weekly = dat_weekly, loc = "PL", forecast_date = forecast_date)
write.csv(forecasts_poland, file = paste0("forecasts/", forecast_date, "-Poland-KIT-baseline.csv"), row.names = FALSE)
