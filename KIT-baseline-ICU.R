# Simple baseline forecast for COVID19 cases in ICU and ventilation in Germany

setwd("/home/johannes/Documents/COVID/KIT-baseline")

# set locale to English:
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# path where the covid19-forecast-hub-de repository is stored
path_hub <- "/home/johannes/Documents/COVID/covid19-forecast-hub-de"

# read in data on ICU and ventilation use:
dat_germany_ICU <- read.csv(paste0(path_hub, "/data-truth/DIVI/bundeslaender/truth_DIVI-cases_covid_current_Germany.csv"),
                            colClasses = c(date = "Date"), stringsAsFactors = FALSE)

dat_germany_vent <- read.csv(paste0(path_hub, "/data-truth/DIVI/bundeslaender/truth_DIVI-cases_covid_current_ventilated_Germany.csv"),
                            colClasses = c(date = "Date"), stringsAsFactors = FALSE)

# handle weekdays and epidemic weeks:
dat_germany_ICU$weekday <- weekdays(dat_germany_ICU$date)
dat_germany_ICU$week <- MMWRweek::MMWRweek(dat_germany_ICU$date)$MMWRweek
dat_germany_ICU$year <- MMWRweek::MMWRweek(dat_germany_ICU$date)$MMWRyear
# aggregate to weekly data:
dat_germany_ICU_weekly <- subset(dat_germany_ICU, weekdays(date) == "Saturday")


colnames(dat_germany_ICU_weekly)[4] <- "curr_ICU"
head(dat_germany_ICU_weekly)

# add target_end_date variable:
target_end_dates <- aggregate(dat_germany_ICU[, c("date")],
                              by = list(week = dat_germany_ICU$week,
                                        location = dat_germany_ICU$location,
                                        location_name = dat_germany_ICU$location_name),
                              FUN = max)
colnames(target_end_dates)[4] <- "target_end_date"


dat_germany_ICU_weekly <- merge(dat_germany_ICU_weekly, target_end_dates,
                                by = c("week", "location", "location_name"))


# put everything together:
dat_germany_weekly <- merge(dat_germany_ICU_weekly, dat_germany_vent,
                            by.x = c("target_end_date", "location", "location_name"),
                            by.y = c("date", "location", "location_name"))
colnames(dat_germany_weekly)[colnames(dat_germany_weekly) == "value"] <- "curr_vent"

tail(dat_germany_weekly)

# select forecast_date for which to generate forecasts
forecast_date <- as.Date("2020-07-13")
last_obs_week <- MMWRweek::MMWRweek(forecast_date)$MMWRweek - 1
# Define quantiles
q <- c(0.01, 0.025, 1:19/20, 0.975, 0.99)

# helper function to estimate overdispersino parameter:
fit_psi <- function(vect){
  mu <- pmax(head(vect, length(vect) - 1), 1)
  obs <- tail(vect, length(vect) - 1)
  fct <- function(psi) -sum(dnbinom(obs, mu = mu, size = psi, log = TRUE))
  opt <- optimize(fct, interval = c(0, 100))
  return(opt$minimum)
}


all_forecasts <- NULL


loc <- "GM01"

for(loc in unique(dat_germany_weekly$location)){
  cat("Starting", loc, "\n")

  # subset:
  dat_location_weekly <- subset(dat_germany_weekly, location == loc &
                                  week %in% (last_obs_week - 5:0) &
                                  year == max(year))
  loc_name <- dat_location_weekly$location_name[1]

  # use last observation as predictive mean:
  mu_ICU <- max(tail(dat_location_weekly$curr_ICU, 1), 0.2)

  # estimate overdispersion parameter from last 5 observations:
  if(all(dat_location_weekly$inc_death == dat_location_weekly$curr_ICU[1])){
    psi_ICU <- 30
  }else{
    psi_ICU <- fit_psi(dat_location_weekly$curr_ICU)
  }

  # write out last observed values, point forecsts and quantiles for ICU use:
  obs_ICU <- data.frame(forecast_date = forecast_date,
                        target = paste((-1:0), "wk ahead curr ICU"),
                        target_end_date = tail(dat_location_weekly$target_end_date, 2),
                        location = loc,
                        type = "observed",
                        quantile = NA,
                        value = tail(dat_location_weekly$curr_ICU, 2),
                        location_name = dat_location_weekly$location_name[1])

  point_ICU <- data.frame(forecast_date = forecast_date,
                          target = paste(1:4, "wk ahead curr ICU"),
                          target_end_date = max(dat_location_weekly$target_end_date) + (1:4)*7,
                          location = loc,
                          type = "point",
                          quantile = NA,
                          value = qnbinom(0.5, mu = mu_ICU, size = psi_ICU),
                          location_name = dat_location_weekly$location_name[1])

  quantiles_ICU <- data.frame(forecast_date = forecast_date,
                              target = rep(paste(1:4, "wk ahead curr ICU"), each = length(q)),
                              target_end_date = rep(max(dat_location_weekly$target_end_date) +
                                                      (1:4)*7, each = length(q)),
                              location = loc,
                              type = "quantile",
                              quantile = rep(q, 4),
                              value = qnbinom(rep(q, 4), mu = mu_ICU, size = psi_ICU),
                              location_name = dat_location_weekly$location_name[1])

  # same for ventilation use:
  mu_vent <- max(tail(dat_location_weekly$curr_vent, 1), 0.2)

  if(all(dat_location_weekly$inc_death == dat_location_weekly$curr_vent[1])){
    psi_vent <- 30
  }else{
    psi_vent <- fit_psi(dat_location_weekly$curr_vent)
  }

  obs_vent <- data.frame(forecast_date = forecast_date,
                         target = paste((-1:0), "wk ahead curr ventilated"),
                         target_end_date = tail(dat_location_weekly$target_end_date, 2),
                         location = loc,
                         type = "observed",
                         quantile = NA,
                         value = tail(dat_location_weekly$curr_vent, 2),
                         location_name = dat_location_weekly$location_name[1])

  point_vent <- data.frame(forecast_date = forecast_date,
                           target = paste(1:4, "wk ahead curr ventilated"),
                           target_end_date = max(dat_location_weekly$target_end_date) + (1:4)*7,
                           location = loc,
                           type = "point",
                           quantile = NA,
                           value = qnbinom(0.5, mu = mu_vent, size = psi_vent),
                           location_name = dat_location_weekly$location_name[1])

  quantiles_vent <- data.frame(forecast_date = forecast_date,
                               target = rep(paste(1:4, "wk ahead curr ventilated"), each = length(q)),
                               target_end_date = rep(max(dat_location_weekly$target_end_date) +
                                                       (1:4)*7, each = length(q)),
                               location = loc,
                               type = "quantile",
                               quantile = rep(q, 4),
                               value = qnbinom(rep(q, 4), mu = mu_vent, size = psi_vent),
                               location_name = dat_location_weekly$location_name[1])


  # put everything together:
  forecasts_location <- rbind(obs_ICU, point_ICU, quantiles_ICU,
                              obs_vent, point_vent, quantiles_vent)

  if(is.null(all_forecasts)){
    all_forecasts <- forecasts_location
  }else{
    all_forecasts <- rbind(all_forecasts, forecasts_location)
  }

  # plotting to check plausibility:
  plot(dat_location_weekly$target_end_date, dat_location_weekly$curr_ICU, ylim = c(0, 2*max(dat_location_weekly$curr_ICU)),
       xlim = c(min(dat_location_weekly$target_end_date), max(dat_location_weekly$target_end_date) + 28),
       main = loc_name, xlab = "time", ylab = "inc deaths")
  points(quantiles_ICU$target_end_date, quantiles_ICU$value, pch = 15, col = rgb(0, 0, 0, alpha = 0.2))

  plot(dat_location_weekly$target_end_date, dat_location_weekly$curr_vent, ylim = c(0, 1.5)*range(dat_location_weekly$curr_vent),
       xlim = c(min(dat_location_weekly$target_end_date), max(dat_location_weekly$target_end_date) + 28),
       main = loc_name, xlab = "time", ylab = "cum deaths")
  points(quantiles_vent$target_end_date,
         quantiles_vent$value, pch = 15, col = rgb(0, 0, 0, alpha = 0.2))
}

# write out results as csv:
write.csv(all_forecasts, file = paste0("forecasts/", forecast_date, "-Germany-KIT-baseline-ICU.csv"), row.names = FALSE)
