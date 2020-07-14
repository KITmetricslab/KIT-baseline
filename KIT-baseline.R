# Simple baseline forecast for COVID19 death numbers in Germany

# setwd("/home/johannes/Documents/COVID/KIT-baseline")

# path where the covid19-forecast-hub-de repository is stored
path_hub <- "/home/johannes/Documents/COVID/covid19-forecast-hub-de"

# read in incident and cumulative death data:
dat_germany_inc <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-Incident Deaths_Germany.csv"),
                            colClasses = c(date = "Date"), stringsAsFactors = FALSE)

dat_germany_cum <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-Cumulative Deaths_Germany.csv"),
                            colClasses = c(date = "Date"), stringsAsFactors = FALSE)

# handle weekdays and epidemic weeks:
dat_germany_inc$weekday <- weekdays(dat_germany_inc$date)
dat_germany_inc$week <- MMWRweek::MMWRweek(dat_germany_inc$date)$MMWRweek
dat_germany_inc$year <- MMWRweek::MMWRweek(dat_germany_inc$date)$MMWRyear

# aggregate to weekly data:
dat_germany_inc_weekly <- aggregate(dat_germany_inc[, c("value")],
                                    by = list(week = dat_germany_inc$week,
                                              year = dat_germany_inc$year,
                                              location = dat_germany_inc$location,
                                              location_name = dat_germany_inc$location_name),
                                    FUN = sum)
colnames(dat_germany_inc_weekly)[5] <- "inc_death"

# add target_end_date variable:
target_end_dates <- aggregate(dat_germany_inc[, c("date")],
                              by = list(week = dat_germany_inc$week,
                                        location = dat_germany_inc$location,
                                        location_name = dat_germany_inc$location_name),
                              FUN = max)
colnames(target_end_dates)[4] <- "target_end_date"

dat_germany_inc_weekly <- merge(dat_germany_inc_weekly, target_end_dates,
                                by = c("week", "location", "location_name"))


# put everything together:
dat_germany_weekly <- merge(dat_germany_inc_weekly, dat_germany_cum,
                            by.x = c("target_end_date", "location", "location_name"),
                            by.y = c("date", "location", "location_name"))
colnames(dat_germany_weekly)[colnames(dat_germany_weekly) == "value"] <- "cum_death"

tail(dat_germany_weekly)

# select forecast_date for which to generate forecasts
forecast_date <- as.Date("2020-07-13")
last_obs_week <- MMWRweek::MMWRweek(forecast_date)$MMWRweek - 1

# Define which quantiles are to be stored:
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

# run through locations:
for(loc in unique(dat_germany_weekly$location)){
  cat("Starting", loc, "\n")

  # subset:
  dat_location_weekly <- subset(dat_germany_weekly, location == loc &
                                  week %in% (last_obs_week - 5:0) &
                                  year == max(year))
  loc_name <- dat_location_weekly$location_name[1]

  # use last observation as predictive mean:
  mu <- max(tail(dat_location_weekly$inc_death, 1), 0.2)

  # estimate overdispersion parameter from last 5 observations:
  if(all(dat_location_weekly$inc_death == dat_location_weekly$inc_death[1])){
    psi <- 30
  }else{
    psi <- fit_psi(dat_location_weekly$inc_death)
  }

  # write out last observed values, point forecsts and quantiles for incident deaths:
  obs_inc <- data.frame(forecast_date = forecast_date,
                        target = paste((-1:0), "wk ahead inc death"),
                        target_end_date = tail(dat_location_weekly$target_end_date, 2),
                        location = loc,
                        type = "observed",
                        quantile = NA,
                        value = tail(dat_location_weekly$inc_death, 2),
                        location_name = dat_location_weekly$location_name[1])

  point_inc <- data.frame(forecast_date = forecast_date,
                          target = paste(1:4, "wk ahead inc death"),
                          target_end_date = max(dat_location_weekly$target_end_date) + (1:4)*7,
                          location = loc,
                          type = "point",
                          quantile = NA,
                          value = qnbinom(0.5, mu = mu, size = psi),
                          location_name = dat_location_weekly$location_name[1])

  quantiles_inc <- data.frame(forecast_date = forecast_date,
                              target = rep(paste(1:4, "wk ahead inc death"), each = length(q)),
                              target_end_date = rep(max(dat_location_weekly$target_end_date) +
                                                      (1:4)*7, each = length(q)),
                              location = loc,
                              type = "quantile",
                              quantile = rep(q, 4),
                              value = qnbinom(rep(q, 4), mu = mu, size = psi),
                              location_name = dat_location_weekly$location_name[1])

  # same for cumulative deaths:
  obs_cum <- data.frame(forecast_date = forecast_date,
                        target = paste((-1:0), "wk ahead cum death"),
                        target_end_date = tail(dat_location_weekly$target_end_date, 2),
                        location = loc,
                        type = "observed",
                        quantile = NA,
                        value = tail(dat_location_weekly$cum_death, 2),
                        location_name = dat_location_weekly$location_name[1])


  point_cum <- data.frame(forecast_date = forecast_date,
                          target = paste(1:4, "wk ahead cum death"),
                          target_end_date = max(dat_location_weekly$target_end_date) + (1:4)*7,
                          location = loc,
                          type = "point",
                          quantile = NA,
                          value = tail(dat_location_weekly$cum_death, 1) +
                            qnbinom(0.5, mu = (1:4)*mu, size = (1:4)*psi),
                          location_name = dat_location_weekly$location_name[1])

  quantiles_cum <- data.frame(forecast_date = forecast_date,
                              target = rep(paste(1:4, "wk ahead cum death"), each = length(q)),
                              target_end_date = rep(max(dat_location_weekly$target_end_date) + (1:4)*7,
                                                    each = length(q)),
                              location = loc,
                              type = "quantile",
                              quantile = rep(q, 4),
                              value = tail(dat_location_weekly$cum_death, 1) +
                                qnbinom(rep(q, 4),
                                        mu = rep((1:4)*mu, each = length(q)),
                                        size = rep((1:4)*psi, each = length(q))),
                              location_name = dat_location_weekly$location_name[1])


  # put everything together:
  forecasts_location <- rbind(obs_inc, point_inc, quantiles_inc,
                              obs_cum, point_cum, quantiles_cum)

  # and add to large table:
  if(is.null(all_forecasts)){
    all_forecasts <- forecasts_location
  }else{
    all_forecasts <- rbind(all_forecasts, forecasts_location)
  }

  # plotting to check plausibility:
  plot(dat_location_weekly$target_end_date, dat_location_weekly$inc_death, ylim = c(0, 2*max(dat_location_weekly$inc_death)),
       xlim = c(min(dat_location_weekly$target_end_date), max(dat_location_weekly$target_end_date) + 28),
       main = loc_name, xlab = "time", ylab = "inc deaths")
  points(quantiles_inc$target_end_date, quantiles_inc$value, pch = 15, col = rgb(0, 0, 0, alpha = 0.2))

  plot(dat_location_weekly$target_end_date, dat_location_weekly$cum_death, ylim = c(0.99, 1.1)*range(dat_location_weekly$cum_death),
       xlim = c(min(dat_location_weekly$target_end_date), max(dat_location_weekly$target_end_date) + 28),
       main = loc_name, xlab = "time", ylab = "cum deaths")
  points(quantiles_cum$target_end_date,
         quantiles_cum$value, pch = 15, col = rgb(0, 0, 0, alpha = 0.2))
}

# store:
write.csv(all_forecasts, file = paste0("forecasts/", forecast_date, "-Germany-KIT-baseline.csv"))
