# helper function to estimate overdispersino parameter:
fit_psi <- function(vect){
  mu <- pmax(head(vect, length(vect) - 1), 0.2)
  obs <- pmax(tail(vect, length(vect) - 1), 0)
  fct <- function(psi) -sum(dnbinom(obs, mu = mu, size = psi, log = TRUE))
  opt <- optimize(fct, interval = c(0, 100))
  return(opt$minimum)
}

# naive forecasts of incident deaths:
baseline_forecast <- function(dat_weekly, loc, target, forecast_date){

  dat_location_weekly <- subset(dat_weekly, location == loc &
                                  target_end_date <= forecast_date &
                                  weekdays(target_end_date) == "Saturday")
  covered_dates <- unique(dat_location_weekly$target_end_date)
  last_five_obs <- tail(sort(covered_dates), 5)


  # subset:
  dat_location_weekly <- subset(dat_location_weekly, target_end_date %in% last_five_obs)

  loc_name <- dat_location_weekly$location_name[1]

  # use last observation as predictive mean:
  mu <- max(tail(dat_location_weekly[, paste0("inc_", target)], 1), 0.2)

  # estimate overdispersion parameter from last 5 observations:
  if(all(dat_location_weekly[, paste0("inc_", target)] == dat_location_weekly[1, paste0("inc_", target)])){
    psi <- 30
  }else{
    psi <- fit_psi(dat_location_weekly[, paste0("inc_", target)])
  }

  # write out last observed values, point forecsts and quantiles for incident deaths:
  obs_inc <- data.frame(forecast_date = forecast_date,
                        target = paste((-1:0), "wk ahead inc", target),
                        target_end_date = tail(dat_location_weekly$target_end_date, 2),
                        location = loc,
                        type = "observed",
                        quantile = NA,
                        value = tail(dat_location_weekly[, paste0("inc_", target)], 2),
                        location_name = dat_location_weekly$location_name[1])

  point_inc <- data.frame(forecast_date = forecast_date,
                          target = paste(1:4, "wk ahead inc", target),
                          target_end_date = max(dat_location_weekly$target_end_date) + (1:4)*7,
                          location = loc,
                          type = "point",
                          quantile = NA,
                          value = qnbinom(0.5, mu = mu, size = psi),
                          location_name = dat_location_weekly$location_name[1])

  quantiles_inc <- data.frame(forecast_date = forecast_date,
                              target = rep(paste(1:4, "wk ahead inc", target), each = length(q)),
                              target_end_date = rep(max(dat_location_weekly$target_end_date) +
                                                      (1:4)*7, each = length(q)),
                              location = loc,
                              type = "quantile",
                              quantile = rep(q, 4),
                              value = qnbinom(rep(q, 4), mu = mu, size = psi),
                              location_name = dat_location_weekly$location_name[1])

  obs_cum <- data.frame(forecast_date = forecast_date,
                        target = paste((-1:0), "wk ahead cum", target),
                        target_end_date = tail(dat_location_weekly$target_end_date, 2),
                        location = loc,
                        type = "observed",
                        quantile = NA,
                        value = tail(dat_location_weekly[, paste0("cum_", target)], 2),
                        location_name = dat_location_weekly$location_name[1])


  point_cum <- data.frame(forecast_date = forecast_date,
                          target = paste(1:4, "wk ahead cum", target),
                          target_end_date = max(dat_location_weekly$target_end_date) + (1:4)*7,
                          location = loc,
                          type = "point",
                          quantile = NA,
                          value = tail(dat_location_weekly[, paste0("cum_", target)], 1) +
                            qnbinom(0.5, mu = (1:4)*mu, size = (1:4)*psi),
                          location_name = dat_location_weekly$location_name[1])

  quantiles_cum <- data.frame(forecast_date = forecast_date,
                              target = rep(paste(1:4, "wk ahead cum", target), each = length(q)),
                              target_end_date = rep(max(dat_location_weekly$target_end_date) + (1:4)*7,
                                                    each = length(q)),
                              location = loc,
                              type = "quantile",
                              quantile = rep(q, 4),
                              value = tail(dat_location_weekly[, paste0("cum_", target)], 1) +
                                qnbinom(rep(q, 4),
                                        mu = rep((1:4)*mu, each = length(q)),
                                        size = rep((1:4)*psi, each = length(q))),
                              location_name = dat_location_weekly$location_name[1])

  ret <- rbind(obs_inc, point_inc, quantiles_inc,
               obs_cum, point_cum, quantiles_cum)

  return(ret)
}
