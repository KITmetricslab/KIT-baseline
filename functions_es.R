# wrapper function to generate forecasts from ets in our format:
forecast_esm <- function(dat, forecast_date, location, target_type = c("case", "death"),
                         n_training = 12, location_name = location, n_sim = 10000){
  # get last observations (incident):
  recent_obs <- dat[dat$location == location &
                      dat$date >= forecast_date - 7*n_training &
                      dat$date <= forecast_date, paste("inc", target_type, sep = "_")]
  if(length(recent_obs) != n_training) stop("Something went wrong when extracting the most recent incident values.")

  # get very last cumulative observation to shift forecasts later
  last_obs_cum <- dat[dat$location == location &
                      dat$date >= forecast_date - 7 &
                      dat$date <= forecast_date, paste("cum", target_type, sep = "_")]
  if(length(last_obs_cum) != 1) stop("Something went wrong when extracting the last cumulative value.")

  recent_obs[recent_obs == 0] <- 0.5 # otherwise esm function cannot be applied
  ts <- ts(recent_obs)

  # fit:
  fit <- ets(ts, model="MMN")

  # simulate:
  sim <- sim_many(fit = fit, n = n_sim)
  # compute quantiles:
  inc_quantiles <- compute_pred_quantiles(sim = sim, inc_or_cum = "inc")
  cum_quantiles <- compute_pred_quantiles(sim = sim, inc_or_cum = "cum")
  cum_quantiles <- cum_quantiles + last_obs_cum # add last obs
  rm(sim)

  # store in data.frame:
  quantile_levels <- c(0.01, 0.025, 1:19/20, 0.975, 0.99)
  n_levels <- length(quantile_levels)
  inc_forecasts <- data.frame(forecast_date = forecast_date,
                              target = rep(paste(1:4, "wk ahead inc", target_type), each = n_levels),
                              target_end_date = rep(next_monday(forecast_date) - 2 + (1:4)*7, each = n_levels),
                              location = location,
                              location_name	= location_name,
                              type = "quantile",
                              quantile = rep(quantile_levels, 4),
                              value = round(as.vector(inc_quantiles)))
  inc_point_forecasts <- subset(inc_forecasts, quantile == 0.5)
  inc_point_forecasts$type <- "point"
  inc_point_forecasts$quantile <- NA

  cum_forecasts <- data.frame(forecast_date = forecast_date,
                              target = rep(paste(1:4, "wk ahead cum", target_type), each = n_levels),
                              target_end_date = rep(next_monday(forecast_date) - 2 + (1:4)*7, each = n_levels),
                              location = location,
                              location_name	= location_name,
                              type = "quantile",
                              quantile = rep(quantile_levels, 4),
                              value = round(as.vector(cum_quantiles)))
  cum_point_forecasts <- subset(cum_forecasts, quantile == 0.5)
  cum_point_forecasts$type <- "point"
  cum_point_forecasts$quantile <- NA

  result <- rbind(inc_point_forecasts, inc_forecasts,
                  cum_point_forecasts, cum_forecasts)

  return(result)
}

# wrapper for simulation:
sim_many <- function(fit, n){
  sim <- matrix(ncol = 4, nrow = n)
  for(i in 1:n) sim[i, ] <- simulate(fit, nsim = 4)
  return(sim)
}

# helper function to compute quantiles of inc and cum:
compute_pred_quantiles <- function(sim, levels = c(0.01, 0.025, 1:19/20, 0.975, 0.99), inc_or_cum = "inc"){
  if(inc_or_cum == "cum"){
    for(i in 1:nrow(sim)) sim[i, ] <- cumsum(sim[i, ])
  }

  quant <- matrix(nrow = length(levels), ncol = 4)
  rownames(quant) <- paste0("q", levels)

  for(i in seq_along(levels)){
    quant[i, ] <- apply(sim, 2, quantile, levels[i])
  }
  return(quant)
}
