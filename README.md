# KIT-baseline
Simple baseline model fo German COVID19 Forecast Hub

This naive method proceeds as follows to predict COVID-19 deaths:

* Set predictive mean for incident deaths during next week to observed number in previous week. If no deaths were observed during previous week set to 0.2 (necessary to avoid zero variance in parameteric predictive distribution).
* Estimate overdispersion parameter of a negative binomial distribution from the last five observations (maximum likelihood; with the respective means as defined above).
* Obtain predictive quantiles from negative binomial distribution with the above mean and dispersion parameters.

The same distribution is used as a forecast of incident deaths in the following weeks (two through four weeks ahead). To obtain cumulative death forecasts we assume that incident deaths observed over the following weeks are independent. Due to the additivity of the negative binomial distribution this results in shifted negative biomial distributions as forecasts for the cumulative number of deaths.

A similar (but somewhat more complicated and less parametric) baseline model is used in the US COVID-19 Forecast Hub, see [here](https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/COVIDhub-baseline/metadata-COVIDhub-baseline.txt).