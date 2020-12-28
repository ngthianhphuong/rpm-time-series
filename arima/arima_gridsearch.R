library(readxl)
library(latex2exp)
training_set <- read_excel("~/MSc Data Management/Thèse professionnelle/ORION_04_Site.xlsx", sheet=3)
val_set <- read_excel("~/MSc Data Management/Thèse professionnelle/ORION_04_Site.xlsx", sheet=1)

training_set = training_set[c("Date", "Site", "RPM")]
val_set = val_set[c("Date", "Site", "RPM")]

View(training_set)
View(val_set)
#install.packages("xts")
library("xts")

sites = c("business insider", "ca m'intéresse", "capital", "ce soir tv",
          "cuisine actuelle", "femme actuelle", "gala", "geo", "neon",
          "télé 2 semaines", "télé loisirs", "voici")


library(tidyverse)
library(forecast)
library(lubridate)
#install.packages("furrr")
library(furrr)
#install.packages("tsibble")
library(tsibble)
#install.packages("brotools")
library(brotools)
library(dplyr)
library(purrr)
library(tidyr)
#install.packages("fpp2")
library(fpp2)

to_tibble <- function(forecast_object){
  point_estimate <- forecast_object$mean %>%
    as_tsibble() %>%
    rename(point_estimate = value,
           date = index)
  
  upper <- forecast_object$upper %>%
    as_tsibble() %>%
    spread(key, value) %>%
    rename(date = index,
           upper80 = `80%`,
           upper95 = `95%`)
  
  lower <- forecast_object$lower %>%
    as_tsibble() %>%
    spread(key, value) %>%
    rename(date = index,
           lower80 = `80%`,
           lower95 = `95%`)
  reduce(list(point_estimate, upper, lower), full_join)
}

# parameters in case of additive model
additive_params = data.frame(rbind(
c("business insider", 3, 1, 2, 0, 0, 0, 7),
c("cuisine actuelle", 1, 1, 1, 1, 1, 1, 7),
c("ca m'intéresse", 2, 1, 2, 0, 0, 0, 7),
c("capital", 2, 1, 1, 0, 0, 0, 7),
c("ce soir tv", 1, 1, 1, 1, 1, 1, 7),
c("femme actuelle", 3, 1, 2, 0, 0, 0, 7),
c("gala", 2, 1, 2, 0, 0, 0, 7),
c("geo", 3, 1, 3, 1, 1, 1, 7),
c("neon", 1, 1, 1, 0, 0, 0, 7),
c("télé 2 semaines", 1, 1, 1, 1, 1, 1, 7),
c("télé loisirs", 6, 1, 1, 0, 0, 0, 7),
c("voici", 1, 1, 1, 1, 1, 1, 7)))


colnames(additive_params) = c("site", "p", "d", "q", "P", "D", "Q", "S")
rownames(additive_params) = additive_params$site
additive_params$site = NULL
additive_params = droplevels(additive_params)
View(additive_params)

# create dataframe that contains best aic
aic_df = data.frame(site=character(),
                    p=integer(),
                    d=integer(),
                    q=integer(),
                    P=integer(),
                    D=integer(),
                    Q=integer(),
                    aic=double(),
                    stringsAsFactors = F)

# create dataframe that contains best RMSE
rmse_df = data.frame(site=character(),
                    p=integer(),
                    d=integer(),
                    q=integer(),
                    P=integer(),
                    D=integer(),
                    Q=integer(),
                    rmse=double(),
                    stringsAsFactors = F)

for (site in c("télé 2 semaines"))
{
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_train$Date = as.Date(subset_train$Date, "%Y - %m - %d")
  subset_ts = xts(subset_train$RPM, subset_train$Date)
  subset_ts = log(subset_ts)
  
  site_val = subset(val_set, Site==site, select=c("Date", "RPM"))
  site_val_ts = ts(data = site_val$RPM)
  site_val_ts = log(site_val_ts)
  
  
  # retrieve parameters
  order_list=list("p" = seq(0, 1),
                  "d" = seq(0, 1),
                  "q" = seq(0, 1)) %>%
    cross() %>%
    map(lift(c))
  
  season_list <- list("P" = 1,
                      "D" = 1,
                      "Q" = 1,
                      "period" = 7)  %>%
    cross() %>%
    map(lift(c))
  
  orderdf <- tibble("order" = order_list)
  seasondf <- tibble("season" = season_list)
  
  # Create grid of hyper-parameters
  hyper_parameters_df <- crossing(orderdf, seasondf)
  nrows <- nrow(hyper_parameters_df)
  
  View(hyper_parameters_df)
  # model with lowest AIC on training set
  
  models_df_train = hyper_parameters_df %>%
    mutate(models = map2(.x = order,
                         .y = season,
                         ~possibly(arima, otherwise = NULL)(x = subset_ts, order = .x, seasonal = .y))) %>% 
    filter(models != 'NULL') %>% 
    mutate(aic = map_dbl(models, "aic"))
  
  best_model_train = models_df_train %>%
    filter(aic == min(models_df_train$aic, na.rm = TRUE))
  
  
  #Run grid search of ARIMA models
  models_df_val <- hyper_parameters_df %>%
    mutate(models =
             map2(.x = order,
                  .y = season,
                  ~possibly(arima, otherwise = NULL)(x = subset_ts, order = .x, seasonal = .y)))
  
  models_df_val = models_df_val %>% drop_na()
  
  # Estimate RSME for each candidate
  models_df_val <- models_df_val %>%
    mutate(forecast = map(models, ~possibly(forecast, otherwise = NULL)(., h = 31))) %>%
    mutate(point_forecast = map(forecast, ~.$`mean`)) %>%
    mutate(true_value = rerun(nrows, site_val_ts)) %>%
    mutate(rmse = map2_dbl(point_forecast, true_value,
                           ~sqrt(mean((exp(as.numeric(.x)) - exp(as.numeric(.y))) ** 2))))
  
  best_model_val <- models_df_val %>%
    filter(rmse == min(rmse, na.rm = TRUE))
  site_aic = c(site,
               best_model_train$order[[1]][[1]], 
               best_model_train$order[[1]][[2]], 
               best_model_train$order[[1]][[3]],
               best_model_train$season[[1]][[1]], 
               best_model_train$season[[1]][[2]], 
               best_model_train$season[[1]][[3]], 
               best_model_train$aic)
  
  aic_df[nrow(aic_df)+1,] = site_aic
  
  site_rmse = c(site,
                best_model_val$order[[1]][[1]], 
                best_model_val$order[[1]][[2]], 
                best_model_val$order[[1]][[3]],
                best_model_val$season[[1]][[1]], 
                best_model_val$season[[1]][[2]], 
                best_model_val$season[[1]][[3]], 
                best_model_val$rmse)
  
  rmse_df[nrow(rmse_df)+1,] = site_rmse
}

View(aic_df)
View(rmse_df)










                    
                    
