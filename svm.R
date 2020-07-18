library(tidyverse)
library(mlr3)
library(mlr3viz)
library(mlr3learners)
library(mlr3tuning)
library(paradox)
library(data.table)

fires.raw <- read_csv("data/forestfires.csv", col_types = cols(
  X = col_factor(levels = 1:9),
  Y = col_factor(levels = 1:9),
  month = col_factor(levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")),
  day = col_factor(levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")),
  FFMC = col_double(),
  DMC = col_double(),
  DC = col_double(),
  ISI = col_double(),
  temp = col_double(),
  RH = col_double(),
  wind = col_double(),
  rain = col_double(),
  area = col_double()
))

fires <- fires.raw %>%
  mutate(X = factor(paste(X,Y, sep = "")), log_area = log(area + 1)) %>%
  rename(xy = X) %>%
  select(-c(Y))

# STFWI: spatial, temporal and the four FWI components (Fire Weather Index)
stfwi.cols <- c("X", "Y", "month", "day", "FFMC", "DMC", "DC", "ISI")
# STM: spatial, temporal and weather components
stm.cols <- c("X", "Y", "month", "day", "temp", "RH", "wind", "rain")
# FWI: only the four FWI components
fwi.cols <- c("FFMC", "DMC", "DC", "ISI")
# M: only the four weather conditions
m.cols <- c("temp", "RH", "wind", "rain")

task <- TaskRegr$new(id = "fires", backend = fires[, c(m.cols, "log_area")], target = "log_area")
task_area <- TaskRegr$new(id = "fires", backend = fires[, c(m.cols, "area")], target = "area")

autoplot(task, type = "pairs")

svm <- lrn("regr.svm", kernel = "radial", cost = 3, gamma = 2^-3, epsilon = 0.4, type = "eps-regression")
svm$train(task)
svm.log.pred <- svm$predict(task)
autoplot(svm.log.pred)

svm.pred <- exp(svm.log.pred$response) - 1
svm.pred[svm.pred < 0] <- 0.

paste("RMSE:", round(rmse(svm.pred, fires$area), digits = 2))
paste("MAD:", round(mad(svm.pred, fires$area), digits = 2))

# CV Tuning
learner <- lrn("regr.svm", kernel = "radial", type = "eps-regression", cost = 3, gamma = 2^-3, epsilon = 0.4)

design = data.table(expand.grid(cost = c(0.1, 1, 10),
                                gamma = c(2^-7, 2^-5, 2^-3, 2^-1),
                                epsilon = c(0.001, 0.01, 0.1, 0.5)))

tune_ps <- ParamSet$new(list(
  ParamDbl$new("cost", lower = 10^-1, upper = 10),
  ParamDbl$new("gamma", lower = 2^-7, upper = 2^-1),
  ParamDbl$new("epsilon", lower = 0.001, upper = 0.5)
))

tuner = tnr("design_points", design = design)

instance = TuningInstance$new(
  task = task_area,
  learner =  learner,
  resampling = rsmp("cv", folds = 5),
  measures = msr("regr.rmse"),
  param_set = tune_ps,
  terminator = term("none")
)

tuner$tune(instance)

learner$param_set$values <- instance$result$params
learner$train(task_area)

pred <- learner$predict(task_area)
autoplot(pred)
# 
# pred <- exp(log.pred$response) - 1
# pred[pred < 0] <- 0.

paste("RMSE:", round(rmse(pred$response, fires$area), digits = 2))
paste("MAD:", round(mad(pred$response, fires$area), digits = 2))

# fires %>%
#   ggplot() +
#   geom_density(aes(log(rain+1), color = "Log")) +
#   geom_density(aes(rain, color = "Normal")) +
#   geom_density(aes(sqrt(rain), color = "Sqrt"))
