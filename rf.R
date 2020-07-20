library(tidyverse)
library(mlr3)
library(mlr3viz)
library(mlr3learners)
library(ranger)


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

# set up the task using log_area as target variable
task <- TaskRegr$new(id = "fires", backend = fires[, -12], target = "log_area")

rf <- lrn("regr.ranger", num.trees = 500, mtry = 4, splitrule = "variance", max.depth = 20) # default parameters (T=500, mtry=auto, split)
rf$train(task)
rf.log.pred <- rf$predict(task)
autoplot(rf.log.pred)

rf.pred <- exp(rf.log.pred$response) - 1
rf.pred[rf.pred < 0] <- 0.

paste("RMSE:", round(rmse(rf.pred, fires$area), digits = 2))
paste("MAD:", round(mad(rf.pred, fires$area), digits = 2))

train_set = sample(task$nrow, 0.6 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)

rf$train(task, row_ids = train_set)
rf.log.pred <- rf$predict(task, row_ids = test_set)
autoplot(rf.log.pred)

rf.pred <- exp(rf.log.pred$response) - 1
rf.pred[rf.pred < 0] <- 0.

paste("RMSE:", round(rmse(rf.pred, fires$area[test_set]), digits = 2))
paste("MAD:", round(mad(rf.pred, fires$area[test_set]), digits = 2))
