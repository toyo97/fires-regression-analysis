library(tidyverse)
library(mlr3)
library("mlr3viz")
library("mlr3learners")
library("ranger")


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

task.m <- TaskRegr$new(id = "fires", backend = fires[, c(m.cols, "log_area")], target = "log_area")
task.fwi <- TaskRegr$new(id = "fires", backend = fires[, c(fwi.cols, "log_area")], target = "log_area")

autoplot(task.m, type = "pairs")
autoplot(task.fwi, type = "pairs")

svm <- lrn("regr.svm", kernel = radial, cost = 3, gamma = 2^(-3), epsilon = 0.4)
svm$train(task.m)
svm.log.pred <- svm$predict(task.m)
autoplot(svm.log.pred)

svm.pred <- exp(svm.log.pred$response) - 1
svm.pred[svm.pred < 0] <- 0.

paste("RMSE:", round(rmse(svm.pred, fires$area), digits = 2))
paste("MAD:", round(mad(svm.pred, fires$area), digits = 2))

# RANDOM FOREST
rf <- lrn("regr.ranger")
rf$train(task.m)
rf.log.pred <- rf$predict(task.m)
autoplot(rf.log.pred)

rf.pred <- exp(rf.log.pred$response) - 1
rf.pred[rf.pred < 0] <- 0.

paste("RMSE:", round(rmse(rf.pred, fires$area), digits = 2))
paste("MAD:", round(mad(rf.pred, fires$area), digits = 2))





# fires %>%
#   ggplot() +
#   geom_density(aes(log(rain+1), color = "Log")) +
#   geom_density(aes(rain, color = "Normal")) +
#   geom_density(aes(sqrt(rain), color = "Sqrt"))
