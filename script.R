library(tidyverse)
library(gridExtra)

fires.raw <- read_csv("data/forestfires.csv", col_types = cols(
  X = col_factor(levels = 1:9),
  Y = col_factor(levels = 2:9),
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
  mutate(X = factor(paste(X,Y, sep = ""))) %>%
  rename(xy = X) %>%
  select(-c(Y))

parea <- ggplot(data = fires) +
  geom_histogram(aes(area), binwidth = 100) +
  xlab("Area (ha)")
  # coord_cartesian(ylim = c(0, 50))

parea_log <- ggplot(data = fires) +
  geom_histogram(aes(log(area + 1)), binwidth = 0.5) +
  xlab("Log-area")
  # coord_cartesian(xlim = c(0, 10))

grid.arrange(parea, parea_log, ncol = 2)

img.file <- system.file(file.path("data/map.png"))
img <- readPNG("data/map.png")

fires.raw %>%
  ggplot(aes(X, y = reorder(Y, desc(Y)))) +
  background_image(img) +
  geom_jitter(aes(color = log(area+1))) +
  geom_tile(color = "grey", alpha = 0) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = factor(9:1)) +
  coord_cartesian(ylim = c(1,9)) +
  scale_color_gradient(low = "blue", high = "red") +
  ylab("Y")
# METRICS
# library(mltools)
# mltools::rmse(naive.predicted, fires$area)

# notice, MSE is implemented with exact mean and not sum/(n-p)
mse <- function(predicted, actual) {
  mse <- mean((actual - predicted) ^ 2)
  return(mse)
}

rmse <- function(predicted, actual) {
  mse <- mse(predicted, actual)
  rmse <- sqrt(mse)
  return(rmse)
}



# First fit the naive or null linear model
naive.lm <- lm(log(area + 1) ~ 1, fires)
summary(naive.lm)

# which is equal to the mean
near(naive.lm$coefficients[[1]], mean(log(1 + fires$area)))

# we compute rmse and mad but avoiding errors in conversion between log and linear space
# i.e. instead of using the inverse of log(x+1) ...
naive.predicted <- exp(predict(naive.lm, fires, type = "response")) - 1

paste("RMSE:", round(rmse(naive.predicted, fires$area), digits = 2))
paste("MAD:", round(mad(naive.predicted, fires$area), digits = 2))

# Now we fit the complete model, with all predictors
complete.lm <- lm(log(area + 1) ~ ., fires)
summary(complete.lm)

complete.predicted <- exp(predict(complete.lm, fires, type = "response")) - 1
complete.predicted[complete.predicted < 0] <- 0.
paste("RMSE:", round(rmse(complete.predicted, fires$area), digits = 2))
paste("MAD:", round(mad(complete.predicted, fires$area), digits = 2))

anova(naive.lm, complete.lm)
# naive model is not enough

fires.raw %>%
  filter(X == 9, Y == 4)

stm.lm <- lm(log(area + 1) ~ xy + month + day + temp + RH + wind + rain, fires)
summary(stm.lm)
stm.predicted <- exp(predict(stm.lm, fires, type = "response")) - 1
stm.predicted[stm.predicted < 0] <- 0.
paste("RMSE:", round(rmse(stm.predicted, fires$area), digits = 2))
paste("MAD:", round(mad(stm.predicted, fires$area), digits = 2))

anova(stm.lm, complete.lm)
# therefore we can start looking for smaller subsets of predictors

fwi.lm <- lm(log(area + 1) ~ FFMC + DMC + DC + ISI, fires)
summary(fwi.lm)
fwi.predicted <- exp(predict(fwi.lm, fires, type = "response")) - 1
fwi.predicted[fwi.predicted < 0] <- 0.
paste("RMSE:", round(rmse(fwi.predicted, fires$area), digits = 2))
paste("MAD:", round(mad(fwi.predicted, fires$area), digits = 2))

m.lm <- lm(log(area + 1) ~ temp + RH + wind + rain, fires)
summary(m.lm)
m.predicted <- exp(predict(m.lm, fires, type = "response")) - 1
m.predicted[m.predicted < 0] <- 0.
paste("RMSE:", round(rmse(m.predicted, fires$area), digits = 2))
paste("MAD:", round(mad(m.predicted, fires$area), digits = 2))

# SAME AS
# m.glm <- glm(area ~ temp + RH + wind + rain, family = gaussian(link="log"), fires)
# summary(m.glm)
# m.predicted <- exp(predict(m.glm, fires, type = "response")) - 1
# m.predicted[m.predicted < 0] <- 0.
# paste("RMSE:", round(rmse(m.predicted, fires$area), digits = 2))
# paste("MAD:", round(mad(m.predicted, fires$area), digits = 2))

# TWO-PART Model
# One notable feature of the two-part model is that separate sets of predictors
# can be specified for the binary and continuous regression equations. 
# 
# Creating two datasets

fires %>%
  mutate(area.gt0 = area > 0) %>% # greater than 0 binary variable
  ggplot(aes(area.gt0)) +
  geom_bar(aes(fill = area.gt0)) +
  xlab("Area > 0")

# binary fire presence dataset
fires.bin <- fires %>%
  mutate(area.gt0 = factor(area > 0)) %>%
  select(-area)

summary(fires.bin)

# area abundance dataset
fires.ab <- fires %>%
  filter(area > 0)

m.bin.glm <- glm(area.gt0 ~ temp + RH + wind + rain, family = "binomial", data = fires.bin)
summary(m.bin.glm)

m.ab.lm <- lm(log(area +1) ~ temp + RH + wind + rain, data = fires.ab)
summary(m.ab.lm)

fwi.bin.glm <- glm(area.gt0 ~ FFMC + DMC + DC + ISI, family = "binomial", fires.bin)
summary(fwi.bin.glm)

fwi.ab.lm <- lm(log(area +1) ~ FFMC + DMC + DC + ISI, fires.ab)
summary(fwi.ab.lm)

# compute E(area) = Pr(Z = 1)E(Y | Z = 1)
pred.m.bin <- predict(m.bin.glm, newdata = fires, type = "response")
pred.m.ab <- exp(predict(m.ab.lm, newdata = fires, type = "response")) - 1

pred.fwi.bin <- predict(fwi.bin.glm, newdata = fires, type = "response")
pred.fwi.ab <- exp(predict(fwi.ab.lm, newdata = fires, type = "response")) - 1

pred.m.comb <- pred.m.bin * pred.m.ab
pred.m.comb[pred.m.comb < 0] <- 0.
paste("RMSE:", round(rmse(pred.m.comb, fires$area), digits = 2))
paste("MAD:", round(mad(pred.m.comb, fires$area), digits = 2))

pred.fwi.comb <- pred.fwi.bin * pred.fwi.ab
pred.fwi.comb[pred.fwi.comb < 0] <- 0.
paste("RMSE:", round(rmse(pred.fwi.comb, fires$area), digits = 2))
paste("MAD:", round(mad(pred.fwi.comb, fires$area), digits = 2))

pred.fwim.comb <- pred.fwi.bin * pred.m.ab
pred.fwim.comb[pred.fwim.comb < 0] <- 0.
paste("RMSE:", round(rmse(pred.fwim.comb, fires$area), digits = 2))
paste("MAD:", round(mad(pred.fwim.comb, fires$area), digits = 2))

pred.mfwi.comb <- pred.m.bin * pred.fwi.ab
pred.mfwi.comb[pred.mfwi.comb < 0] <- 0.
paste("RMSE:", round(rmse(pred.mfwi.comb, fires$area), digits = 2))
paste("MAD:", round(mad(pred.mfwi.comb, fires$area), digits = 2))

# REC Curve
rec_curve <- function(predicted, actual) {
  x <- seq(0, 15, 0.1)
  y <- x %>%
    map_dbl(~ 100 * sum(abs(predicted - actual) < .x) / length(predicted))
  
  return(list("x" = x, "y" = y))
}

rec.m <- rec_curve(pred.m.comb, fires$area)
rec.fwi <- rec_curve(fwi.predicted, fires$area)

ggplot() +
  geom_line(aes(rec.m$x, rec.m$y, color = "M")) +
  geom_line(aes(rec.fwi$x, rec.fwi$y, color= "FWI")) +
  labs(title = "REC curve", x = "Absolute error", y = "Tolerance (%)") +
  theme(plot.title = element_text(hjust = 0.5))

# TODO: add bootstrap for parameter confidence intervals

# fires_bin.test.result <- fires_bin.test %>%
#   mutate(model_prob = predict(complete.glm, newdata = ., type = "response"),
#          model_pred = factor(1 * (model_prob > .5), levels = c(0, 1)),
#          area_bin = factor(1 * area.gt0, levels = c(0, 1)),
#          accurate = (model_pred == area_bin)) %>%
#   select(area.gt0, model_prob, model_pred, area_bin, accurate)
# 
# sum(fires_bin.test.result$accurate)/nrow(fires_bin.test.result)
# 
# library(caret)
# confusionMatrix(predict(complete.glm, newdata = fires_bin.test), fires_bin.test$area.gt0)
# 
# # Confusion matrix
# # fires_bin.test.result %>%
# #   group_by(area_bin, model_pred) %>%
# #   count() %>%
# #   ggplot(aes(x = c(0, 0, 1, 1), y = c(0, 1, 0, 1))) +
# #     geom_tile(aes(fill = Y), colour = "white") +
# #     geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
# #     scale_fill_gradient(low = "blue", high = "red") +
# #     theme_bw() + theme(legend.position = "none")
# 
# library(ROCR)
# predictions <- prediction(fires_bin.test.result$model_pred, fires_bin.test.result$area_bin)
# sens <- data.frame(x=unlist(performance(predictions, "tpr")@x.values), 
#                    y=unlist(performance(predictions, "tpr")@y.values))
# spec <- data.frame(x=unlist(performance(predictions, "tnr")@x.values), 
#                    y=unlist(performance(predictions, "tnr")@y.values))
# 
# sens %>% ggplot(aes(x,y)) + 
#   geom_line() + 
#   geom_line(data=spec, aes(x,y,col="red")) +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
#   labs(x='Cutoff', y="Sensitivity") +
#   theme(axis.title.y.right = element_text(colour = "red"), legend.position="none")
