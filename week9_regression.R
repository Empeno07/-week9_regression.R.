rm(list = ls())
library(ggplot2)

str(cars)
summary(cars)
# Speed range: 4 to 25 mph

ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Stopping distance rises with speed",
       x = "Speed (mph)", y = "Stopping distance (ft)")

# Plot: positive direction, roughly linear, more spread at high speed.

cor(cars$speed, cars$dist)
cor.test(cars$speed, cars$dist)
# r = 0.807, p about 1.5e-12, 95% CI for r about 0.68 to 0.89

fit <- lm(dist ~ speed, data = cars)
summary(fit)
confint(fit)

# Slope: each extra 1 mph is associated with about 3.93 ft more stopping distance.
# Intercept: -17.58 ft at 0 mph. Not read literally, since 0 mph is outside
# the data (4 to 25 mph) and a negative distance is impossible.
# R-squared: 0.651. Speed accounts for about 65% of the variation in
# stopping distance; the rest is left over.

cars$fitted <- fitted(fit)
cars$resid  <- resid(fit)

ggplot(cars, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals against fitted values",
       x = "Fitted stopping distance (ft)", y = "Residual (ft)")

# Residuals spread out more to the right: about 9 ft at 12 mph or less,
# about 19 ft at 18 mph or more. I trust predictions less at high speed.

predict(fit, newdata = data.frame(speed = 15), interval = "prediction")
# fit 41.4 ft, 95% prediction interval about 10.2 to 72.6 ft (wide).
# Why not 40 mph: the data only cover 4 to 25 mph, and the line may not hold outside that range.
# Causation: faster cars in this data tend to need longer to stop, but this does not show why, and it cannot tell what would happen if a speed limit
# changed. Other factors: road surface, brakes, reaction time.