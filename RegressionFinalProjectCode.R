rm(list = ls())

library(MASS)
library(car)
library(lmtest)

setwd("C:/Users/krupa/OneDrive/Desktop/MBS/Spring 2026/Applied Regression/Final Project")

df <- read.csv("EWR_June24_Dataset.csv")

# Date variables
df$FL_DATE <- as.Date(df$FL_DATE, format = "%m/%d/%Y")
df$DAY_OF_MONTH <- as.numeric(format(df$FL_DATE, "%d"))

# Convert categorical variables
df$DAY_OF_WEEK <- factor(df$DAY_OF_WEEK)
df$IS_WEEKEND <- factor(df$IS_WEEKEND)
df$IS_PEAK_HOUR <- factor(df$IS_PEAK_HOUR)
df$FOG_FLAG <- factor(df$FOG_FLAG)
df$LOW_CEILING_FLAG <- factor(df$LOW_CEILING_FLAG)
df$RAIN_FLAG <- factor(df$RAIN_FLAG)

# Remove missing values
df <- na.omit(df)

# Optional: remove extreme taxi-out values
#df <- df[df$TAXI_OUT > 0 & df$TAXI_OUT < 60, ]

# -------------------------------
# Exploratory plots
# -------------------------------

hist(df$TAXI_OUT,
     main = "Distribution of Taxi-Out Time",
     xlab = "Taxi-Out Time in Minutes",
     col = "lightblue")

boxplot(TAXI_OUT ~ IS_PEAK_HOUR, data = df,
        main = "Taxi-Out Time by Peak Hour",
        xlab = "Peak Hour",
        ylab = "Taxi-Out Time")

boxplot(TAXI_OUT ~ RAIN_FLAG, data = df,
        main = "Taxi-Out Time by Rain",
        xlab = "Rain Flag",
        ylab = "Taxi-Out Time")

plot(df$CONGESTION_15M, df$TAXI_OUT,
     main = "Taxi-Out Time vs 15-Minute Congestion",
     xlab = "CONGESTION_15M",
     ylab = "Taxi-Out Time")

plot(df$CONGESTION_60M, df$TAXI_OUT,
     main = "Taxi-Out Time vs 60-Minute Congestion",
     xlab = "CONGESTION_60M",
     ylab = "Taxi-Out Time")

# -------------------------------
# MODEL 1: Full Model
# -------------------------------

full_model <- lm(TAXI_OUT ~ DISTANCE +
                   HOUR_OF_DAY +
                   DAY_OF_WEEK +
                   IS_WEEKEND +
                   IS_PEAK_HOUR +
                   CONGESTION_15M +
                   CONGESTION_60M +
                   CONGESTION_SURFACE_AT_DEP +
                   REL_CONG_15M +
                   REL_CONG_60M +
                   TMP_C +
                   DEW_C +
                   SLP_hPa +
                   WindSpeed_ms +
                   VIS_m +
                   CIG_m +
                   FOG_FLAG +
                   LOW_CEILING_FLAG +
                   RAIN_FLAG,
                 data = df)

summary(full_model)
anova(full_model)

# Multicollinearity check
vif(full_model)

# -------------------------------
# MODEL 2: Stepwise Model
# -------------------------------

step_model <- stepAIC(full_model, direction = "both", trace = TRUE)

final_model <- step_model

summary(final_model)
anova(final_model)

# Final multicollinearity check
vif(final_model)

# -------------------------------
# Model comparison
# -------------------------------

AIC(full_model, final_model)
BIC(full_model, final_model)

# -------------------------------
# Diagnostics for final model
# -------------------------------

par(mfrow = c(2, 2))
plot(final_model)

par(mfrow = c(1, 1))

plot(fitted(final_model), residuals(final_model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals(final_model))
qqline(residuals(final_model), col = "red")

# -------------------------------
# Outlier and influence checks
# -------------------------------

rstudent_vals <- rstudent(final_model)
outliers <- which(abs(rstudent_vals) > 3)
outliers

cooks_d <- cooks.distance(final_model)

plot(cooks_d, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance")
abline(h = 4 / nrow(df), col = "red", lty = 2)

# -------------------------------
# Autocorrelation check
# -------------------------------

dwtest(final_model)

# -------------------------------
# Performance metrics
# -------------------------------

RMSE <- sqrt(mean(residuals(final_model)^2))
MAE <- mean(abs(residuals(final_model)))
R2 <- summary(final_model)$r.squared
Adj_R2 <- summary(final_model)$adj.r.squared

data.frame(RMSE, MAE, R2, Adj_R2)

# -------------------------------
# Prediction intervals
# -------------------------------

predict(final_model, newdata = df[1:5, ], interval = "predict", level = 0.95)

# -------------------------------
# Save outputs
# -------------------------------

capture.output(summary(full_model), file = "taxi_full_model_summary.txt")
capture.output(summary(final_model), file = "taxi_final_model_summary.txt")
capture.output(anova(full_model), file = "taxi_full_model_anova.txt")
capture.output(anova(final_model), file = "taxi_final_model_anova.txt")