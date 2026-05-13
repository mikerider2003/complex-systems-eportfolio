```{r}
# =========================
# 1. Load packages
# =========================
library(scatterplot3d)

# =========================
# 2. Load dataset
# =========================
data <- read.csv("traffic_timeseries.csv", stringsAsFactors = FALSE)
```

```{r}
# =========================
# 3. Inspect structure
# =========================
head(data)
str(data)
names(data)

# =========================
# 4. Convert timestamp
# =========================
data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%S")

```

```{r}
# =========================
# 5. Select time series
# =========================
x <- data$volume

# Quick checks
length(x)
sum(is.na(x))
```


```{r}
# =========================
# 6. Plot original time series
# =========================

# Plot the original traffic volume time series
plot(
  data$timestamp,
  x,
  type = "l",
  main = "Traffic Volume Time Series",
  xlab = "Time",
  ylab = "Traffic Volume"
)

# Save the figure in high resolution for the report
png("time_series_traffic.png", width = 2000, height = 1200, res = 300)

plot(
  data$timestamp,
  x,
  type = "l",
  main = "Traffic Volume Time Series",
  xlab = "Time",
  ylab = "Traffic Volume"
)

dev.off()

```

```{r}
# =========================
# 7. Set delay parameter
# =========================
tau <- 10

# =========================
# 8. Construct delay embedding
# =========================
x1 <- x[1:(length(x) - 2 * tau)]
x2 <- x[(1 + tau):(length(x) - tau)]
x3 <- x[(1 + 2 * tau):length(x)]

# Check lengths
length(x1)
length(x2)
length(x3)

```

```{r}
# =========================
# Normalize real traffic signal
# =========================

x_scaled <- scale(x)[,1]

# =========================
# Construct 2D embedding
# =========================

s1_real <- x_scaled[1:(length(x_scaled) - tau)]
s2_real <- x_scaled[(1 + tau):length(x_scaled)]

# =========================
# Save high-resolution PNG
# =========================

png(
  "phase2d_traffic.png",
  width = 2000,
  height = 1600,
  res = 300
)

plot(
  s1_real,
  s2_real,
  pch = 16,
  col = rgb(0, 0, 0.6, 0.15),
  xlab = expression(x(t)),
  ylab = expression(x(t + tau)),
  main = "Phase Space Reconstruction (2D)"
)

# Add trajectory segment
lines(
  s1_real[1:100],
  s2_real[1:100],
  col = "red",
  lwd = 2
)

legend(
  "topright",
  legend = c("States", "Trajectory segment"),
  col = c(rgb(0,0,0.6,0.3), "red"),
  pch = c(16, NA),
  lty = c(NA, 1),
  bty = "n"
)

dev.off()

# =========================
# Display plot in VSCode/RStudio
# =========================

plot(
  s1_real,
  s2_real,
  pch = 16,
  col = rgb(0, 0, 0.6, 0.15),
  xlab = expression(x(t)),
  ylab = expression(x(t + tau)),
  main = "Phase Space Reconstruction (2D)"
)

lines(
  s1_real[1:100],
  s2_real[1:100],
  col = "red",
  lwd = 2
)

legend(
  "topright",
  legend = c("States", "Trajectory segment"),
  col = c(rgb(0,0,0.6,0.3), "red"),
  pch = c(16, NA),
  lty = c(NA, 1),
  bty = "n"
)
```


```{r}
# =========================
# Autocorrelation analysis
# =========================

# Save high-resolution PNG
png(
  "acf_traffic.png",
  width = 2000,
  height = 1200,
  res = 300
)

acf(
  x,
  lag.max = 100,
  main = "Autocorrelation Function of Traffic Volume"
)

dev.off()

# Display plot in VS Code / RStudio
acf(
  x,
  lag.max = 100,
  main = "Autocorrelation Function of Traffic Volume"
)
```

```{r}
# =========================
# Surrogate signal comparison
# =========================

# Generate noisy cyclic surrogate signal
t <- 1:length(x_scaled)

surrogate <- sin(2 * pi * t / 96) +
  rnorm(length(x_scaled), mean = 0, sd = 0.3)

# Normalize surrogate
surrogate <- scale(surrogate)[,1]

# =========================
# Construct surrogate embedding
# =========================

s1_sur <- surrogate[1:(length(surrogate) - tau)]
s2_sur <- surrogate[(1 + tau):length(surrogate)]

# =========================
# Save high-resolution PNG
# =========================

png(
  "phase2d_surrogate.png",
  width = 2000,
  height = 1600,
  res = 300
)

plot(
  s1_sur,
  s2_sur,
  pch = 16,
  col = rgb(0.5, 0, 0, 0.15),
  xlab = expression(x(t)),
  ylab = expression(x(t + tau)),
  main = "Phase Space Reconstruction of Noisy Cyclic Surrogate"
)

dev.off()

# =========================
# Display plot in VSCode/RStudio
# =========================

plot(
  s1_sur,
  s2_sur,
  pch = 16,
  col = rgb(0.5, 0, 0, 0.15),
  xlab = expression(x(t)),
  ylab = expression(x(t + tau)),
  main = "Phase Space Reconstruction of Noisy Cyclic Surrogate"
)
```

