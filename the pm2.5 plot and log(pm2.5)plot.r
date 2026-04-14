library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


df <- read_csv("data_processed/pm25_daily_clean_2022_2025.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(station_id, date)

# Plot daily PM2.5 (raw) for each station
ggplot(df, aes(x = date, y = pm25_daily)) +
  geom_line() +
  facet_wrap(~ station_id, ncol = 1, scales = "free_y") +
  labs(
    title = "Daily PM2.5 (µg/m³)",
    x = "Date",
    y = "PM2.5 (daily mean)"
  ) +
  theme_minimal()

# plot log-transformed y = log(pm25_daily + 0.1)
df <- df %>% mutate(y = log(pm25_daily + 0.1))

ggplot(df, aes(x = date, y = y)) +
  geom_line() +
  facet_wrap(~ station_id, ncol = 1) +
  labs(
    title = "Log-transformed Daily PM2.5: y = log(PM2.5 + 0.1)",
    x = "Date",
    y = "y"
  ) +
  theme_minimal()


df <- read.csv("data_processed/pm25_daily_clean_2022_2025.csv")
df$date <- as.Date(df$date)

# Raw scale
p1 <- ggplot(df, aes(x = date, y = pm25_daily)) +
  geom_line(linewidth = 0.3) +
  labs(
    title = expression("Daily PM"[2.5]*" (raw scale, "*mu*"g/m"^3*")"),
    x = "Date",
    y = expression("PM"[2.5]*" (daily mean)")
  ) +
  theme_bw()

# Log scale
p2 <- ggplot(df, aes(x = date, y = y)) +
  geom_line(linewidth = 0.3) +
  labs(
    title = expression("Log-transformed Daily PM"[2.5]*": y = log(PM"[2.5]*" + 0.1)"),
    x = "Date",
    y = "y"
  ) +
  theme_bw()


# month boxplot
p3 <- ggplot(df, aes(x = factor(month), y = y)) +
  geom_boxplot(fill = "lightblue", outlier.size = 0.8) +
  labs(
    title = expression("Monthly Distribution of log(PM"[2.5]*" + 0.1)"),
    x = "Month",
    y = "y"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_bw()

# Histogram of y_t
p4 <- ggplot(df, aes(x = y)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", linewidth = 0.2) +
  labs(
    title = expression("Histogram of y"[t]*" = log(PM"[2.5]*" + 0.1)"),
    x = "y",
    y = "Count"
  ) +
  theme_bw()


