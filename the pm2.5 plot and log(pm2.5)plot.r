library(readr)
library(dplyr)
library(ggplot2)

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

