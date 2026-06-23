# R Script to visualize Boston's historical temperature data
# Saved as plot_boston_temperatures.R

library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# 1. Load the dataset
if (!file.exists("boston_temperatures.csv")) {
  stop("boston_temperatures.csv not found! Please run 'download_boston_temperatures.R' first to fetch the data.")
}

cat("Loading dataset...\n")
temp_data <- read_csv("boston_temperatures.csv", show_col_types = FALSE)

# 2. Extract year, month, and season
cat("Processing dates...\n")
temp_data <- temp_data %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    day_of_year = yday(date),
    # Assign seasons
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      month %in% c("Mar", "Apr", "May") ~ "Spring",
      month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      month %in% c("Sep", "Oct", "Nov") ~ "Autumn"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

# Define custom aesthetics (premium look)
bg_color <- "#FAFAFA"
text_color <- "#2B2B2B"
grid_color <- "#E5E5E5"

custom_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA),
    text = element_text(color = text_color),
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "#555555", size = 13, margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "#777777", margin = margin(t = 15)),
    panel.grid.major = element_line(color = grid_color, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(color = text_color),
    legend.position = "bottom"
  )

# =========================================================================
# Plot 1: Annual Mean Temperature Trend (1940-2025)
# =========================================================================
cat("Generating Plot 1: Annual Temperature Trends...\n")

# Calculate annual averages (exclude 2026 as it is incomplete)
annual_summary <- temp_data %>%
  filter(year < 2026) %>%
  group_by(year) %>%
  summarize(
    mean_temp_f = mean(temp_mean_f, na.rm = TRUE),
    mean_max_f = mean(temp_max_f, na.rm = TRUE),
    mean_min_f = mean(temp_min_f, na.rm = TRUE),
    .groups = "drop"
  )

p1 <- ggplot(annual_summary, aes(x = year, y = mean_temp_f)) +
  # Background range ribbon between annual max/min mean
  geom_ribbon(aes(ymin = mean_min_f, ymax = mean_max_f), fill = "#1E88E5", alpha = 0.08) +
  # Annual average data points
  geom_point(color = "#37474F", size = 2, alpha = 0.6) +
  # Connecting line
  geom_line(color = "#37474F", linewidth = 0.5, alpha = 0.4) +
  # Trend line using Local Regression (LOESS)
  geom_smooth(method = "loess", color = "#D81B60", linewidth = 1.5, se = TRUE, fill = "#D81B60", alpha = 0.15) +
  labs(
    title = "Boston Long-Term Temperature Trend (1940 - 2025)",
    subtitle = "Annual mean temperature (points/line) with a LOESS smooth trend curve. Shaded area shows the range between average daily min/max.",
    x = "Year",
    y = "Average Temperature (°F)",
    caption = "Source: Open-Meteo Historical Weather API (ERA5 Reanalysis)"
  ) +
  scale_x_continuous(breaks = seq(1940, 2025, by = 10)) +
  scale_y_continuous(labels = function(x) paste0(x, "°F")) +
  custom_theme

# Save Plot 1
ggsave("boston_annual_trend.png", plot = p1, width = 10, height = 6, dpi = 300)
cat("Saved plot 1: boston_annual_trend.png\n")


# =========================================================================
# Plot 2: Seasonal Temperature Range (Boxplot of Daily Mean Temp by Month)
# =========================================================================
cat("Generating Plot 2: Monthly Temperature Distributions...\n")

p2 <- ggplot(temp_data, aes(x = month, y = temp_mean_f, fill = season)) +
  geom_boxplot(outlier.color = "#CFD8DC", outlier.alpha = 0.4, outlier.size = 1) +
  scale_fill_manual(values = c(
    "Winter" = "#29B6F6", # Cool light blue
    "Spring" = "#66BB6A", # Soft green
    "Summer" = "#FFCA28", # Sunny yellow
    "Autumn" = "#FF7043"  # Warm orange-red
  )) +
  labs(
    title = "Boston Monthly Temperature Range & Distributions",
    subtitle = "Boxplots of daily mean temperatures (1940 - 2026) colored by season.",
    x = "Month",
    y = "Daily Mean Temperature (°F)",
    fill = "Season:",
    caption = "Source: Open-Meteo Historical Weather API (ERA5 Reanalysis)"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "°F")) +
  custom_theme +
  theme(legend.position = "right")

# Save Plot 2
ggsave("boston_monthly_ranges.png", plot = p2, width = 10, height = 6, dpi = 300)
cat("Saved plot 2: boston_monthly_ranges.png\n")

cat("Visualizations complete! You can view the output PNG files in your workspace.\n")
