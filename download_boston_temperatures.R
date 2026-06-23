# R Script to download historical temperature data for Boston, MA using Open-Meteo API
# Saved as download_boston_temperatures.R

library(httr)
library(readr)
library(dplyr)

# 1. Define location parameters for Boston, Massachusetts
latitude <- 42.3601
longitude <- -71.0589
timezone <- "America/New_York"

# 2. Define date parameters (from 1940-01-01 to 2 days ago)
start_date <- "1940-01-01"
end_date <- as.character(Sys.Date() - 2)

cat(sprintf("Configuring request for Boston, MA (Lat: %s, Lon: %s)...\n", latitude, longitude))
cat(sprintf("Date range: %s to %s\n", start_date, end_date))

# 3. Construct the API URL
# Requesting maximum and minimum daily temperatures at 2m height
url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive?",
  "latitude=", latitude,
  "&longitude=", longitude,
  "&start_date=", start_date,
  "&end_date=", end_date,
  "&daily=temperature_2m_max,temperature_2m_min",
  "&timezone=", timezone,
  "&format=csv"
)

# 4. Fetch the data
cat("Sending request to Open-Meteo Archive API (no API key required)...\n")
response <- GET(url)

if (status_code(response) != 200) {
  stop(sprintf("Failed to retrieve data from Open-Meteo. HTTP Status Code: %d\nDetails: %s", 
               status_code(response), content(response, "text")))
}

cat("Data downloaded successfully! Parsing and cleaning...\n")

# 5. Read the CSV content (skipping the first 3 metadata lines)
csv_text <- content(response, "text", encoding = "UTF-8")
temp_data <- read_csv(
  I(csv_text), 
  skip = 3, 
  show_col_types = FALSE
)

# 6. Clean column names
# The columns are typically: time, temperature_2m_max (°C), temperature_2m_min (°C)
temp_data <- temp_data %>%
  rename(
    date = time,
    temp_max_c = `temperature_2m_max (°C)`,
    temp_min_c = `temperature_2m_min (°C)`
  ) %>%
  mutate(
    # Also calculate the temperature in Fahrenheit for convenience
    temp_max_f = round(temp_max_c * 9/5 + 32, 1),
    temp_min_f = round(temp_min_c * 9/5 + 32, 1),
    # Calculate daily average temperature (simple mean)
    temp_mean_c = round((temp_max_c + temp_min_c) / 2, 1),
    temp_mean_f = round((temp_max_f + temp_min_f) / 2, 1)
  )

# 7. Write to CSV file in the codespace
output_file <- "boston_temperatures.csv"
write_csv(temp_data, output_file)

cat(sprintf("Cleaned data successfully written to: '%s'\n", output_file))
cat(sprintf("Total observations: %d days\n", nrow(temp_data)))
cat("Sample of downloaded data:\n")
print(head(temp_data))
