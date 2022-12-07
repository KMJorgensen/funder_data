# Clean greenseeker measurements

library(tidyverse)

# Load dataset
greenseeker <- read_csv2("Data/FUNDER_raw_greenseeker_measurements_2022.csv")

# Comments column describes weather conditions on the day measurements were done. In raw data-file it's entered on the two first rows of each new site (will be interpreted as belonging to a single plot). This information will be broken out to a separate dataset that only displays the weather conditions.

weather <- greenseeker |>
  select(Site, comment) |>
  drop_na() |>
  separate(comment, c("measurement_time", "weather"), sep = ":") |>
  separate(measurement_time, c("text", "time"), sep = " ") |>
  select(-text) |>
  pivot_wider(names_from = time , values_from = weather) |>
  rename("weather_before_measurement" = "before",
         "weather_after_measurement" = "after",
         "siteID" = "Site") |>
  mutate(siteID = recode(siteID,
                         # old name (replace) = valid name (do not change)
                         'GUD' = "Gudmedalen",
                         'LAV' = "Lavisdalen",
                         'RAM' = "Rambera",
                         'ULV' = "Ulvehaugen",
                         'SKJ' = "Skjelingahaugen",
                         'ALR' = "Alrust",
                         'ARH' = "Arhelleren",
                         'FAU' = "Fauske",
                         'HOG' = "Hogsete",
                         'OVS' = "Ovstedalen",
                         'VIK' = "Vikesland",
                         'VES' = "Veskre"))

# Export weather conditions during measurements
write_csv2(weather, file = "Clean_data/FUNDER_clean_greenseeker_measurements_weather_conditions_2022.csv")

