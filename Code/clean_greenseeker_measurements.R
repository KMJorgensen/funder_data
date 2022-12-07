# Clean greenseeker measurements

library(tidyverse)

# Load dataset
greenseeker <- read_csv2("Data/FUNDER_raw_greenseeker_measurements_2022.csv")

# Comments column describes weather conditions on the day measurements were done. In raw data-file it's entered on the two first rows of each new site (will be interpreted as belonging to a single plot). This information will be broken out to a separate dataset that only displays the weather conditions.

# Clean up names according to FUNDER naming convention
green <- greenseeker |>
  select(-comment, -block) |>
  separate(ID, c("site", "blockID", "treat")) |>
  select(-site, -treat)

green$Site <- str_to_title(green$Site)

# Fix plotIDs
green <- green |>
  mutate(blockID = paste(Site, blockID, sep = "")) |>
  mutate(plotID = paste(blockID, treatment, sep = ""))

# Fix siteIDs
green <- green |>
  rename("siteID" = "Site") |>
  mutate(siteID = recode(siteID,
                         # old name (replace) = valid name (do not change)
                         'Gud' = "Gudmedalen",
                         'Lav' = "Lavisdalen",
                         'Ram' = "Rambera",
                         'Ulv' = "Ulvehaugen",
                         'Skj' = "Skjelingahaugen",
                         'Alr' = "Alrust",
                         'Arh' = "Arhelleren",
                         'Fau' = "Fauske",
                         'Hog' = "Hogsete",
                         'Ovs' = "Ovstedalen",
                         'Vik' = "Vikesland",
                         'Ves' = "Veskre"))

# Pivot to long format, first by measurement time

# Change order of columns
green <- green |>
  select(date, siteID, blockID, plotID, treatment, time_1, time_2, "1_before", "2_before", "1_after", "2_after")


green_first <- green |>
  select(!c(time_2, "1_after", "2_after")) |>
  pivot_longer(!c(date, siteID, blockID, plotID, treatment, time_1), names_to = "rep", values_to = "record") |>
  mutate(rep = recode(rep,
    "1_before" = "1",
    "2_before" = "2")) |>
  mutate(measurement = "first") |>
  rename("time" = "time_1")

green_second <- green |>
  select(!c(time_1, "1_before", "2_before")) |>
  pivot_longer(!c(date, siteID, blockID, plotID, treatment, time_2), names_to = "rep", values_to = "record") |>
  mutate(rep = recode(rep,
                      "1_after" = "1",
                      "2_after" = "2")) |>
  mutate(measurement = "second")|>
  rename("time" = "time_2")

green.long <- rbind(green_first, green_second)




##################################################################

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

