# Clean soil depth measurements

library(tidyverse)
library(stringr)

soil_depth_raw <- read_csv2("Data/FUNDER_raw_soil_depth_measurements_2022.csv")

soil_depth <- soil_depth_raw |>
  select(-block) |>
  rename("1" = depth1, "2" = depth2, "3" = depth3) |>
  relocate(treatment, .after = plotID) |>
  pivot_longer(!c(siteID, treatment, plotID), names_to = "measurement", values_to = "depth_cm")


# Replace "," with "." as decimal deliminator
soil_depth$depth_cm <- str_replace_all(soil_depth$depth_cm, ",", ".")

# Force soil depth to be read as a numeric variable
soil_depth$depth_cm <- as.numeric(soil_depth$depth_cm)

