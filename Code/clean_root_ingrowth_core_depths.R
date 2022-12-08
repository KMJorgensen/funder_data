# Cleaning RIC depths

library(tidyverse)

RIC <- read_csv2("Data/FUNDER_raw_root_ingrowth_core_depths_2022.csv")

RIC_clean <- RIC |>
  select(4:6) |>
  separate(plotID, c("siteID", "blockID", "treatment"), sep = "_")

RIC_clean$siteID <- str_to_title(RIC_clean$siteID)

RIC_clean <- RIC_clean |>
  mutate(blockID = paste(siteID, blockID, sep = "")) |>
  mutate(plotID = paste(blockID, treatment, sep = "")) |>
  relocate(plotID, .after = blockID)

RIC_clean$RIC_cm <- gsub(",", ".", RIC_clean$RIC_cm)
RIC_clean$RIC_cm <- as.numeric(RIC_clean$RIC_cm)
