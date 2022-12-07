# Clean slope and aspect data

slope_aspect_raw <- read_csv2("Data/FUNDER_raw_slope_and_aspect_2022.csv")

### Clean dataset ####
# Rename columns
# Remove non-existing plots (Fau2GB and Ulv1B)
# Recode siteID:s

slope_aspect <- slope_aspect_raw |>
  rename(siteID = site) |>
  select(siteID, blockID, plotID, treatment, slope, aspect, comment) |>
  filter(plotID != "Fau2GB") |>
  filter(plotID != "Ulv1B") |>
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

# Export cleaned dataset
write.csv2(slope_aspect, file = "Clean_data/FUNDER_clean_slope_and_aspect_2022.csv", row.names = FALSE)
