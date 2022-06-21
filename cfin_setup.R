library(tidymodels)
library(ranger)

cfin <- readr::read_csv("ecomon_data.csv.gz", col_types = readr::cols()) |>
  na.omit() |>
  mutate(month = lubridate::month(date) |> as.factor(),
         year = lubridate::year(date),
         abundance = log10(calfin_10m2 + 1)) |>
  select(lat, lon, year, month, abundance, Bathy_depth, sfc_temp:btm_salt)

set.seed(400)
cfin_split <- initial_split(cfin, prop = 3/4, strata = abundance)
cfin_train <- training(cfin_split)

# creating a resampling object
cfin_folds <- cfin_train |>
  vfold_cv(v = 5, repeats = 1, strata = abundance)

cfin_control <- control_resamples(save_pred = TRUE, save_workflow = TRUE)