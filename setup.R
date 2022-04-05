library(tidymodels)
library(ranger)

# defining data_iris

data_iris <- bind_cols(all_of(iris), 
                       row = 1:nrow(iris))

# RSample - generate data splits

data_split <- initial_split(data_iris, prop = 3/4)

# Recipes - define and apply preprocessing steps

iris_recipe_prepped <- recipe(Species ~ ., 
                           data = training(data_split)) |>
  update_role(row, new_role = "ID") |>
  step_corr(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_rename(Row = row) |>
  prep()

prepped_training <- juice(iris_recipe_prepped)
prepped_testing <- bake(iris_recipe_prepped, testing(data_split))

#  Parsnip - create a model and generate predictions

rf_fit <- rand_forest() |>
  set_mode("classification") |>
  set_args(trees = 500) |>
  set_engine("ranger") |>
  fit(Species ~ ., data = prepped_training)

predictions <- augment(rf_fit, prepped_testing)



