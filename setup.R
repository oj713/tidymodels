library(tidymodels)
library(ranger)

# defining data_iris

data_iris <- bind_cols(all_of(iris), 
                       row = 1:nrow(iris))

# RSample - generate data splits

data_split <- initial_split(data_iris, prop = 3/4)

# Recipes - define and apply preprocessing steps

iris_recipe <- recipe(Species ~ ., 
                           data = training(data_split)) |>
  update_role(row, new_role = "ID") |>
  step_corr(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_rename(Row = row) 

iris_recipe_prepped <- iris_recipe |>
  prep()

prepped_training <- juice(iris_recipe_prepped)
prepped_testing <- bake(iris_recipe_prepped, testing(data_split))

#  Parsnip - create a model and generate predictions

rf <- rand_forest() |>
  set_mode("classification") |>
  set_args(trees = 500) |>
  set_engine("ranger")

rf_fit <- rf |>
  fit(Species ~ ., data = prepped_training)

# Workflows 

iris_preds <- workflow() |>
  add_recipe(iris_recipe) |>
  add_model(rf) |>
  fit(training(data_split)) |>
  augment(testing(data_split))




