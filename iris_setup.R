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

prepped_training <- bake(iris_recipe_prepped, NULL)
prepped_testing <- bake(iris_recipe_prepped, testing(data_split))

#  Parsnip - create a model and generate predictions

rf <- rand_forest() |>
  set_mode("classification") |>
  set_args(trees = 200) |>
  set_engine("randomForest")

rf_fit <- rf |>
  fit(formula(iris_recipe_prepped), data = prepped_training)

iris_preds <- workflow() |>
  add_recipe(iris_recipe) |>
  add_model(rf) |>
  fit(training(data_split)) |>
  augment(testing(data_split))

