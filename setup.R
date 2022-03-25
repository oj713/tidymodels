library(tidymodels)

# defining data_iris
data_iris <- bind_cols(all_of(iris), 
                       row = 1:nrow(iris))

data_split <- initial_split(data_iris, prop = 3/4)


