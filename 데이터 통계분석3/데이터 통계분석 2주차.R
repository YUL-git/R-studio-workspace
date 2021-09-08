iris
head(iris)
install.packages("tidymodels")
Yes
library(tidymodels)
iris_tb <- as_tibble(iris)
glimpse(iris_tb)
iris_tb %>%
  sample_n(size = 5)
iris_tb %>%
  group_by(Species) %>%
  summarize(mean_Sepal.Length = mean(Sepal.Length),
            mean_Sepal.Width = mean(Sepal.Width),
            mean_Petal.Length = mean(Petal.Length),
            mean_Petal.With = mean(Petal.Width))
set.seed(999)
iris_split <- initial_split(iris_tb, prop = 0.7, sarata = Species)
iris_train <- training(iris_split)
iris_test <- testing(iris_split)
table(iris_train$Species)
table(iris_test$Species)
iris_cv <- vfold_cv(training(iris_split), v=3,repeats=1)
