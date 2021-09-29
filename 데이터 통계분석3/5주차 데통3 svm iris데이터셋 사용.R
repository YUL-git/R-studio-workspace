library(tidymodels)
tidymodels_prefer()
library(themis)
library(kernlab)
library(rsample)
head(iris)
glimpse(iris)
set.seed(201804255)
ir_splits <- initial_split(iris,prop=0.7,strata=Species)
ir_train <- training(iris_splits)
ir_test <- testing(iris_splits)

dim(ir_train)
dim(ir_test)

svm_poly_model <- svm_poly()%>%
  set_engine("kernlab")%>%
  set_mode("classification")

svm_poly_model_fit <- svm_poly_model %>%
  fit(Species ~.,data=iris)

svm_poly_model_fit_pred <- predict(svm_poly_model_fit,
                                   new_data=ir_test)
svm_poly_model_fit_pred

svm_poly_model_fit_pred_rev <- ir_test %>%
  select(Species)%>%
  bind_cols(predict(svm_poly_model_fit,
                    new_data=ir_test))
svm_poly_model_fit_pred_rev

accuracy(svm_poly_model_fit_pred_rev,truth=Species,estimate=.pred_class)
f_meas(svm_poly_model_fit_pred_rev,truth=Species,estimate=.pred_class)

conf_mat(svm_poly_model_fit_pred_rev,
         truth=Species,estimate=.pred_class)
