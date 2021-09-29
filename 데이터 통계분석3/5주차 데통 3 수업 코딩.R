library(tidymodels)
tidymodels_prefer()
library(palmerpenguins)
install.packages("themis")
library(themis)
library(kernlab)
install.packages("recipes")
library(recipes)
# 데이터 세트 불러오기
data(package="palmerpenguins")
head(penguins)
# 데이터 전처리
penguins_tb <- penguins%>%
  filter(!is.na(sex))%>%
  select(-year,-island)
#데이터 세트 살펴보기
glimpse(penguins_tb)

# 학습용 vs 평가용 데이터 세트로 구분
library(rsample)
set.seed(201804255)
penguins_splits <- initial_split(penguins_tb,prop=0.7, strata=sex)

# 학습용 데이터 세트와 평가용 데이터 세트 확인
pg_train <- training(penguins_splits)
pg_test <- testing(penguins_splits)
dim(pg_train)
dim(pg_test)

## 기본 작업으로 모델 만들기
#다항 커널 svm모델 함수 적용
svm_poly_model <- svm_poly()%>%
  set_engine("kernlab")%>%
  set_mode("classification")
#모델 함수를 데이터 세트에 적용
svm_poly_model_fit <- svm_poly_model%>%
  fit(sex ~.,data=pg_train)
svm_poly_model_fit

#이 모델의 정확률은? predict함수 사용
svm_poly_model_fit_pred <- predict(svm_poly_model_fit,
                                   new_data=pg_test)
svm_poly_model_fit_pred

# 앞선 결과물을 원 데이터와 일치시키기 위한 작업
svm_poly_model_fit_pred_rev <- pg_test %>%
  select(sex)%>%
  bind_cols(predict(svm_poly_model_fit,
                    new_data=pg_test))
svm_poly_model_fit_pred_rev

# 정확률 측정
accuracy(svm_poly_model_fit_pred_rev,truth=sex,estimate=.pred_class)
f_meas(svm_poly_model_fit_pred_rev,truth=sex,estimate=.pred_class)

#confusion matrix
conf_mat(svm_poly_model_fit_pred_rev,
         truth=sex,estimate=.pred_class)

## 피처공학
#recipe함수
pg_train_rec <- recipe(sex~.,data=pg_train)%>%
  step_corr(all_numeric(),threshold=0.9)%>%
  step_normalize(all_numeric())%>%
  step_dummy(all_nominal(),-all_outcomes())

pg_train_rec

# 모델 아키텍쳐
svm_poly_pg_workflow <- workflow()%>%
  add_recipe(pg_train_rec)%>%
  add_model(svm_poly_model)
svm_poly_pg_workflow

# workflow를 교차 검증용 데이터 세트에 적합하도록 적용
penguins_cv <- vfold_cv(training(penguins_splits),
                        v=3,repeats=1)
library(workflows)
svm_poly_pg_rs <- svm_poly_pg_workflow %>%
  fit_resamples(resamples=penguins_cv,
                control=control_resamples(save_pred=TRUE),
                metrics=metric_set(recall,precision,f_meas,
                                   accuracy,kap,roc_auc)
                )
svm_poly_pg_rs

#평가지표 비교
svm_poly_pg_rs %>%
  collect_metrics(summarize=TRUE)
#평가지표 중 mean 값이 가장 큰 roc_auc모델을 선택
best_svm_poly_pg_roc_auc <- select_best(svm_poly_pg_rs,
                                        metric="roc_auc")
# 최적 모델 만들기
final_svm_poly_pg <- finalize_workflow(svm_poly_pg_workflow,
                                       best_svm_poly_pg_roc_auc)
final_svm_poly_pg

# 모델 평가
final_svm_poly_pg_fit <- final_svm_poly_pg %>%
  last_fit(penguins_splits)
final_svm_poly_pg_fit %>%
  collect_metrics()

#confusion matrix
final_svm_poly_pg_fit %>%
  collect_predictions() %>%
  conf_mat(truth=sex,estimate=.pred_class)

# 성별 파악하기
# 최종 모델 만들기
final_svm_poly_pgmodel <- fit(final_svm_poly_pg,
                              penguins_tb)
# 새로운 데이터를 tibbles형식으로 만들기
new_penguin <- tribble(~species, ~bill_length_mm, ~bill_depth_mm,
                       ~flipper_length_mm, ~body_mass_g,
                       "Chinstrap",45.7,18.2,195,3650)
#펭귄 성별 예측하기
predict(final_svm_poly_pgmodel, new_data=new_penguin)

#Hyper parameter찾기
show_model_info("svm_poly")
show_engines("svm_poly")

#모델 아키텍쳐
svm_poly_tune_model <- svm_poly(
  cost=tune(),
  degree=tune())%>%
  set_engine("kernlab")%>%
  set_mode("classification")

# 모델의 작업흐름
svm_poly_pg_tune_workflows <- svm_poly_pg_workflow %>%
  update_model(svm_poly_tune_model)

#tune 모델 적합(fit)
svm_poly_pg_tune_rs <-
  svm_poly_pg_tune_workflows%>%
  tune_grid(
    resamples=penguins_cv,
    grid=10,
    control=control_grid(),
    metrics=NULL)
#최적의 hyper parameter를 선정한다
svm_poly_pg_tune_rs %>%
  show_best("roc_auc",n=5)

#roc_auc 기준을 이용하여 다항 svm모델을 만든다
svm_poly_pg_tune_best_roc_auc <- select_best(
  svm_poly_pg_tune_rs, "roc_auc")
svm_poly_pg_tune_best_roc_auc

# 최적의 조합 확인
final_svm_poly_pg_tune <- finalize_workflow(svm_poly_pg_tune_workflows,
                                            svm_poly_pg_tune_best_roc_auc)

final_svm_poly_pg_tune

#모델 성능 평가
final_svm_poly_pg_tune_fit <-
  final_svm_poly_pg_tune %>%
  last_fit(penguins_splits)

final_svm_poly_pg_tune_fit %>%
  collect_metrics()

#confusion matrix사용
final_svm_poly_pg_fit %>%
  collect_predictions()%>%
  conf_mat(truth=sex,estimate=.pred_class)

           