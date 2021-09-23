library(tidymodels)
tidymodels_prefer()
library(tidytuesdayR)
install.packages("themis")
library(themis)
install.packages("kernlab")
library(kernlab)
install.packages("palmerpenguins")
library(palmerpenguins)

#탐색적 데이터 분석
data(package="palmerpenguins")
head(penguins)

#데이터 전처리를 해주자(나에게 필요 없는데이터 삭제)
penguins_tb <- penguins%>%
  filter(!is.na(sex))%>%
  select(-year,-island)

#데이터 세트 살펴보기
glimpse(penguins_tb)

#요약통계를 해주자
penguins_tb%>%
  group_by(sex)%>%
  summarize(bill_length=mean(bill_length_mm),
            bill_depth=mean(bill_depth_mm),
            flipper_length=mean(flipper_length_mm),
            body_mass=mean(body_mass_g))

install.packages("resample")
library(resample)
#학습용, 평가용 데이터 세트 구분하자
set.seed(888)
penguins_splits <- initial_split(penguins_tb, prop=0.7, strata=sex)
#교차검증용 데이터 세트
penguins_cv <- vfold_cv(training(penguins_splits),v=3,repeats=1)

#피처공학 recipe함수를 이용하자
penguins_rec <- recipe(sex ~.,data=training(penguins_splits))%>%
  step_corr(all_numeric(),threshold=0.9)%>%
  step_normalize(all_numeric())%>%
  step_dummy(all_nominal(),-all_outcomes())
penguins_rec

install.packages("recipes")
library(recipes)
#이제 우리가 새로배운 모델인 radial svm모델을 사용해보자
#모델 아키텍처 svm_rbf()함수를 이용할거야
svm_rbf_model <- svm_rbf()%>%
  set_engine("kernlab")%>%
  set_mode("classification")
svm_rbf_model

install.packages('workflows')
library(workflows)
#모델의 작업 흐름 관리 workflow
svm_rbf_pg_workflow <- workflow()%>%
  add_recipe(penguins_rec)%>%
  add_model(svm_rbf_model)
svm_rbf_pg_workflow

install.packages("fit.models")
library(fit.models)
install.packages("FIT")
library(FIT)

#workflow 교차 검증용 데이터세트에 적합하도록 적용
svm_rbf_pg_rs <- svm_rbf_pg_workflow %>%
  fit_resamples(resamples=penguins_cv,
                control=control_resamples(save_pred=TRUE),
                metrics=metric_set(recall,precision,f_meas,accuracy,kap,roc_auc)
                )
svm_rbf_pg_rs

install.packages("Metrics")
library(Metrics)

#평가지표비교
svm_rbf_pg_rs %>%
  collect_metrics(summarize=TRUE)

#평가지표중 가장 높은 것을 채택하자
best_svm_rbf_pg_roc_auc <- select_best(svm_rbf_pg_rs,metric="roc_auc")

#finalize_workflow()함수로 최적 모델만들기
final_svm_rbf_pg <- finalize_workflow(svm_rbf_pg_workflow,best_svm_rbf_pg_roc_auc)
final_svm_rbf_pg

install.packages("tune")
library(tune)
install.packages("workflowsets")
library(workflowsets)
#모델 평가(이 모델이 얼마나 높은 정확률을 보여줄것이냐!)
#학습용 데이터 세트에서 만든 모델을 평가용 데이터 세트에 적용
final_svm_rbf_pg_fit <- final_svm_rbf_pg %>%
  last_fit(penguins_splits)

final_svm_rbf_pg_fit %>%
  collect_metrics()

#confusion matrix
final_svm_rbf_pg_fit%>%
  collect_predictions()%>%
  conf_mat(truth=sex,estimate=.pred_class)

#새로운 데이터를 적용하여 펭귄 성별 파악하기
#최종 final 모델
final_svm_rbf_pgmodel <- fit(final_svm_rbf_pg, penguins_tb)

#새로운 데이터를 tibbles형식으로 만들기
new_penguin <- tribble(~species, ~bill_length_mm,~bill_depth_mm,
                       ~flipper_length_mm,~body_mass_g,
                       "Adelie",38.3,18.7,185,3790)
#펭귄 성별 예측하기
predict(final_svm_rbf_pgmodel, new_data=new_penguin)

#여기까지가 기본내용>??!?!!>!>!!?!?

#Hyper Parameter 모델:tune()
#Hyper Parameter 찾기
show_model_info("svm_rbf")
show_engines("svm_rbf")

#모델 아키텍처
svm_rbf_tune_model <- svm_rbf(
  cost=tune(),
  rbf_sigma=tune())%>%
  set_engine("kernlab")%>%
  set_mode("classification")

#모델 작업 흐름
svm_rbf_pg_tune_workflows <- svm_rbf_pg_workflow%>%
  update_model(svm_rbf_tune_model)
svm_rbf_pg_tune_workflows

#tune 모델 적합
svm_rbf_pg_tune_rs <- svm_rbf_pg_tune_workflows%>%
  tune_grid(
    resamples=penguins_cv,
    grid=10,
    control=control_grid(),
    metrics=NULL)

#show_best()함수를 이용하여 최적의 파라미터를 선정한다
svm_rbf_pg_tune_rs %>%
  show_best("roc_auc",n=3)

#roc_auc 기준을 이용하여 radial svm모델을 만든다
svm_rbf_pg_tune_best_roc_auc <- select_best(svm_rbf_pg_tune_rs,"roc_auc")
svm_rbf_pg_tune_best_roc_auc

#finalize_workflow()함수를 이용하여 최적으로 선정된 조합을 확인 할 수 있다
final_svm_rbf_pg_tune <- finalize_workflow(svm_rbf_pg_tune_workflows,
                                           svm_rbf_pg_tune_best_roc_auc)
final_svm_rbf_pg_tune
#Hyper Parameter 모델 성능 평가,last_fit()사용
final_svm_rbf_pg_tune_fit <- final_svm_rbf_pg_tune %>%
  last_fit(penguins_splits)

final_svm_rbf_pg_tune_fit %>%
  collect_metrics()

#confusion matrix 사용
final_svm_rbf_pg_tune_fit%>%
  collect_predictions()%>%
  conf_mat(truth=sex,estimate=.pred_class)

#Hyper Parameter모델: dials()
#tune(사용과 동일하다)

