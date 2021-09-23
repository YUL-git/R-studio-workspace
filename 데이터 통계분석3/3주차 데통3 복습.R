library(tidymodels)
library(tidytuesdayR)

# 탐색적 데이터 분석, 데이터 세트 불러오기
tuesdata <- tt_load('2020-07-28')
penguin <- tuesdata$penguins

# 데이터 세트 확인하기
head(penguin)

# 데이터 전처리
penguin_tb <- penguin%>%
  filter(!is.na(sex))%>%
  select(-year,-island)%>%
  mutate_if(is.character,as.factor)

# 데이터 전처리후 데이터 세트를 살펴보자
glimpse(penguin_tb)

# 이제 평가용 데이터 세트를 만들기 전에 sample로 뽑아주자
penguin_tb%>%
  sample_n(size=5)

# 이 샘플 데이터로 요약 통계를 해주자
penguin_tb %>%
  group_by(sex) %>%
  summarize(bill_length=mean(bill_length_mm),
            bill_depth=mean(bill_depth_mm),
            flipper_length=mean(flipper_length_mm),
            body_mass=mean(body_mass_g))

# 이제 학습용 데이터와 평가용 데이터 세트로 구분해주자
set.seed(999)
penguin_splits <- initial_split(penguin_tb,prop=0.7,strata=sex)
penguin_cv <- vfold_cv(training(penguin_splits),v=3,repeats=1)

# 이제 피처공학으로 모델의 정확도 향상을 위해 행렬 작성을 해주자
penguin_rec <- recipe(sex~.,data=training(penguin_splits))%>%
  step_corr(all_numeric(),threshold=0.9)%>%
  step_normalize(all_numeric())%>%
  step_dummy(all_nominal(),-all_outcomes())
penguin_rec

# 이제 우리가 만든 recipe객체를 학습용 데이터 세트에 넣자
penguin_juice <-penguin_rec %>%
  prep() %>%
  juice()

# 피처공학 결과 예측변수와 반응변수의 관계를 시각적으로 표현
penguin_juice %>%
  pivot_longer(-sex)%>%
  ggplot()+
  geom_histogram(aes(value,fill=sex))+
  facet_wrap(~name)
# 음~ 잘안나놔져있네

#knn 모델에 이걸 적용하자
knn_model <- nearest_neighbor()%>%
  set_engine("kknn")%>%
  set_mode("classification")
knn_model

# 이제 이 모델의 작업 흐름 관리를 넣자
knn_pg_workflow <- workflow()%>%
  add_recipe(penguin_rec)%>%
  add_model(knn_model)
knn_pg_workflow

# workflow를 교차 검증용 데이터 세트에 적합하도록 적용하자
knn_penguin_rs <- knn_pg_workflow %>%
  fit_resamples(resamples=penguin_cv,
                control=control_resamples(save_pred=TRUE),
                metrics=metric_set(recall,precision,f_meas,accuracy,
                                   kap,roc_auc))
knn_penguin_rs %>%
  collect_metrics(summarize=TRUE)

# 평가지표중 mean값이 가장큰 모델을 선정하자
best_pg_roc_auc <- select_best(knn_penguin_rs,metric="roc_auc")

# 이제 최적 모델 만들자
knn_penguin_final <- finalize_workflow(knn_pg_workflow,best_pg_roc_auc)
knn_penguin_final

# 이제 모델 평가를 하자
knn_penguin_final_fit <- knn_penguin_final %>%
  last_fit(penguin_splits)
knn_penguin_final_fit %>%
  collect_metrics

# confusion matrix로 평가 해보자
knn_penguin_final_fit %>%
  collect_predictions()%>%
  conf_mat(truth=sex,estimate=.pred_class)

# 이제 진짜 최종 모델을 만들자
penguin_final_knnmodel <- fit(knn_penguin_final,penguin_tb)

# 새로운 데이터를 tibbles형식으로 만들기
new_penguin <- tribble(~species,~bill_length_mm,~bill_depth_mm,
                       ~flipper_length_mm,~body_mass_g,
                       "Adelie",38.3,18.7,185,3790)

# 펭귄 성별 예측하기
predict(penguin_final_knnmodel,new_data=new_penguin)

# 다른 모델보다 성능이 뛰어난 모델을 사용해보자
show_model_info("nearest_neighbor")
show_engines("nearest_neighbor")

# 이 모델을 이용해서 기존에 만든 모델로 적용해보자
knn_tune_model <- nearest_neighbor(
  neighbors=tune(),
  weight_func=tune(),
  dist_power=tune())%>%
  set_engine("kknn")%>%
  set_mode("classification")

# 이 모델의 작업흐름을 만들어주자
pg_tune_workflows <- knn_pg_workflow  %>%
  update_model(knn_tune_model)
pg_tune_workflows

# tune 모델 적합화 시키자
knn_tune_rs <- pg_tune_workflows%>%
  tune_grid(
    resamples=penguin_cv,
    grid=10,
    control=control_resamples(save_pred=TRUE),
    metrics=metric_set(accuracy,kap,roc_auc)
  )

# show_best()함수를 이용해서 최적의 hyper parameter를 선정하자
knn_tune_rs%>%
  show_best("roc_auc",n=3)

# finalize_workflow()함수를 이용하여 최적으로 선정된 hyper parameter 조합을 확인하자
pg_best_tune_roc_auc <- select_best(knn_tune_rs,"roc_auc")
pg_best_tune_roc_auc

knn_tune_final <-finalize_workflow(pg_tune_workflows,pg_best_tune_roc_auc)
knn_tune_final

# 이 모델의 성능을 평가해보자
knn_tune_final_fit <- knn_tune_final %>%
  last_fit(penguin_splits)
knn_tune_final_fit %>%
  collect_metrics
# 오~ 정확도가 증가했어
# confusion matrix를 사용하자
knn_tune_final_fit %>%
  collect_predictions()%>%
  conf_mat(sex,.pred_class)

# hyper parameter 모델 dials()=tune()사용과 동일하다
knn_dials_model <- nearest_neighbor(
  neighbors =tune(),
  weight_func=tune(),
  dist_power=tune()) %>%
  set_engine("kknn")%>%
  set_mode("classification")

knn_dials_model

# 모델의 작업 흐름
pg_dials_workflows <- knn_pg_workflow %>%
  update_model(knn_dials_model)
pg_dials_workflows
