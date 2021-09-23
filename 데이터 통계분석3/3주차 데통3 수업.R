#기본 로딩 패키지
library(tidymodels)
install.packages("tidytuesdayR")
library(tidytuesdayR)
# 데이터 세트 불러오기
tuesdata <- tt_load('2020-07-28')
penguin <- tuesdata$penguins

# 데이터 세트 확인하기
head(penguin)

# 데이터 전처리
penguin_tb <- penguin%>%
  filter(!is.na(sex))%>%
  select(-year,-island)%>%
  mutate_if(is.character,as.factor)

# 데이터 세트 살펴보기
glimpse(penguin_tb)
penguin_tb%>%
  sample_n(size=5)

# 요약 통계
penguin_tb%>%
  group_by(sex)%>%
  summarize(bill_length = mean(bill_length_mm),
            bill_depth = mean(bill_depth_mm),
            flipper_length = mean(flipper_length_mm),
            body_mass = mean(body_mass_g))

# 학습용 vs 평가용 데이터 세트 구분
set.seed(999)
penguin_splits <- initial_split(penguin_tb,prop = 0.7,strata = sex)
penguin_cv <- vfold_cv(training(penguin_splits),v =3, repeats=1)

# recipe()함수
penguin_rec <- recipe(sex ~., data=training(penguin_splits))%>%
  step_corr(all_numeric(),threshold = 0.9)%>%
  step_normalize(all_numeric())%>%
  step_dummy(all_nominal(),-all_outcomes())
penguin_rec

# juice()함수
penguin_juice <- penguin_rec %>%
  prep()%>%
  juice()

# 피처공학 결과 예측변수와 반응변수의 관계를 시각적으로 표현
penguin_juice %>%
  pivot_longer(-sex)%>%
  ggplot() +
  geom_histogram(aes(value,fill = sex))+
  facet_wrap(~name)

#모델 아키텍처
knn_model <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")
knn_model

#모델의 작업 흐름 관라: workflow
knn_pg_workflow <- workflow() %>%
  add_recipe(penguin_rec) %>%
  add_model(knn_model)
knn_pg_workflow

# workflow를 교차 검증용 데이터 세트에 적합 하도록 적용
knn_penguin_rs <- knn_pg_workflow %>%
  fit_resamples(resamples = penguin_cv,
                control = control_resamples(save_pred = TRUE),
                metrics = metric_set(recall,precision,f_meas,accuracy,
                                     kap,roc_auc)
                )
knn_penguin_rs %>%
  collect_metrics(summarize = TRUE)

# 평가지표 중 mean 값이 가장 큰 roc_auc 모델을 선택한다
best_pg_roc_auc <- select_best(knn_penguin_rs, metric = "roc_auc")

# finalize_workflow() 함수로 최적 모델만들기
knn_penguin_final <- finalize_workflow(knn_pg_workflow,best_pg_roc_auc)
knn_penguin_final

# 모델 평가
# last_fit() 함수를 사용하여 최종 예측 모델 성능 평가
knn_penguin_final_fit <- knn_penguin_final %>%
  last_fit(penguin_splits)
knn_penguin_final_fit %>%
  collect_metrics()

# confusion matrix
knn_penguin_final_fit %>%
  collect_predictions() %>%
  conf_mat(truth = sex, estimate = .pred_class)

# 새로운 데이터를 적용하여 펭귄 성별 파악하기
# 최종 모델 만들기
penguin_final_knnmodel <- fit(knn_penguin_final, penguin_tb)

# 새로운 데이터를 tibbles 형식으로 만들기
new_penguin <- tribble(~species, ~bill_length_mm, ~bill_depth_mm,
                       ~flipper_length_mm, ~body_mass_g,
                       "Adelie", 39.3, 20.6,181, 3650)

# 펭귄 성별 예측하기
predict(penguin_final_knnmodel, new_data = new_penguin)

# Hyper Parameter 모델: tune()
