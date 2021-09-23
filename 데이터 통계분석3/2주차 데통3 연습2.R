# 탐색적 데이터 분석
# 우리가 원하는 데이터를 미리 head()로 확인하자
head(iris)

# tibbles 객체 <- data frame의 발전된 형태
library(tibble)
library(tidymodels)
iris_tb <- as_tibble(iris)

# tibbles 객체로 받고 데이터 세트를 보는건 glimpse()로 봄
glimpse(iris_tb)

# 데이터 요약하기전에 너무 많으니까 sample()로 뽑아줄거야
iris_tb %>%
  sample_n(size=5)

# 이제 이 샘플 데이터로 데이터 요약을 해줄게(평균값?)
iris_tb %>%
  group_by(Species) %>%
  summarize(mean_Sepal.Length=mean(Sepal.Length),
            mean_Sepal.Width=mean(Sepal.Width),
            mean_Petal.Length=mean(Petal.Length),
            mean_Petal.Width=mean(Petal.Width))

# 이제 요약통계를 했으니 학습 데이터와 평가 데이터셋을 만들자
set.seed(999)
iris_split <- initial_split(iris_tb,prop=0.7,strata=Species)
iris_train <- training(iris_split)
iris_test <- testing(iris_split)

# 학습용 및 평가용 데이터 세트가 균등하게 구분되었냐? table() 함수로 구분
table(iris_train$Species)
table(iris_test$Species)

# 잘 나뉘었으니 이제 정확도를 높이기 위해서 교차 검증 세트를 준비하자
iris_cv <- vfold_cv(training(iris_split), v=3,repeats=1)

# 모델의 정확도 향상을 위해 모델 행렬을 작성하자 recipe()함수를 이용
iris_rec <- recipe(Species ~.,data=iris_train) %>%
  step_downsample(Species) %>%
  step_center(-Species)%>%
  step_scale(-Species)%>%
  step_BoxCox(-Species)%>%
  prep()
iris_rec

# recipe 객체를 학습용 데이터 세트에 적용
iris_juiced <- juice(iris_rec)
iris_juiced

# recipe 객체를 평가용 데이터 세트에 적용
baked_test <- bake(iris_rec,new_data=iris_test)

# 피처공학 결과 예측변수와 반응변수의 관계를 시각적으로 나타내자
iris_juiced %>% pivot_longer(-Species)%>%
  ggplot()+
  geom_histogram(aes(value,fill=Species))+
  facet_wrap(~name)

# 이제 데이터 세트를 피처공학으로 만들었으니 기본 모델을 만들자
knn_model <- nearest_neighbor()%>%
  set_engine("kknn")%>%
  set_mode("classification")
knn_model

# 이 모델의 작업 흐름 관리를 만들자
knn_workflow <- workflow()%>%
  add_recipe(iris_rec)%>% # 평가용 데이터 세트를 넣었어
  add_model(knn_model)

# workflow를 교차 검증용 데이터 세트를 만들자
knn_iris_rs <- knn_workflow%>%
  fit_resamples(resamples = iris_cv,
                control=control_resamples(save_pred=TRUE),
                metrics=metric_set(recall,precision,f_meas,accuracy,
                                   kap,roc_auc))
knn_iris_rs %>%
  collect_metrics(summarize=TRUE)

# 평가지표중 mean값이 가장 높은 기준으로 최적 모델을 만들자
best_roc_auc <- select_best(knn_iris_rs,metric="roc_auc")
knn_iris_final <- finalize_workflow(knn_workflow,best_roc_auc)
knn_iris_final

library(workflowsets)
library(tune)
library(yardstick)

# 모델 성능평가
knn_iris_final_fit <- knn_iris_final %>%
  last_fit(iris_split)
knn_iris_final_fit %>%
  collect(metrics())

# 이제 여기서 confusion matrix로 확인해보자
knn_iris_final_fit %>%
  collect_predictions()%>%
  conf_mat(truth=Species, estimate = .pred_class)

# 이제 진짜 최종 모델을 만들거야
final_knnmodel <- fit(knn_iris_final,iris_tb)
new_iris <- tribble(~Sepal.Length, ~Sepal.Width, ~Petal.Length,~Petal.Width,
                   4.6, 3.2, 1.3, 0.2)
predict(final_knnmodel,new_data=new_iris)

predict(final_knnmodel,new_data=new_iris)
