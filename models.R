library(tidymodels)
library(readr)
library(readxl)

library(psych)

library(mlbench)
library(caret)
library(caretEnsemble)
library(rpart)

library(lmtest)
library(xgboost)
library(glmnet)
library(randomForest)


# EDA

df = read_xlsx("~data_final.xlsx", col_names = T)

df$shirota = df$shirota %>% as.numeric()
df$dolgota = df$dolgota %>% as.numeric()
df$shirota = df$shirota %>% as.numeric()
df$floor = df$floor %>% as.numeric()
df$if_balcony = df$if_balcony %>% as.numeric()
df$if_lift = df$if_lift %>% as.numeric()
df$type_toilet = df$type_toilet %>% as.numeric()


df$rooms = df$rooms %>% as.factor()
df$district = df$district %>% as.factor()
df$housetype = df$housetype %>% as.factor()
df$remont = df$remont %>% as.factor()
df$subway_TRUE = df$subway_TRUE %>% as.factor()

options(scipen=100)


df2 = df %>% select(-c(id, addr, shirota, dolgota, opisanie, district, floor, subway_TRUE))


tab = psych::describe(select(df, -c(id, addr, shirota, dolgota, opisanie, district, floor, subway_TRUE, rooms, type_toilet,if_lift, if_balcony, housetype, remont)), IQR = T, skew = F, quant=c(.25,.75)) %>% as.data.frame()
tab = tab %>% select(-c(n, vars, se))


litter_price = 1.5*tab[["IQR"]][1] + tab[["Q0.75"]][1]
litter_dist = 1.5*tab[["IQR"]][7] + tab[["Q0.75"]][7]

df = df %>% filter(price<=litter_price) %>% filter(subway_dist_peshkom_TRUE<=60)
df = df %>% select(-c(id, opisanie, addr, shirota, dolgota))


set.seed(523)
split = initial_split(df, prop = 0.8)
train_df = training(split)
test_df = testing(split)

fmla = as.formula("price ~ square+rooms+district+kitchen_square+living_square+floor+year+toilet_quantity+type_toilet+if_lift+if_balcony+housetype+remont+subway_TRUE+subway_dist_peshkom_TRUE")


# model 1

model1 = decision_tree(mode = 'regression') %>% 
  set_engine("rpart")

model1_wf = workflow() %>%
  add_formula(fmla) %>%
  add_model(model1) %>%
  fit(train_df)


set.seed(523)
folds = rsample::vfold_cv(train_df, v = 10, repeats = 5)

train_tune = tune_grid(model1_wf, resamples = folds)


train_best = train_tune %>% select_best("rmse")
final_model1 = finalize_model(model1, train_best)

model1_wf = workflow() %>%
  add_formula(fmla) %>%
  add_model(final_model1) %>%
  fit(train_df)

save(model1_wf, file = "model1.Rda")


# model 2

fmla2 = as.formula("price ~ square+rooms+kitchen_square+living_square+year+toilet_quantity+type_toilet+if_lift+if_balcony")

model2 = decision_tree(mode = 'regression') %>% 
  set_engine("rpart")

model2_wf = workflow() %>%
  add_formula(fmla2) %>%
  add_model(model2) %>%
  fit(train_df)


set.seed(523)
folds = rsample::vfold_cv(train_df, v = 10, repeats = 5)

train_tune2 = tune_grid(model2_wf, resamples = folds)


train_best2 = train_tune2 %>% select_best("rmse")
final_model2 = finalize_model(model2, train_best2)

model2_wf = workflow() %>%
  add_formula(fmla10) %>%
  add_model(final_model2) %>%
  fit(train_df)

save(model2_wf, file = "model2.Rda")


# model 3

train_df_matr = data.matrix(train_df[,2:16])

set.seed(523)
ridge_model = cv.glmnet(train_df_matr, train_df[[1]], alpha = 0, nfolds = 20, relax = T)

best_lambda = ridge_model$lambda.min # find optimal lambda which minimizes MSE
ridge_model = glmnet(train_df_matr, train_df[[1]],
                     alpha = 0, lambda = best_lambda)

save(ridge_model, file = "model3.Rda")
save(best_lambda2, file = "lamb1.Rda")


# model 4

set.seed(523)
lasso_model = cv.glmnet(train_df_matr, train_df[[1]],
                        alpha = 1, nfolds = 20, relax = T)

best_lambda2 = lasso_model$lambda.min # find optimal lambda which minimizes MSE
lasso_model = glmnet(train_df_matr, train_df[[1]],
                     alpha = 1, lambda = best_lambda2)

save(lasso_model, file = "model4.Rda")
save(best_lambda2, file = "lamb2.Rda")


# model 5

fmla2 = as.formula("price ~ square+rooms+kitchen_square+living_square+year+toilet_quantity+type_toilet+if_lift+if_balcony")

set.seed(523)
rf = randomForest(formula = fmla2, data = train_df,
                  mtry = 5, ntrees = 500, importance = T)

save(rf, file = "model5.Rda")
