

# Split
tem1 <- data.table(tem1, df7) 
train <- tem1[1:nrow(train),]
test <- tem1[(nrow(train)+1) : nrow(tem),]


# ===============================================================================================================
# Train model

df_tem <- 1:nrow(train) ; oof <- 1:nrow(train)
c <- 0
a <- vector()
result_vec <- vector()
ppte <- 0
ncol(train)
i = 1
gc()
print(i)
set.seed(88)
folds <- KFold(y, nfolds = 5,  stratified = F , seed = 71)

names(folds)[i] <- "test"
test_st  <- train[folds$test,]

dtrain <- lgb.Dataset(data.matrix( train[-c(folds$test), ] ), label = y[-c(folds$test)]   )
dval <- lgb.Dataset(data.matrix( train[folds$test, ] ), label = y[c(folds$test)]          )

lgb_param <- list(boosting_type = 'gbdt',
                  objective = "huber",
                  boost_from_average = 'false',
                  metric = "none",
                  learning_rate = 0.05,
                  num_leaves = 128,
                  #  min_gain_to_split = 0.01,
                  feature_fraction = 0.05,
                  #  feature_fraction_seed = seed,
                  bagging_freq = 1,
                  bagging_fraction = 1,
                  min_sum_hessian_in_leaf = 5,
                  #  min_data_in_leaf = 100,
                  lambda_l1 = 0,
                  lambda_l2 = 0,
                  alpha = 0.3
                  
)

valids <- list(valid = dval)
lgb <- lgb.train(params = lgb_param,  data = dtrain, nrounds = 20000 , eval = "RMSE",
                 eval_freq = 50, valids = valids, early_stopping_rounds = 300, verbose = -1)


impp <- lgb.importance(lgb)
write.csv(impp, "imp.csv", row.names = F)



