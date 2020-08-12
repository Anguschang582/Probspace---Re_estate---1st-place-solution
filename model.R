


# Select top 700
train <- train[,impp$Feature[1:700], with=F ]
test <- test[,impp$Feature[1:700], with=F ]

# ===============================================================================================================
# Train model
df_tem <- 1:nrow(train) ; oof <- 1:nrow(train)
c <- 0
a <- vector()
result_vec <- vector()
ppte <- 0
ncol(train)

for(i in 1:5){
  #i = 1
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
                    learning_rate = 0.008,
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
  
  #a <- as.numeric(Sys.time())
  lgb <- lgb.train(params = lgb_param,  data = dtrain, nrounds = 50000 , eval = "RMSE",
                   eval_freq = 50, valids = valids, early_stopping_rounds = 500, verbose = -1)
  #cat("Train time :" , ( as.numeric(Sys.time()) - a ) / 60, "min", "\n" )
  
  pp <- predict(lgb, data.matrix(test_st))
  ppte1 <- predict(lgb, data.matrix(test))
  ppte <- ppte + ppte1
  
  df_tem[as.numeric(folds$test)] <- as.numeric(unlist(pp))
  a[i] <- RMSLE( expm1(df_tem[as.numeric(folds$test)]) , expm1(y[as.numeric(folds$test)])  )
  cat("best iter :" , lgb$best_iter, "best score :", a[i] ,"\n" )
  invisible(gc())
}

# CV score
mean(a)

# Submission
ppte <- expm1(ppte/5) ; ppte[ppte<0] <- 1e-6
sub <- data.frame(id = test_id, y = ppte)
write.csv(sub,"this_should_work.csv",row.names = F)





