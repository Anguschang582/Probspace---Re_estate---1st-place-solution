



# sq_meter, land__mean__ON__h31_price, nobeyuka_m2, built_year

lgb_pred_have_na <- function(data, target, target_name){
  
  cat("input col :", ncol(data),"\n")
  data <- as.data.frame(data)
  non_na_idx <- which(is.na( as.numeric(unlist(data[,target_name])) ) == F  )
  na_idx <- which(is.na( as.numeric(unlist(data[,target_name])) ) == T  )  
  
  if(target_name == "land__mean__ON__h31_price"){ data <- data[,which(grepl("_price",colnames(data)) == F )] }
  cleaned_target <- target[is.na(target) == F]
  cleaned_data <- data[,which(grepl(target_name,colnames(data)) == F )]   
  cat("cleaned col :", ncol(cleaned_data),"\n")
  
  non_na_data <- cleaned_data[non_na_idx,]
  na_data <- cleaned_data[na_idx,]
  
  df_tem <- 1:nrow(non_na_data)
  c <- 0
  a <- vector()
  ppte <- 0
  
  for(i in 1:5){
    invisible(gc())
    print(i)
    set.seed(77)
    folds <- KFold(cleaned_target, nfolds = 5,  stratified = FALSE, seed = 0)
    
    names(folds)[i] <- "test"
    test_st  <- non_na_data[folds$test,]
    
    d0 <- lgb.Dataset(data.matrix( non_na_data[-c(folds$test),] ), label = cleaned_target[-c(folds$test)] )
    dval <- lgb.Dataset(data.matrix( non_na_data[folds$test,] ), label = cleaned_target[c(folds$test)] )
    
    lgb_param <- list(boosting_type = 'gbdt',
                      objective = "regression" ,
                      metric = "RMSE",
                      learning_rate = 0.1,
                      num_leaves = 96,
                      feature_fraction = 0.05,
                      bagging_freq = 1,
                      bagging_fraction = 1,
                      min_data_in_leaf = 200
    )
    
    valids <- list(valid = dval)
    lgb <- lgb.train(params = lgb_param,  data = d0, nrounds = 2000, 
                     eval_freq = 200, valids = valids, early_stopping_rounds = 300, verbose = 1)
    
    pp <- predict(lgb, data.matrix(test_st))
    ppte1 <- predict(lgb, data.matrix(na_data))
    ppte <- ppte + ppte1
    
    df_tem[as.numeric(folds$test)] <- as.numeric(unlist(pp))
    cat( ">>>>  END  <<<< ","\n")
  }
  ppte <- ppte/5
  return(list(df_tem,ppte))
}


lgb_pred_no_na <- function(data, target, target_name){
  
  data <- as.data.frame(data)
  cat("input col :", ncol(data),"\n")
  cleaned_data <- data[,which(grepl(target_name,colnames(data)) == F )]  
  if(target_name == "land__mean__ON__h31_price"){ data <- data[,which(grepl("square_meter",colnames(data)) == F )] }
  cat("cleaned col :", ncol(cleaned_data),"\n")
  
  df_tem <- 1:nrow(cleaned_data)
  c <- 0
  a <- vector()
  ppte <- 0
  target <- log1p(target)
  
  for(i in 1:5){
    invisible(gc())
    print(i)
    set.seed(77)
    folds <- KFold(target, nfolds = 5,  stratified = FALSE, seed = 0)
    
    names(folds)[i] <- "test"
    test_st  <- cleaned_data[folds$test,]
    
    d0 <- lgb.Dataset(data.matrix( cleaned_data[-c(folds$test),] ), label = target[-c(folds$test)] )
    dval <- lgb.Dataset(data.matrix( cleaned_data[folds$test,] ), label = target[c(folds$test)] )
    
    lgb_param <- list(boosting_type = 'gbdt',
                      objective = "regression" ,
                      metric = "RMSE",
                      learning_rate = 0.1,
                      num_leaves = 96,
                      feature_fraction = 0.05,
                      bagging_freq = 1,
                      bagging_fraction = 1,
                      min_data_in_leaf = 200
    )
    
    valids <- list(valid = dval)
    lgb <- lgb.train(params = lgb_param,  data = d0, nrounds = 1000, 
                     eval_freq = 200, valids = valids, early_stopping_rounds = 300, verbose = 1)
    
    pp <- predict(lgb, data.matrix(test_st))
    
    df_tem[as.numeric(folds$test)] <- as.numeric(unlist(pp))
    cat( ">>>>  END  <<<< ","\n")
  }
  return(df_tem)
}


get_pred0 <- function(data){
  
  data <- as.data.frame(data)
  c <- 0 
  tmp2 <- 1:nrow(data)
  pred_data <- data.frame(0)
  col <- data[,c("land__mean__ON__h31_price", "nobeyuka_m2", "built_year")]
  for(target_name in colnames(col)){
    
    library(lightgbm)  
    c <- c+1
    tmp1 <- lgb_pred_have_na(data, as.numeric(unlist(col[,c])), target_name) 
    tmp2[ which(is.na(  as.numeric(unlist(col[,target_name]))  ) == F) ] <-  tmp1[[1]]
    tmp2[ which(is.na(  as.numeric(unlist(col[,target_name]))  ) == T) ] <-  tmp1[[2]]
    pred_data <- data.frame(pred_data,tmp2)
    lgb.unloader(wipe = T)
  }
  pred_data <- pred_data[,-1] ; colnames(pred_data) <- paste0(colnames(col),"___pred0_by_lgbm",sep="")
  return(pred_data)
}


df7 <- get_pred0(tem1)
df8 <- data.table(expm1(lgb_pred_no_na(tem1, tem1$sq_meter, "sq_meter")), 
                  expm1(lgb_pred_no_na(tem1, tem1$time_to_nearest_aki, "time_to_nearest_aki"))
)

colnames(df8) <- paste0(c("sq_meter", "time_to_nearest_aki"),"___pred0_by_lgbm",sep="")

df7 <- data.table(df7, df8)
write.csv(df7, "df7.csv", row.names=F)



