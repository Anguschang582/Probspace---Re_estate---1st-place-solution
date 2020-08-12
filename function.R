

# Some function
beyond1std_ratio <- function(x){ return( sum(ifelse(x > (mean(x,na.rm=T) + sd(x,na.rm=T)),1,0)) / length(x) )}
iqr_ratio <- function(x){ return( quantile(x,0.75,na.rm=T) / quantile(x,0.25,na.rm=T) )}
mean_var <- function(x){ return( sd(x,na.rm=T) /  mean(x,na.rm=T) )}
range_diff <- function(x){ return( max(x,na.rm=T) - min(x,na.rm=T) )}
range_per <- function(x){ return( max(x,na.rm=T) / min(x,na.rm=T) )}
hl_ratio <- function(x){ return( sum(ifelse(x > mean(x),1,0)) / sum(ifelse(x >= mean(x),0,1)) )}
sw_stat <- function(x){ return( ifelse(sum(is.na(x) == F) > 3 & sum(is.na(x) == F) < 5000 &
                                         length(x) > 3 & length(x) < 5000 & sum(diff(x),na.rm=T)!=0 , 
                                       shapiro.test(as.numeric(x))$statistic, NA) )}
x_diff <- function(x){ return(x - mean(x,na.rm=T)) }
x_ratio <- function(x){ return(x / mean(x,na.rm=T)) }
x_zscore <- function(x){ return( (x-mean(x,na.rm=T)) / sd(x,na.rm=T)) }

freq1ratio <- function(x){ return( 
  ifelse(sort(table(x),decreasing = T)[1] == sort(table(x),decreasing = T)[2],
         NA, as.numeric( sort(table(x),decreasing = T)[1] / length(x) )  )  )}
freq1count <- function(x){ return( 
  ifelse(sort(table(x),decreasing = T)[1] == sort(table(x),decreasing = T)[2],
         NA, ( sort(table(x),decreasing = T)[1] )  )  )}

entropy_freqtable <- function(x){ return( as.numeric(entropy(table(x)))) }


# Revised version of function "zen2han" from "Nippon" package
z2h <- function (s) 
{
  s <- ifelse(s == "", NA, s)  
  ori_s <- s
  s <- na.omit(s)
  if (any(Encoding(s) != "UTF-8")) 
    s <- iconv(s, from = "", to = "UTF-8")
  s <- paste(s, sep = "")
  y <- sapply(unlist(strsplit(s, split = ",")), function(x) {
    i <- utf8ToInt(x)
    if (i >= 65281 && i <= 65374) {
      return(intToUtf8(i - 65248))
    }
    else {
      return(x)
    }
  })
  ori_s[!is.na(ori_s)] <- paste(y)
  return(ori_s)
}

# Get group function
# can acclerate whole process by using %dopar%
get_group_feature_numer2cate <- function(data, fun_list, numer_list, cate_list){
  
  tmp1 <- data.table(row_idx = 1:nrow(data))
  
  for(fun_name in fun_list){
    cat("- Processing : ", fun_name, "\n")
    for(cate in cate_list){
      for(numer in numer_list){
        
        if(cate == numer) next
        data2 <- data[,c(cate,numer), with=F]
        
        if(fun_name %in% c("x_diff", "x_ratio", "x_zscore")){
          tmp <- data2[ , col := get(fun_name)( get(numer) ), by = cate]
        } else{ tmp <- data2[ , col := get(fun_name)( na.omit(get(numer)) ), by = cate] }
        
        colnames(tmp)[3] <- paste0(fun_name, "__ON__", numer, "__BY__", cate)
        tmp1 <- data.table(tmp1,tmp[,-c(1:2)])
      } 
    }
  }
  tmp1 <- tmp1[, lapply(.SD, function(x){ ifelse(is.infinite(x) | is.nan(x), NA, x) } )]
  cat("====  Process completed, ignore the warning message from max and min function (returning Inf)  ====", "\n")
  return(tmp1[,-1])
}  

# Get group function
# can acclerate whole process by using %dopar%
get_group_feature_cate2cate <- function(data, fun_list, cate_list){
  
  tmp1 <- data.table(row_idx = 1:nrow(data))
  data2 <- data[,c(cate_list), with=F]
  
  
  for(fun_name in fun_list){
    cat("- Processing : ", fun_name, "\n")
    for(cate1 in cate_list){
      for(cate2 in cate_list){
        
        if(cate1 == cate2) next
        if(cate1 == "sub_area" & cate2 == "area_code") next
        tmp <- data2[, col := NULL][ , col := get(fun_name)( get(cate2) ), by = cate1][ , "col", with=F]
        colnames(tmp) <- paste0(fun_name, "__ON__", cate2, "__BY__", cate1)
        tmp1 <- data.table(tmp1,tmp)
      } 
    }
  }
  cat("====  Process completed  ====", "\n")
  return(tmp1[,-1])
}  


get_bayes_mean <- function(data, numer_list, cate_list, prior=20){
  
  tmp1 <- data.table(row_idx = 1:nrow(data))
  
  for(cate in cate_list){
    for(numer in numer_list){
      
      data2 <- data[,c(cate,numer), with=F]
      tmp <- data2[ , col :=  ( ( ( get(numer) / mean(na.omit(get(numer))) ) * .N) + prior ) / (.N + prior)  , by = cate][is.na(col), col:= 1]
      colnames(tmp)[3] <- paste0("bayes_mean", "__ON__", numer, "__BY__", cate)
      tmp1 <- data.table(tmp1,tmp[,-c(1:2)])
    } 
  }
  
  tmp1 <- tmp1[, lapply(.SD, function(x){ ifelse(is.infinite(x) | is.nan(x) , NA, x) } )]
  cat("====   Process completed   ====", "\n")
  return(tmp1[,-1])
}  


get_count_encoding <- function(data, cate_list){
  tmp1 <- data.table(row_idx = 1:nrow(data))
  for(cate in cate_list){
    tmp <- data[ , col := .N, by=cate ][ , "col", with=F] 
    colnames(tmp) <- paste0("count_encoding__OF__", cate)
    tmp1 <- data.table(tmp1, tmp)
  }
  cat("====  Process completed  ====", "\n")
  return(tmp1[,-1])
}



# Author of the code : Branden Murray
# https://www.kaggle.com/brandenkmurray/it-is-lit
# This is a modified version for regression task
catNWayAvgCV <- function(data, varList, y, pred0, filter, k, f, g=1, lambda=NULL, r_k, cv=NULL){
  
  # It is probably best to sort your dataset first by filter and then by ID (or index)
  n <- length(varList)
  varNames <- paste0("v",seq(n))
  ind <- unlist(cv, use.names=FALSE)
  oof <- NULL
  if (length(cv) > 0){
    for (i in 1:length(cv)){
      sub1 <- data.table(v1=data[,varList,with=FALSE], y=data[,y,with=FALSE], pred0=data[,pred0,with=FALSE], filt=filter)
      sub1 <- sub1[sub1$filt==TRUE,]
      sub1[,filt:=NULL]
      colnames(sub1) <- c(varNames,"y","pred0")
      sub2 <- sub1[cv[[i]],]
      sub1 <- sub1[-cv[[i]],]
      sum1 <- sub1[,list(sumy=sum(y), avgY=mean(y), cnt=length(y)), by=varNames]
      tmp1 <- merge(sub2, sum1, by = varNames, all.x=TRUE, sort=FALSE)
      set(tmp1, i=which(is.na(tmp1[,cnt])), j="cnt", value=0)
      set(tmp1, i=which(is.na(tmp1[,sumy])), j="sumy", value=0)
      if(!is.null(lambda)) tmp1[beta:=lambda] else tmp1[,beta:= 1/(g+exp((tmp1[,cnt] - k)/f))]
      tmp1[,adj_avg:=((1-beta)*avgY+beta*pred0)]
      set(tmp1, i=which(is.na(tmp1[["avgY"]])), j="avgY", value=tmp1[is.na(tmp1[["avgY"]]), pred0])
      set(tmp1, i=which(is.na(tmp1[["adj_avg"]])), j="adj_avg", value=tmp1[is.na(tmp1[["adj_avg"]]), pred0])
      set(tmp1, i=NULL, j="adj_avg", value=tmp1$adj_avg*(1+(runif(nrow(sub2))-0.5)*r_k))
      oof <- c(oof, tmp1$adj_avg)
    }
  }
  oofInd <- data.frame(ind, oof)
  oofInd <- oofInd[order(oofInd$ind),]
  sub1 <- data.table(v1=data[,varList,with=FALSE], y=data[,y,with=FALSE], pred0=data[,pred0,with=FALSE], filt=filter)
  colnames(sub1) <- c(varNames,"y","pred0","filt")
  sub2 <- sub1[sub1$filt==F,]
  sub1 <- sub1[sub1$filt==T,]
  sum1 <- sub1[,list(sumy=sum(y), avgY=mean(y), cnt=length(y)), by=varNames]
  tmp1 <- merge(sub2, sum1, by = varNames, all.x=TRUE, sort=FALSE)
  tmp1$cnt[is.na(tmp1$cnt)] <- 0
  tmp1$sumy[is.na(tmp1$sumy)] <- 0
  if(!is.null(lambda)) tmp1$beta <- lambda else tmp1$beta <- 1/(g+exp((tmp1$cnt - k)/f))
  tmp1$adj_avg <- (1-tmp1$beta)*tmp1$avgY + tmp1$beta*tmp1$pred0
  tmp1$avgY[is.na(tmp1$avgY)] <- tmp1$pred0[is.na(tmp1$avgY)]
  tmp1$adj_avg[is.na(tmp1$adj_avg)] <- tmp1$pred0[is.na(tmp1$adj_avg)]
  # Combine train and test into one vector
  return(c(oofInd$oof, tmp1$adj_avg))
}


get_target_encoding <- function(data, cate_list){
  
  data <- tem ; cate_list <- cols
  
  for_enc <- data.table(data[ ,cate_list, with=F],                        # takes only low cardinaility categorical features
                        y = c(y,rep(NA,nrow(test))),                      # target
                        pred0 = rep(mean(y),nrow(tem)),                   # global mean
                        filter=c(rep(0,nrow(train)),rep(1,nrow(test)))    # Indicater, to show where is train and test
  ) 
  
  cvFoldsList <- createFolds(y, k=5, list=TRUE, returnTrain=FALSE)       # create 5 folds
  
  tmp1 <- data.table(row_idx = 1:nrow(data))
  for(cate in cate_list){
    tmp <- data.table(fea = catNWayAvgCV(for_enc, varList=cate, y="y", pred0="pred0",
                                         filter=for_enc$filter==0, k=30, f=10, r_k=0.02, cv=cvFoldsList))
    colnames(tmp) <- paste0("target_encoding__OF__", cate)
    tmp1 <- data.table(tmp1, tmp)
  }
  cat("====  Process completed  ====", "\n")
  return(tmp1[,-1])
}

# Extract feature from land_price.csv
extract_feature_land <- function(data){
  
  # Numeric : 2,3,14,20,22,34,40,41,(43)  44:49,50:80
  # Cate    : 5,6,17,18,24,32,37
  
  colnames(data)[c(2:6,14,17,18,20,22,24,32,34,37,40,41,43,44:49,50:80 )] <- 
    c("longitude", "latitude", "area_code", "use", "seq_num", "chiseki", "backbone", "facility", "maguchi_ratio", #20
      "floor", "faced_road_type", "envir", "distance_to_aki", "cityplan_type", "tapei_ratio", "floor_area_ratio", "selected_yr", #43
      paste0("s",58:63,"_price"), paste0("h",1:31,"_price")  )
  
  # Data cleaning
  df_tmp <- data[,c(paste0("s",58:63,"_price"), paste0("h",1:31,"_price")), with=F]
  df_tmp[df_tmp == 0] <- NA
  data[,c(paste0("s",58:63,"_price"), paste0("h",1:31,"_price"))]<- df_tmp
  invisible(gc())
  
  data[, `:=`(selected_yr = as.numeric(str_count(selected_yr, "1")),
              past15yr_price_mean = apply(data[, paste0("h",c(16:31),"_price"), with=F], 1, mean, na.rm=T),
              past15yr_price_sd = apply(data[, paste0("h",c(16:31),"_price"), with=F], 1, sd, na.rm=T),
              overall_price_mean = apply(data[, c(paste0("s",58:63,"_price"), paste0("h",1:31,"_price")), with=F], 1, mean, na.rm=T),
              overall_price_sd = apply(data[, c(paste0("s",58:63,"_price"), paste0("h",1:31,"_price")), with=F], 1, sd, na.rm=T)
  )]
  
  fun_list1 <- c("mean", "sd", "max", "min", "sum","skewness", "kurtosis",
                 "IQR", "iqr_ratio", "mean_var", "range_diff", "range_per", "hl_ratio")
  fun_list2 <- c("n_distinct", "entropy_freqtable") 
  tar_col_numer <- c("longitude", "latitude", "chiseki", "maguchi_ratio", "floor", "distance_to_aki","tapei_ratio", "floor_area_ratio",
                     "selected_yr", paste0("h",c(1,11,21,31),"_price"), 
                     "overall_price_mean", "overall_price_sd", "past15yr_price_mean", "past15yr_price_sd")
  tar_col_cate <- c("use", "seq_num", "backbone", "facility", "faced_road_type", "envir", "cityplan_type")
  
  # Making features
  tmp <- data[ , .( col = .N ), by = "area_code"][,1] 
  
  for(funfun in fun_list1){
    for(tar_col in tar_col_numer){
      df <- data[ , .( col = get(funfun)( as.double(na.omit(get(tar_col))) ) ) , by = "area_code" ] 
      colnames(df)[2] <- paste0( "land__", funfun , "__ON__", tar_col )  
      tmp <- merge(tmp, df, by = "area_code", all.x=TRUE, sort=FALSE)
    }
  }
  
  for(funfun in fun_list2){
    for(tar_col in tar_col_cate){
      df <- data[, col := NULL][ , .( col = get(funfun)( get(tar_col) ) ) , by = "area_code" ] 
      colnames(df)[2] <- paste0( "land__", funfun , "__ON__", tar_col )  
      tmp <- merge(tmp, df, by = "area_code", all.x=TRUE, sort=FALSE)
    }
  }
  tmp <- tmp[, lapply(.SD, function(x){ ifelse(is.infinite(x) | is.nan(x) , NA, x) } )]
  return(tmp)
}















