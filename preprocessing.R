


# system setting and call library
Sys.setlocale("LC_CTYPE", locale="Japanese")

library(tidyverse)
library(lightgbm)
library(moments)
library(entropy)
library(rBayesianOptimization)
library(MLmetrics)
library(data.table)
library(caret)

# Read data
train <- fread("train_data.csv", encoding = "UTF-8") 
test <- fread("test_data.csv", encoding = "UTF-8")
land_price <- fread("published_land_price.csv", encoding = "UTF-8")
y <- log1p(train$y) ; train$y <- NULL
test_id <- test$id

tem <- rbind(train,test)

# ==========================================================================================
# Data Cleaning
tem <- tem %>% as.data.frame() %>%
  mutate(
    
    # 全角 -> 半角
    間取り = z2h(間取り),
    
    # 最寄駅：距離（分） (time_to_nearest_aki)
    time_to_nearest_aki = ifelse(`最寄駅：距離（分）` == "30分?60分", 45, `最寄駅：距離（分）`),
    time_to_nearest_aki = ifelse(time_to_nearest_aki == "1H?1H30", 75, time_to_nearest_aki),
    time_to_nearest_aki = ifelse(time_to_nearest_aki == "1H30?2H", 105, time_to_nearest_aki),
    
    # 間取り
    room_type_normal = as.numeric(str_extract(間取り,"[0-9]+")),
    room_type_R = ifelse(grepl("R",間取り), 1, 0 ),
    room_type_L = ifelse(grepl("L",間取り), 1, 0 ),
    room_type_D = ifelse(grepl("D",間取り), 1, 0 ),
    room_type_K = ifelse(grepl("K",間取り), 1, 0 ),
    room_type_S = ifelse(grepl("S",間取り), 1, 0 ),
    
    # 面積（㎡） (sq_meter)
    sq_meter = ifelse(`面積（㎡）` == "2000㎡以上", 2500, `面積（㎡）`),
    sq_meter = as.numeric(ifelse(sq_meter  == "5000㎡以上", 5500, sq_meter)),
    
    # 間口 (maguchi)
    maguchi = as.numeric(ifelse(間口 == "50.0m以上", 75, 間口)),
    
    # 延床面積（㎡）(nobeyuka_m2)
    nobeyuka_m2 = as.numeric(ifelse(`延床面積（㎡）` == "2000㎡以上", 2500, `延床面積（㎡）`)),
    
    # 建築年 (built_year)
    year1 = str_remove_all(建築年,'[0-9]|年'),
    year1 = ifelse(year1 == "昭和", 1925, year1), 
    year1 = ifelse(year1 == "平成", 1988, year1), 
    built_year_b4ww2 = ifelse(year1 == "戦前", 1, 0), 
    year1 = as.numeric(ifelse(year1 == "戦前", NA, year1)), 
    year2 = as.numeric(str_extract_all(建築年, '[0-9]+')),
    built_year = year1 + year2,
    
    # 取引時点 (transaction date)
    trans_date_yr =  as.numeric(str_extract_all(取引時点, "[0-9]+")), 
    trans_date_q = as.numeric(z2h(str_remove_all(取引時点, "[0-9]|年第|四半期"))),
    trans_date = trans_date_yr + 0.2 * trans_date_q,
    
    
    
  ) %>%
  select(-`最寄駅：距離（分）`, -間取り, -`面積（㎡）`, -間口, -`延床面積（㎡）`, -建築年, -year1, -year2, -取引時点)


# JPN -> ENG
# 種類, 地域, 市区町村コード, 都道府県名, 市区町村名, 地区名, 最寄駅：名称, 土地の形状, 建物の構造, 用途, 今後の利用目的, 
# 前面道路：方位, 前面道路：種類, 前面道路：幅員（ｍ）, 都市計画, 建ぺい率（％）, 容積率（％）, 改装, 取引の事情等

colnames(tem)[c(2:20)] <- c("type", "land_type", "area_code", "city", "area", "sub_area", "nearest_aki_name", "land_shape",
                            "house_backbone", "use", "future_use", "faced_direction", "faced_road_type", "faced_road_width", "city_plan", 
                            "tapei_ratio", "floor_area_ratio", "reconstructed", "note"
)

tem <- tem %>% mutate(
  room_total = apply( tem[,c("room_type_normal", "room_type_R","room_type_L","room_type_D","room_type_K","room_type_S")], 1, 
                      function(x){sum(x,na.rm = T)}  ) ,
  square_meter_per_room = sq_meter / room_total ,
  square_meter_per_room_v2 = sq_meter / room_type_normal
) %>%
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
  select(-area, -id, -city) %>%
  as.data.table()


cols <- names(which(lapply(tem,class) == "character"))
tem[, (cols) := lapply(.SD, function(z) as.integer(as.factor(z))), .SDcols = cols]
df6 <- extract_feature_land(land_price)
tem <- merge(tem, df6, by = "area_code", all.x=TRUE, sort=FALSE)
# ===============================================================================================================
# Making features : part 1~5
fun_list1 <- c("mean", "sd", "max", "min", "sum","skewness", "kurtosis",
               "IQR", "iqr_ratio", "beyond1std_ratio", "mean_var", "range_diff", "range_per", "hl_ratio", "sw_stat",
               "x_diff", "x_ratio", "x_zscore", "entropy")
fun_list2 <- c("n_distinct", "freq1ratio", "freq1count", "entropy_freqtable")

numer_list <- c("sq_meter", "nobeyuka_m2", "square_meter_per_room", "built_year", "faced_road_width", "floor_area_ratio", 
                "maguchi" ,"square_meter_per_room_v2", "trans_date", "land__mean__ON__h31_price")

cate_list <- c("area_code", "nearest_aki_name", "land_type", "use", "sub_area", "trans_date_yr")


df1 <- get_group_feature_numer2cate(tem, fun_list1, numer_list, cate_list)
df2 <- get_group_feature_cate2cate(tem, fun_list2, cate_list)
df3 <- get_bayes_mean(tem, numer_list, cate_list)
df4 <- get_count_encoding(tem, cate_list = cols)
df5 <- get_target_encoding(tem, cate_list = cols)
tem1 <- data.table(tem, df1, df2, df3, df4, df5)



