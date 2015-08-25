# run entire script with 100% data 

setwd('/home/fast/2015_avito') # modify this line to point the right folder

require(inline)
require(sqldf)
require(data.table)
require(xgboost)
require(tau)

source('kaggle-avito/avito_utils.R')

full_data <- T
source('kaggle-avito/avito_data1.R')

source('kaggle-avito/avito_phone.R')

source('kaggle-avito/avito_search.R')

source('kaggle-avito/avito_visit.R')

source('kaggle-avito/avito_cat_cnt.R')

source('kaggle-avito/avito_data_merge.R')

n_repeat <- 8

#one model with shorter feature list and all the data points
rseq_limit <- 1e6
xt <- 0
gc()
rseed_offset <- 200
feature_list <- feature_list1
source('kaggle-avito/avito_train_xgb.R')
predv_list1 <- predv / ctr
predh_list1 <- predh / ctr

#another model with longer feature_list but only last 200 impressions
rseq_limit <- 200
xt <- 0
gc()
rseed_offset <- 300
feature_list <- c(feature_list1, feature_list2)
source('kaggle-avito/avito_train_xgb.R')
predv_list2_r200 <- predv / ctr
predh_list2_r200 <- predh / ctr

#test on 2% validation data
predv_avg2 <- (predv_list1 + predv_list2_r200) / 2
print(paste("logloss of avg of two xgbs (should be ~.04252): ", logloss(yv, predv_avg2)))

#create final submission
predh_avg2 <- (predh_list1 + predh_list2_r200) / 2
predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh_avg2,6))
my_write_csv(predh.dt, file='avito_sub_finalx8_final', row.names=F) #private LB ~.04029
