# run entire script with 10% data -- highly recommended in case there are bugs

setwd('/home/fast/2015_avito') # modify this line to point the right folder

require(inline)
require(sqldf)
require(data.table)
require(xgboost)
require(tau)

source('kaggle-avito/avito_utils.R')

full_data <- F
source('kaggle-avito/avito_data1.R')

source('kaggle-avito/avito_phone.R')

source('kaggle-avito/avito_search.R')

source('kaggle-avito/avito_visit.R')

source('kaggle-avito/avito_cat_cnt.R')

source('kaggle-avito/avito_data_merge.R')

n_repeat <- 8
feature_list <- c(feature_list1, feature_list2)
source('kaggle-avito/avito_train_xgb.R')
predv_list2 <- predv / ctr

feature_list <- feature_list1
source('kaggle-avito/avito_train_xgb.R')
predv_list1 <- predv / ctr

predv_avg12 <- predv_list1 * .5 + predv_list2 * .5
print(paste("logloss of avg of two xgbs (should be ~.04176): ", logloss(yv, predv_avg12)))

