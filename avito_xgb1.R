if(full_data) {
  dt1 <- s01_ths2
  base_ft <- dt1$search_time_rseq > 1 & !(dt1$UserID %% 50 == 1) & dt1$IsClick >=0
  print(sum(base_ft))
} else {
  dt1 <- s01_ths2[IsClick >=0, ]
  base_ft <- dt1$search_time_rseq > 2
}
k <- 100

calc_exp2(dt1, base_ft, 'IsClick', c('AdID'), 'exp2_ad', 100, verbose=T)
calc_exp2(dt1, base_ft, 'IsClick', c('SearchQuery'), 'exp2_query', 100, mean_y0=NULL, verbose=T)
calc_exp2(dt1, base_ft, 'IsClick', c('SearchQuery', 'AdID'), 'exp2_query_ad', 100, mean_y0=NULL, verbose=T)
calc_exp2(dt1, base_ft, 'IsClick', c('SearchQuery', 'Title'), 'exp2_query_title', 100, mean_y0=NULL, verbose=T)
calc_exp2(dt1, base_ft, 'IsClick', c('Params'), 'exp2_aparam', 100, mean_y0=NULL, verbose=T)
calc_exp2(dt1, base_ft, 'IsClick', c('LocationID', 'Params'), 'exp2_loc_aparam', 100, mean_y0=NULL, verbose=T)


dt1[, ad_title_nchar := nchar(Title)]

ad_sparam_query_cnt <- dt1[, list(ad_sparam_query_cnt=.N), by=.(AdID, SearchParams, SearchQuery)]

user_ad_sum3 <- dt1[, list(user_ad_cnt=.N), by=.(AdID, UserID)]

ad_sparam_entropy <- calc_entropy(dt1, 'AdID', c('SearchParams'), 'adid_sparam') 
ad_query_entropy <- calc_entropy(dt1, 'AdID', c('SearchQuery'), 'adid_query') 
left_merge_inplace(ad_sparam_entropy, ad_query_entropy, 'AdID', verbose=T)

left_merge_inplace(dt1, ad_sparam_query_cnt, by=c('AdID', 'SearchParams', 'SearchQuery'), verbose=T)
dt1[, ad_sparam_query_cnt_ratio := ad_sparam_query_cnt * 1.0 / ad_cnt]
left_merge_inplace(dt1, user_ad_sum3, by=c('AdID', 'UserID'), verbose=T)
left_merge_inplace(dt1, ad_sparam_entropy, by=c('AdID'), verbose=T)


use_exp <- F
feature_list1 <- c('Position', 'HistCTR', 'IsUserLoggedOn', 'LocationID', 'search_time3', 'search_time_seq',
                  'user_cat_cnt', 'user_v_cnt', 'user_pcat_cnt', 'user_visit_cat_entropy', 'search_cnt',
                  'hour', 'dow', 'CategoryID', 'ParentCategoryID', 'query_nchar', 'param_nchar', 'Price',
                  'ad_title_nchar', 'user_v_cat_avg_price', 'cat_price_diff', 'ad_params_nchar', 'params_nchar_diff',
                  'ad_param_v_cnt', 'search_ad_cnt', 'search_ad1_cnt', 'search_ad2_cnt', 'search_ad3_cnt',
                  'ad_position2', 'time_gap_prev_search', 'user_ad_seq', 'user_searchquery_seq', 'ad_cnt',
                  'user_csum_cads_cnt', 'user_csum_cads_y', 
                  'cat_equal', 
                  'total_other_ads_visit_cnt',
                  'user_session_seq', 'user_session_no', 'user_search_no', 'user_same_search_seq',
                  "UserAgentID",       "UserAgentOSID",    "UserDeviceID",     "UserAgentFamilyID",
                  'user_phone_cnt',
                  'user_search_loc_entropy', 'user_search_dow_entropy', 'user_search_hour_entropy',
                  'user_visit_loc_entropy', 'user_visit_dow_entropy', 'user_visit_hour_entropy',
                  'user_search_sparam_entropy', 'user_visit_param_entropy',
                  'user_ad_prior_clicks' 
                  )

feature_list2 <- c('user_ad_cnt', 'ad_sparam_query_cnt_ratio', 
                   'adid_sparam_entropy', 'adid_query_entropy')

feature_list_exp <- c('exp2_ad', 'exp2_query', 'exp2_query_ad', 'exp2_query_title', 'exp2_loc_aparam', 'exp2_aparam')

feature_list <- c(feature_list1, feature_list2)
if(use_exp) feature_list <- c(feature_list, feature_list_exp)
  
MSV <- -999
for(vn in feature_list) {
  if(sum(is.na(dt1[[vn]]))>0) {
    print(vn)
    dt1[is.na(dt1[[vn]]), paste(vn, sep='') := MSV]
  }
}


y <- dt1$IsClick
testing <- T

if (full_data) {
  if(testing) {
    fv1 <- dt1$search_time_rseq == 2
    ft <- y >= 0 & (!fv1) & dt1$UserID %% 10 == 1
    fv <- y >= 0 & fv1
  } else {
    fv1 <- dt1$UserID %% 50 == 1 & dt1$search_time_rseq == 2
    ft <- y >= 0 & (!fv1) #& dt1$search_time_rseq < 200 #& dt1$UserID %% 10 == 1
    fv <- y >= 0 & fv1
  }
  fh <- y < 0
} else {
  ft <- (dt1$search_time_rseq > 2 & dt1$search_time_rseq < 200)
  fv <- dt1$search_time_rseq <= 2  
}


xt <- as.matrix(dt1[ft, feature_list, with=F])
yt <- y[ft]
xv <- as.matrix(dt1[fv, feature_list, with=F])
yv <- y[fv]
if (full_data) {
  xh <- as.matrix(dt1[fh, feature_list, with=F])
  yh <- y[fh]
}



print(dim(xt))
print(dim(xv))
if(full_data) {
  print(dim(xh))
  if(dim(xh)[1] != dim(sub0)[1]) stop('xh has wrong shape!!!')
}

MSV2 <- 9999999
if(full_data & !testing) dtest <- xgb.DMatrix(xh, missing = MSV2)
dvalid <- xgb.DMatrix(xv, label = yv, missing = MSV2)

ds_k <- 50
ds_kt <- 1
if(full_data) {
  ds_k <- 50
  ds_kt <- 1
}

if(F) {
seltv <- get_ds_filter(y, ds_k, ds_kt)
dt1sel <- dt1[seltv, c('IsClick', feature_list1, feature_list2), with=F]
dt1sel[, partition := 0]
dt1sel[dt1$search_time_rseq[seltv]==2, partition := 1]

write.csv(dt1sel, file='output/dt1sel.csv', row.names=F)
}

xgbs <- list()
predv <- 0
predh <- 0
predt <- 0
ctr <- 0

idx_exp <- which(feature_list %in% feature_list_exp)

for(i in c(1:4)) {
  ctr <- ctr + 1
  
  set.seed(i)
  selt <- get_ds_filter(yt, ds_k, ds_kt)
  selv <- get_ds_filter(yv, ds_k, ds_kt)
  
  t_shuffle_idx <- sample(sum(selt))  
  xt1 <- xt[selt, ][t_shuffle_idx, ]
  print(dim(xt1))
  if(use_exp) xt1[, idx_exp] <- xt1[, idx_exp] * matrix(exp((runif(dim(xt1)[1]*length(feature_list_exp))-.5)*.04), nrow=dim(xt1)[1], ncol=length(feature_list_exp))
  dtrain1 <- xgb.DMatrix(xt1, label = yt[selt][t_shuffle_idx], missing = MSV2)
  dvalid1 <- xgb.DMatrix(xv[selv, ], label = yv[selv], missing = MSV2)
  watchlist <- list(train = dtrain1, valid = dvalid1)

  tree_depth <- 11
  if(full_data & !testing) tree_detph <- 14
  ts1 <- proc.time()
  bst <- xgboost2(dtrain1, max.depth = tree_depth, watchlist = watchlist, colsample_bytree = .4, verbose=1, print.every.n=10, eval_metric='logloss',
                  eta = 0.15, nthread = 4, nround = 150, objective = "binary:logistic", set.seed=i, min_child_weight=1, num_parallel_tree=1)
  print(proc.time() - ts1)
  xgbs[[i]] <- bst
  if(full_data & !testing) {
    predh0 <- calibrate_ds(predict(bst, dtest), ds_k, ds_kt)
    predh <- predh + predh0   
  }
  
  pred <-  calibrate_ds(predict(bst, dvalid), ds_k, ds_kt)
  predv <- predv + pred
  print(paste(i, '-----------------', logloss(yv, predv / ctr)))
  gc()  
}

dt1v <- dt1[fv, ]
f1 <- dt1v$user_ad_prior_clicks > 0 | T
f1 <- !is.na(dt1v$exp2v_title_query)
dt1v[f1, my_lift((adid2_csum_y + .01*k)/(adid2_csum_cnt + k), predv[f1]/ctr, yv[f1], NULL, 20, print=T)]
dt1v[f1, my_lift(predv, predv[f1]/ctr, yv[f1], NULL, 500, print=T)]
dt1v[, pred1 := predv/ctr]

dt1v<-dt1v[order(SearchID, pred1), ]
tmp_pred1_rank <- dt1v[, .(pred_rank = cumsum(one), pred_mean=mean(pred1)), by=list(SearchID)]
dt1v[, pred1_rank:=tmp_pred1_rank$pred_rank]
dt1v[, pred1_mean:=tmp_pred1_rank$pred_mean]

fv1 <- runif(dim(dt1v)[1]) >= .5
fv2 <- !fv1

xvnew <- cbind(xv, predv/ctr)
xv1 <- xvnew[fv1, ]
xv2 <- xvnew[fv2, ]
yv1 <- yv[fv1]
yv2 <- yv[fv2]

dv1 <- xgb.DMatrix(xv1, label = yv1, missing = MSV2)
dv2 <- xgb.DMatrix(xv2, label = yv2, missing = MSV2)

watchlist2 <- list(train = dv1, valid = dv2)
bst2 <- xgboost2(dv2, max.depth = 3, watchlist = watchlist2, colsample_bytree = .3, verbose=1, print.every.n=10, eval_metric='logloss',
                eta = .1, nthread = 16, nround = 150, objective = "binary:logistic", set.seed=i, min_child_weight=1)
prednew <- predict(bst2, xgb.DMatrix(xvnew, missing = MSV2))
my_lift(dt1v$cat_price_diff[fv2], predv[fv2]/ctr, yv[fv2], NULL, 10)

dt1t <- dt1[ft, ]
dt1t[, pred := predt / ctr]
dt1t1 <- dt1t[UserID %% 100 == 11, ]
dt1t[search_time_rseq==3, my_lift(ad_rank_in_search_by_price, pred, IsClick, NULL, 10, print=T)]
k <- 100
dt1t1[, my_lift((ad_csum_cads_y + .01*k)/(ad_csum_cads_cnt + k), pred, IsClick, NULL, 10, print=T)]


predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh/ctr,5))
my_write_csv(predh.dt, file='output/sub0', row.names=F)

predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh/ctr,5))
my_write_csv(predh.dt, file='output/sub1', row.names=F)

predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh/ctr,6))
my_write_csv(predh.dt, file='output/sub2_0401x', row.names=F)

my_lift(dt1$HistCTR[fh], NULL, predh.dt$IsClick, NULL, 10)

logloss( yv, pmin(predv/ctr, .2))
logloss( yv, dt1$HistCTR[fv])
logloss( yv, pmax(pmin(dt1$HistCTR[fv], 0.016), 0.005))

k <- 50
my_lift((dt1$user_time_search_cnt_3sec[fv]), pmin(predv/ctr, .3), yv, NULL, 20, print=T)

my_lift(((dt1$CategoryID[!ft] == dt1$AdCategoryID[!ft])), predv/ctr, yv, NULL, 10, print=T)
my_lift(((dt1$cat_equal[!ft])), pred, IsClick, NULL, 10)


predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh/ctr,6))
my_write_csv(predh.dt, file='output/sub3_0400x', row.names=F)

predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh/ctr,6))
my_write_csv(predh.dt, file='output/sub3x16_0400x', row.names=F)

predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh/ctr,6))
my_write_csv(predh.dt, file='output/sub3_rseq200_0400x', row.names=F)


predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh/ctr,6))
my_write_csv(predh.dt, file='output/sub3x16_run2_0400x', row.names=F)

predh.dt <- data.frame(ID=dt1$ID[fh], IsClick=round(predh/ctr,6))
my_write_csv(predh.dt, file='output/sub4x16_0400x', row.names=F)
