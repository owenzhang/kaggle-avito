#fit xgboost model with repetitively smart sampled data (keeping all events, randomly sample non-events)

if(use_exp) feature_list <- c(feature_list, feature_list_exp)

MSV <- -999
for(vn in feature_list) {
  if(sum(is.na(dt1[[vn]]))>0) {
    print(vn)
    dt1[is.na(dt1[[vn]]), paste(vn, sep='') := MSV]
  }
}


y <- dt1$IsClick
testing <- F

if (full_data) {
  if(testing) {
    fv1 <- dt1$search_time_rseq == 2
    ft <- y >= 0 & (!fv1) & dt1$UserID %% 10 == 1
    fv <- y >= 0 & fv1
  } else {
    fv1 <- dt1$UserID %% 50 == 1 & dt1$search_time_rseq == 2
    ft <- y >= 0 & (!fv1) & dt1$search_time_rseq < rseq_limit #& dt1$UserID %% 10 == 1
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

xgbs <- list()
predv <- 0
predh <- 0
predt <- 0
ctr <- 0

idx_exp <- which(feature_list %in% feature_list_exp)

for(i in c(1:n_repeat)) {
  ctr <- ctr + 1
  
  set.seed(rseed_offset + i)
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
  if(full_data & !testing) tree_depth <- 14
  print(paste("tree depth", tree_depth))
  ts1 <- proc.time()
  bst <- xgboost2(dtrain1, max.depth = tree_depth, watchlist = watchlist, colsample_bytree = .4, verbose=1, print.every.n=10, eval_metric='logloss',
                  eta = 0.15, nthread = 16, nround = 150, objective = "binary:logistic", set.seed=i, min_child_weight=1, num_parallel_tree=1)
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
