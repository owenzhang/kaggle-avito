#hack for xgboost wrapper to get watchlist into the call
xgboost2 <- function(dtrain, params = list(), nrounds, 
                     verbose = 1, print.every.n = 1L, early.stop.round = NULL, watchlist = NULL,
                     maximize = NULL, num_parallel_tree=1, ...) {
  
  params <- append(params, list(...))
  
  bst <- xgb.train(params, dtrain, nrounds, watchlist, verbose = verbose, print.every.n=print.every.n,
                   early.stop.round = early.stop.round, num_parallel_tree=num_parallel_tree)
  
  return(bst)
}

#compute entropy by group, over subgrp
calc_entropy <- function(df, group, subgrp, tgt_vn_prefix) {
  sum1 <- df[, .N, by=list(df[[group]], df[[subgrp]])]
  setnames(sum1, c(group, subgrp, 'subgrpcnt'))
  sum2 <- df[, .N, by=list(df[[group]])]
  setnames(sum2, c(group, 'cnt'))
  #sum2[, dummy:=1]
  #dowdf <- data.table(x=unique(df[[subgrp]]), dummy=1)
  #setnames(dowdf, c(subgrp, 'dummy'))
  #sum2a <- merge(sum2, dowdf, by='dummy', allow.cartesian=T)
  sum3 <- merge(sum2, sum1, by=c(group))
  #sum3[is.na(subgrpcnt), subgrpcnt:=0]
  sum3[, entropy := - log(subgrpcnt * 1.0 / cnt) * subgrpcnt * 1.0 / cnt]
  sum3[is.na(entropy), entropy := 0]
  sum4 <- sum3[, sum(entropy), by=list(sum3[[group]])]
  setnames(sum4, c(group, paste(tgt_vn_prefix, 'entropy', sep='_')))
  return(sum4)
}

#generate smart downsample index
get_ds_filter <- function(y, ds_k, ds_kt=1) {
  r1 <- runif(length(y))
  sel <- r1 < 1.0 / ds_k
  sel[y==1] <- r1[y==1] < 1.0 / ds_kt
  return(sel)
}

#calibrate the prediction to original scale (before smart downsample)
calibrate_ds <- function(p, ds_k, ds_kt=1) {
  return(p * ds_kt / (p * ds_kt + (1 - p) * ds_k))
}

#response encoder for categorical features, with credibility adjustment and leave-one-out
calc_exp2 <- function(dt, ft, vn_y, by, tgt_vn, k, mean_y0=NULL, verbose=F) {
  dt[, tmp_y := dt[[vn_y]]]
  tmp <- dt[ft, list(.N, sum(tmp_y), mean(tmp_y)), by=by]
  if(verbose) print(paste("dim of summary :", dim(tmp)))
  setnames(tmp, c(by, 'tmp_cnt', 'tmp_sumy', 'tmp_mean_y'))
  if(is.null(mean_y0)) mean_y0 <- mean(tmp$tmp_mean_y)
  if(verbose) print(paste("mean_y0 = ", mean_y0))
  tmp[, tmp_mean_y := NULL]
  left_merge_inplace(dt, tmp, by=by, verbose=verbose)
  dt[is.na(tmp_cnt), tmp_cnt := 0]
  dt[is.na(tmp_sumy), tmp_sumy := 0]
  dt[ft, tmp_cnt := tmp_cnt - 1L]
  dt[ft, tmp_sumy := tmp_sumy - tmp_y]
  dt[, paste(tgt_vn, sep='') := (tmp_sumy + mean_y0 * k) / (tmp_cnt + k)]
  dt[, tmp_y := NULL]
  dt[, tmp_sumy := NULL]
  dt[, tmp_cnt := NULL]
  return(0)
}

#simple logistic loss function
logloss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = pmin(1-eps, pred, pmax(eps, pred))
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  #print(ll)
  ll = ll * -1/length(act)      
  return(ll)
}

#shift column by n rows
add_shift1 <- function(dt, key, n, nv, force=F) {
  tgt_vn <- paste(key, '_shift', nv, sep='')
  if(!force) {
    if(tgt_vn %in% names(dt)) stop(paste('variable ', tgt_vn, 'already exists in the data'))
  }
  if(n >0) a <- c(rep(NA, n), head(dt[[key]], -n))
  else a <- c(tail(dt[[key]], n), rep(NA, -n))
  dt[, tgt_vn := a, with=F]
}

#shift multiple columns by n rows
add_shift <- function(dt, keys, values, ns, force=F) {
  for(n in ns) {
    nv <- n
    if(n < 0) nv <- paste('N', -n, sep='')
    if(!is.null(keys)) {
      for(key in keys) {
        add_shift1(dt, key, n, nv, force)
      }
    }
    for(value in values) {
      add_shift1(dt, value, n, nv, force)
      if(!is.null(keys)) {
        for(key in keys) {
          dt[dt[[key]] != dt[[paste(key, '_shift', nv, sep='')]], paste(value, '_shift', nv, sep='') := NA, with=F]
        }
      }
    }
  }
}

#compute first n component of svd 
get_svd_u <- function(df1, u, v, x, n, tgt_vn_prefix) {
  df1[, u_idx:=as.integer(factor(df1[[u]]))]
  df1[, v_idx:=as.integer(factor(df1[[v]]))]
  if(is.null(x)) {
    m1 <- sparseMatrix(i=df1[['u_idx']], j=df1[['v_idx']])
  } else {
    m1 <- sparseMatrix(i=df1[['u_idx']], j=df1[['v_idx']], x=df1[[x]])
  }
  
  r <- irlba(m1, nu=n, nv=n)
  
  dtu <- data.frame(u_idx=c(1:dim(m1)[1]))
  for(i in c(1:n)) {
    dtu[[paste(tgt_vn_prefix, i, sep='_')]] <- r$u[, i]
  }  
  
  #print(names(dtu))
  #print(names(df1))
  rmap <- data.table(unique(df1[, c(u, 'u_idx'), with=F]))
  #print(rmap)
  dtu1 <- merge(dtu, rmap, by='u_idx')
  dtu1$u_idx <- NULL
  return(dtu1)
}

#wrapper to make calling get_svd_u easier
get_svd <- function(df, vn_u, vn_v, n, tgt_vn) {
  tmp1 <- df[, length(time2), by=list(df[[vn_u]], df[[vn_v]])]
  setnames(tmp1, c(vn_u, vn_v, 'cnt'))
  tmp1 <- tmp1[!is.na(tmp1[[vn_v]]), ]
  tmp1[, x1:=log(cnt + 1)]
  
  svd1 <- get_svd_u(tmp1, vn_u, vn_v, 'x1', n, tgt_vn)
  return(svd1)
}

#compute ngram/tfidf/svd
calc_ngram_svd <- function(df, vn_id, vn_text, NN, tf_min, df_max, ngram_n, svd_n, tgt_vn_prefix) {
  
  #df <- em_ev2_seq
  #vn_id <- "enrollment_id"
  #vn_text <- 'em_ev2_seq'
  #NN <- -1
  #tf_min <- 10
  #df_max <- 10000
  #ngram_n <- c(1:2)
  #svn_n <- 20
  #tgt_vn_prefix <- 'em_ev3gram_svd_em'
  ptm <- proc.time()
  
  if(NN<0) NN <- dim(df)[1]
  
  texts <- as.list(df[[vn_text]][1:NN])
  org_ids <- df[[vn_id]][1:NN]
  
  a <- list()
  for (n1 in ngram_n) {
    a1 <- textcnt(texts, split='[[:space:]]', n=n1, method='string', recursive = T, verbose = F)
    a <- c(a, a1)
    print(paste("done counting words by document for ngram ", n1, length(a)))
    print(proc.time() - ptm)
  }
  a2 <- unlist(a)
  an <- lapply(a, function(x) length(x))
  df1 <- data.frame(id=org_ids, cnt=unlist(an))
  ids <- unlist(apply(df1[, c("id", "cnt")], 1, function(x) rep(x[1], x[2])))
  dt2 <- data.table(id=ids, ngram=names(a2), cnt=a2)
  
  dt3 <- dt2[, length(id), by=list(ngram)]
  setnames(dt3, c('ngram', 'cnt_total'))
  dt3 <- dt3[cnt_total >= tf_min, ]
  
  dt4 <- merge(dt2, dt3, by='ngram')
  
  dt4[, tfidf := log(cnt + 1) * log(NN / cnt_total)]
  
  print(paste(dim(dt2), dim(dt3), dim(dt4), sep="|"))
  setnames(dt4, "id", vn_id)
  svd1 <- get_svd(dt4, vn_id, 'ngram', svd_n,  tgt_vn_prefix)
  
  print(proc.time() - ptm)
  return(svd1)
}

#write compressed csv.gz
my_write_csv <- function(obj, file, row.names=F, timing=T) {
  st <- proc.time()
  con <- pipe(paste("pigz -p30 > ", file, ".csv.gz", sep=''), "wb")
  write.csv(obj, file = con, row.names=row.names)
  close(con)
  if(timing) {
    print(proc.time() - st)
  }
}

#fill all missings in DT with value
dt_fill_na = function(DT, value=0) {
  # either of the following for loops
  
  # or by number (slightly faster than by name) :
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,value)
}

#merge a small dataset to a large data.table without copying the large data.table
left_merge_inplace <- function(dt1, dt2, by, verbose=F, fill.na=NA) {
  st <- proc.time()
  dt1a <- copy(dt1[, by, with=F])
  dt2a <- copy(dt2[, by, with=F])
  if (verbose) {
    print('small datasets created')
    print(proc.time() - st)
  }
  dt1a[, tmp_idx1 := c(1:dim(dt1a)[1])]
  dt2a[, tmp_idx2 := c(1:dim(dt2a)[1])]
  if (verbose) {
    print('row index created')
    print(proc.time() - st)
  }
  dt3 <- merge(dt1a, dt2a, by=by, all.x=T)
  if (verbose) {
    print('small datasets merged')
    print(proc.time() - st)
  }
  dt3 <- dt3[order(tmp_idx1), ]
  if (verbose) {
    print('merged dataset reordered')
    print(proc.time() - st)
  }
  dt2_idx_map <- dt3$tmp_idx2
  if (verbose) {
    print('row index generated')
    print(proc.time() - st)
  }
  
  for(vn in names(dt2)) {
    if(!(vn %in% by)) {
      dt1[, paste(vn, sep='') := dt2[[vn]][dt2_idx_map]]
      if(!is.na(fill.na)) dt1[is.na(dt1[[vn]]), paste(vn, sep=''):=fill.na]
      if (verbose) {
        print(paste('assigned variable ', vn))
        print(proc.time() - st)
      }
    }
  }
}

#utility function for ngram based text similarity
get_ngram_cnts <- function(texts, ngram_n) {
  ptm <- proc.time()
  a <- list()
  for (n1 in ngram_n) {
    a1 <- textcnt(texts, split='[[:space:]|[:punct:]]', n=n1, method='string', recursive = T, verbose = F)
    a <- c(a, a1)
    print(paste("done counting words by document for ngram ", n1, length(a)))
    print(proc.time() - ptm)
  }
  return(a)
}


#utility function for ngram based text similarity
get_rep_id_dt <- function(a, ids0) {
  an <- lapply(a, function(x) length(x))
  df1 <- data.frame(id=ids0, cnt=unlist(an))
  ids <- unlist(apply(df1[, c("id", "cnt")], 1, function(x) rep(x[1], x[2])))
  #print(length(ids))
  dt1 <- data.table(id=ids, ngram=names(unlist(a)), cnt=unlist(a))
  return(dt1)
}

#text similarity based on cosine distance of ngram/tfidf
calc_ngram_dist <- function(dt0, vn_id, vn_text1, vn_text2, ngram_n, tgt_vn) {
  if(F) {
    dt0 <- lt0[, ]
    
    vn_id <- "id"
    vn_text1 <- 'user_skill_str'
    vn_text2 <- 'req_skill_str'
    NN <- -1
    ngram_n <- c(1:2)
  }
  ptm <- proc.time()
  
  dt0a <- dt0[, c(vn_id, vn_text1, vn_text2), with=F]
  dt0a[, org_ord:= c(1:dim(dt0a)[1])]
  
  dt1 <- data.table(unique(data.frame(dt0a[, c(vn_id, vn_text1, vn_text2), with=F])))
  setnames(dt1, c('id', vn_text1, vn_text2))
  NN <- dim(dt1)[1]
  
  texts1 <- (dt1[[vn_text1]])[1:NN]
  texts2 <- (dt1[[vn_text2]])[1:NN]
  org_ids <- dt1[['id']][1:NN]
  
  a1 <- get_ngram_cnts(texts1, ngram_n)
  a2 <- get_ngram_cnts(texts2, ngram_n)
  
  dt1_wids <- get_rep_id_dt(a1, org_ids)
  dt2_wids <- get_rep_id_dt(a2, org_ids)
  
  dt12_wids <- rbind(dt1_wids, dt2_wids)
  dt12_sum <- dt12_wids[, list(total_cnt=sum(cnt)), by=list(ngram)]
  
  setnames(dt1_wids, 'cnt', 'cnt1')
  setnames(dt2_wids, 'cnt', 'cnt2')
  
  dt3 <- merge(merge(dt1_wids, dt2_wids, by=c('id', 'ngram'), all.x=T, all.y=T), dt12_sum, by='ngram')
  
  dt3[is.na(cnt1), cnt1:=0]
  dt3[is.na(cnt2), cnt2:=0]
  
  dt3_sum <- dt3[, list(doc_cnt=.N), by=list(ngram)]
  dt4 <- merge(dt3, dt3_sum, by='ngram')
  setnames(dt4, 'id', vn_id)
  
  dt4[, tfidf1 := log(cnt1 + 1) * log(NN / doc_cnt)]
  dt4[, tfidf2 := log(cnt2 + 1) * log(NN / doc_cnt)]
  
  dt5 <- dt4[, list(cos_sim=sum(tfidf1 * tfidf2) / sqrt(sum(tfidf1*tfidf1)*sum(tfidf2*tfidf2))), by=vn_id]
  setnames(dt5, 'cos_sim', tgt_vn)
  return(dt5)
}

