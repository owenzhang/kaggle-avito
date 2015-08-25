time2 <- strptime(s01_visit$ViewDate, '%Y-%m-%d %H:%M:%S')

min_time2 <- min(time2)
min_time3 <- as.Date(min_time2)
time3 <- as.integer(difftime(time2, min_time3, unit='sec'))
if(conserve_ram) {
  rm(time2)
}
gc()

s01_visit[, visit_time3 := time3]
s01_visit[, hour := as.integer(round(as.integer(visit_time3)  %/% 3600)) %% 24]
s01_visit[, dow := as.integer(round(as.integer(visit_time3)  %/% 86400)) %% 7]

user_visit_dow_entropy <- calc_entropy(s01_visit, 'UserID', 'dow', 'user_visit_dow') 
user_visit_hour_entropy <- calc_entropy(s01_visit, 'UserID', 'hour', 'user_visit_hour') 

s01_visit <- s01_visit[order(UserID, visit_time3), ]
add_shift(s01_visit, c('UserID'), c('visit_time3'), 1, force=F)
user_visit_time_interval_mean <- s01_visit[, .(user_visit_time_interval_mean=mean(visit_time3 - visit_time3_shift1, na.rm=T)), by=UserID]


# count of visits in subcategory and category
# entropy of subcat and cat
setnames(s01_ads, 'LocationID', 'AdLocationID')
setnames(s01_ads, 'CategoryID', 'AdCategoryID')

left_merge_inplace(s01_visit, s01_ads, 'AdID', verbose=T)
setnames(cat, 'CategoryID', 'AdCategoryID')
left_merge_inplace(s01_visit, cat, 'AdCategoryID', verbose=T)

user_visit_loc_entropy <- calc_entropy(s01_visit, 'UserID', 'AdLocationID', 'user_visit_loc') 
user_visit_param_entropy <- calc_entropy(s01_visit, 'UserID', 'Params', 'user_visit_param') 


ad_param_v_cnt <- s01_visit[, .N, by=list(Params)]
setnames(ad_param_v_cnt, 'N', 'ad_param_v_cnt')

ad_param_avg_price <- s01_visit[, mean(Price), by=list(Params)]
setnames(ad_param_avg_price, 'V1', 'ad_param_avg_price')

left_merge_inplace(ad_param_v_cnt, ad_param_avg_price, by='Params')

user_v_avg_price <- s01_visit[, mean(Price, na.rm=T), by=list(UserID)]
setnames(user_v_avg_price, 'V1', 'user_v_avg_price')
user_v_cat_avg_price <- s01_visit[, mean(Price), by=list(UserID, AdCategoryID)]
setnames(user_v_cat_avg_price, 'V1', 'user_v_cat_avg_price')

user_v_price_sum2 <- s01_visit[, .(user_v_min_price=min(Price, na.rm=T), user_v_max_price=max(Price, na.rm=T), user_v_var_price=var(Price, na.rm=T)), by=UserID]
user_v_price_sum2[is.infinite(user_v_min_price), user_v_min_price := NA]
user_v_price_sum2[is.infinite(user_v_max_price), user_v_max_price := NA]

user_cat_cnt <- s01_visit[, .N, by=list(UserID, AdCategoryID)]
setnames(user_cat_cnt, "N", "user_cat_cnt")
user_pcat_cnt <- s01_visit[, .N, by=list(UserID, ParentCategoryID)]
setnames(user_pcat_cnt, "N", "user_pcat_cnt")
user_v_cnt <- s01_visit[, .N, by=list(UserID)]
setnames(user_v_cnt, "N", "user_v_cnt")

left_merge_inplace(user_v_cnt, user_v_avg_price, by='UserID')
left_merge_inplace(user_cat_cnt, user_v_cat_avg_price, by=c('UserID', 'AdCategoryID'))

user_visit_cat_entropy <- calc_entropy(s01_visit, 'UserID', 'AdCategoryID', 'user_visit_cat') 

if(conserve_ram) {
  rm(s01_visit)
}
gc()

