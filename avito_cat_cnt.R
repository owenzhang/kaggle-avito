
left_merge_inplace(s01_ths, s01_ads, by='AdID', verbose=T)
left_merge_inplace(s01_ths, cat, by='AdCategoryID', verbose=T)
left_merge_inplace(s01_ths, s01_search, by='SearchID', verbose=T)

if(conserve_ram) {
  rm(s01_search)
  gc()
}

s01_ths1 <- s01_ths[, c('UserID', 'AdID', 'search_time3', 'SearchQuery', 'IsClick'), with=F]
s01_ths1[, one:=1]
s01_ths1[, org_idx:=c(1:.N)]
s01_ths1[, y1:=pmax(IsClick, 0)]

s01_ths1 <- s01_ths1[order(UserID, AdID, search_time3), ]
tmp <- s01_ths1[, list(cumsum(one), cumsum(y1)), by=list(UserID, AdID)]
tmp[, org_idx:=s01_ths1$org_idx]
system.time(tmp <- tmp[order(org_idx), ])
s01_ths[, user_ad_seq := tmp$V1]
s01_ths[, user_ad_prior_clicks := tmp$V2 - pmax(IsClick, 0)]

s01_ths1 <- s01_ths1[order(UserID, SearchQuery, search_time3), ]
tmp <- s01_ths1[, cumsum(one), by=list(UserID, SearchQuery)]
tmp[, org_idx:=s01_ths1$org_idx]
tmp <- tmp[order(org_idx), ]
s01_ths[, user_searchquery_seq := tmp$V1]
s01_ths[SearchQuery=='', user_searchquery_seq := -user_searchquery_seq]

if(conserve_ram) {
  rm(s01_ths1); gc()
}

if(full_data) {
  s01_ths1x <- s01_ths[ObjectType==3 & search_time_rseq<= 1000, ]
} else {
  s01_ths1x <- s01_ths[ObjectType==3 & search_time_rseq<= 2000 & search_time_rseq >= 2, ]
}

if(conserve_ram) {
  rm(s01_ths); gc()
}


ad_cnt <- s01_ths1x[, .N, by=list(AdID)]
setnames(ad_cnt, 'N', 'ad_cnt')

s01_ths1x[, is_test := as.integer(IsClick <0)]

search_sum1 <- s01_ths1x[, list(.N, sum(IsClick)), by=list(SearchID, UserID, is_test, search_time3)]
setnames(search_sum1, c('N', 'V2'), c('cnt', 'sumy'))
search_sum1 <- search_sum1[order(UserID, is_test, search_time3), ]
tmp <- search_sum1[, list(cumsum(cnt), cumsum(sumy)), by='UserID']
search_sum1[, user_csum_cads_cnt := tmp$V1 - cnt]
search_sum1[, user_csum_cads_y := tmp$V2 - sumy]


tmp <- search_sum1[, list(cumsum(cnt), cumsum(sumy)), by='UserID']
search_sum1[, user_csum_cads_cnt := tmp$V1 - cnt]
search_sum1[, user_csum_cads_y := tmp$V2 - sumy]

ad_info <- s01_ths1x[, c('one', 'IsClick', 'AdID', 'search_time3'), with=F]
ad_info[, org_idx := c(1:dim(ad_info)[1])]
ad_info <- ad_info[order(AdID, search_time3), ]
tmp <- ad_info[, list(cumsum(one), cumsum(IsClick)), by='AdID']
tmp[, org_idx := ad_info$org_idx]
tmp <- tmp[order(org_idx), ]
s01_ths1x[, ad_csum_cads_cnt := tmp$V1 - one]
s01_ths1x[, ad_csum_cads_y := tmp$V2 - IsClick]


left_merge_inplace(s01_ths1x, search_sum1[, c('SearchID', 'user_csum_cads_cnt', 'user_csum_cads_y'), with=F], by='SearchID', verbose=T)
if(conserve_ram) {
  rm(search_sum1); gc()
}
left_merge_inplace(s01_ths1x, ad_param_v_cnt, by='Params', verbose=T)
left_merge_inplace(s01_ths1x, user_v_cnt, by=c('UserID'), verbose=T)
left_merge_inplace(s01_ths1x, user_pcat_cnt, by=c('UserID', 'ParentCategoryID'), verbose=T)
left_merge_inplace(s01_ths1x, user_visit_cat_entropy, by=c('UserID'), verbose=T)
left_merge_inplace(s01_ths1x, search_ad_cnt, by='SearchID', verbose=T)
left_merge_inplace(s01_ths1x, ad_cnt, by='AdID', verbose=T)
#setnames(user_cat_cnt, 'CategoryID', 'AdCategoryID')
left_merge_inplace(s01_ths1x, user_cat_cnt, by=c('UserID', 'AdCategoryID'), verbose=T)


left_merge_inplace(s01_ths1x, user_search_loc_entropy, by='UserID', verbose=T)
left_merge_inplace(s01_ths1x, user_search_dow_entropy, by='UserID', verbose=T)
left_merge_inplace(s01_ths1x, user_search_hour_entropy, by='UserID', verbose=T)
left_merge_inplace(s01_ths1x, user_visit_loc_entropy, by='UserID', verbose=T)
left_merge_inplace(s01_ths1x, user_visit_dow_entropy, by='UserID', verbose=T)
left_merge_inplace(s01_ths1x, user_visit_hour_entropy, by='UserID', verbose=T)


left_merge_inplace(s01_ths1x, user_sparam_entropy, by='UserID', verbose=T)
left_merge_inplace(s01_ths1x, user_visit_param_entropy, by='UserID', verbose=T)

left_merge_inplace(s01_ths1x, s01_ths1x[, c('ID', 'ad_csum_cads_cnt', 'ad_csum_cads_y'), with=F], by='ID', verbose=T)
left_merge_inplace(s01_ths1x, s01_user, by='UserID', verbose=T)

left_merge_inplace(s01_ths1x, user_phone_cnt, by='UserID', verbose=T)
s01_ths1x[is.na(user_phone_cnt), user_phone_cnt := 0]


s01_ths2 <- s01_ths1x

s01_ths2[is.na(user_v_cnt), user_v_cnt:=0]
s01_ths2[is.na(user_cat_cnt), user_cat_cnt:=0]
s01_ths2[is.na(user_pcat_cnt), user_pcat_cnt:=0]
s01_ths2[is.na(user_visit_cat_entropy), user_visit_cat_entropy:=-1]

s01_ths2[, search_cnt := search_time_seq + search_time_rseq - 1]

s01_ths2[, cat_price_diff := Price - user_v_cat_avg_price]
s01_ths2[, ad_params_nchar := nchar(Params)]
s01_ths2[, params_nchar_diff := ad_params_nchar - param_nchar]
s01_ths2[, ad_position2 := search_ad3_cnt * 7 + Position]
s01_ths2[, cat_equal := 0]
s01_ths2[CategoryID==AdCategoryID, cat_equal := 1]
s01_ths2[is.na(CategoryID) | is.na(AdCategoryID), cat_equal := -1]

gc()
