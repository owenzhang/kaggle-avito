if(full_data) {
  dt1 <- s01_ths2
  base_ft <- dt1$search_time_rseq > 1 & !(dt1$UserID %% 50 == 1) & dt1$IsClick >=0
  print(sum(base_ft))
} else {
  dt1 <- s01_ths2[IsClick >=0, ]
  base_ft <- dt1$search_time_rseq > 2
}

#reponse encoding with leave-one-out and credibility adjustment
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


use_exp <- T
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
