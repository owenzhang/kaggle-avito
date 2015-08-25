#load data 
sub0 <- fread('input/sampleSubmission_HistCTR.csv')

hs0 <- fread('input/testSearchStream.tsv')
hs0[, IsClick := -1]
ts0 <- fread('input/trainSearchStream.tsv')
ts0[, ID:=-c(1:dim(ts0)[1])]

cat <- fread('input/Category.tsv')
cat$SubcategoryID <- NULL

loc <- fread('input/Location.tsv')
user <- fread('input/UserInfo.tsv')

ad0 <- fread('input/AdsInfo.tsv')
v0 <- fread('input/VisitsStream.tsv')

s0 <- fread('input/SearchInfo.tsv')

pr0 <- fread('input/PhoneRequestsStream.tsv')

conserve_ram <- F
if(full_data) conserve_ram <- T

if(full_data) {
  s01_user <- user
  s01_search <- s0
  s01_visit <- v0
  s01_phone <- pr0
  s01_ths <- rbind(ts0, hs0)
  s01_ads <- ad0
  rm(user, s0, v0, pr0, ts0, hs0, ad0)
  gc()
} else {
  sample_k <- 10
  s01_user <- user[UserID %% sample_k == 1, ]
  s01_search <- s0[UserID %% sample_k == 1, ]
  s01_visit <- v0[UserID %% sample_k == 1, ]
  s01_phone <- pr0[UserID %% sample_k == 1, ]
  system.time(s01_ts <- ts0[SearchID %in% s01_search$SearchID, ])
  system.time(s01_hs <- hs0[SearchID %in% s01_search$SearchID, ])
  s01_ths <- rbind(s01_ts, s01_hs)
  s01_ads <- ad0[AdID %in% unique(c(s01_ts$AdID, s01_hs$AdID)), ]
  rm(user, s0, v0, pr0, ts0, hs0, ad0)
  gc()
}

