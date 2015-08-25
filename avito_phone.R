user_phone_cnt <- s01_phone[, list(user_phone_cnt=.N), by=list(UserID)]

