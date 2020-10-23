# Identifying  the set of Questions and Decks Viewed and Not Viewed by Users : 
# assessment data set explanation 
# Trail 1 = 10 marks 
# Trail 2 = 5 marks 
# Trail 3 / 4 = 0 marks 

# Unique Users, Questions , # of Trials and points earned  from assessment data 
u_q_t_M = unique(assess_dt[,c("masked_user_id","question_id", "no_of_trials","points_earned")])  #  "points_earned" 
#View(u_q_t_M)

####1)  To Create the Matrix for Questions ANswered by Users D.T = u_q_t_M
new_var <- "qns_ans"
u_q_t_M[,(new_var):=dplyr::case_when(
  qns_ans = points_earned == 10 ~ 1,
  qns_ans = points_earned == 5 ~ 1,
  qns_ans = points_earned == 0 ~ 0)]

####2)  To Create the Matrix for Streams Viewed by Users D.T = u_d_a_M

# Unique Users, decks assessed and actiona performed from streamz data 
u_d_a_M = unique(views_dt[,c("masked_user_id","deck_id","action")])
#View(u_d_a_M)

new_var <- "completed"
u_d_a_M[,(new_var):=dplyr::case_when(
  completed = action == "STREAM_COMPLETE" ~ 1,
  completed = action == "STREAM_RECIEVED" ~ 0)]

#####3) Understanding the Master Users 
Country_User_M = unique(master_dt[,c("country","masked_user_id")])
