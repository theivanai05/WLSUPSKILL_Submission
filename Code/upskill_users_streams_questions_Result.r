## Working on the GB Data alond from PULSEMASTER 
# Creating Recommended Question output

## Creating the Question, COuntry, Tag Master
#C_Q_Tag_M = unique(assess_dt %>% select("country","question_id","question_tags"))

#==============================================================================================================
#Day #: Questions recommendation
#==============================================================================================================

## Creating the Question, COuntry, Tag Master

Assess = ASSES_PS_EVAL_GB_28

df = melt(Assess, id.vars = c("question_id","country","org_id","role_id","submission_utc_ts","no_of_trials","points_earned","masked_user_id"))
names(df)[10] = "question_tags"
df = within(df, rm("variable"))

C_Q_Tag_M = unique(df %>% select("country","question_id","question_tags"))
C_Q_Tag_M  = C_Q_Tag_M  %>% na_if(" ") %>% na.omit() 

## Updating questions that were answered ; keeping on the the latest questions answered. 
u_q_t_M = unique(Assess[,c("masked_user_id","question_id", "no_of_trials","points_earned","submission_utc_ts")])  #  "points_earned" 
u_q_t_M[, eval("answer_datetime"):=ymd_hms(u_q_t_M$submission_utc_ts)]
u_q_t_M[, eval("answer_date"):=as_date(u_q_t_M$answer_datetime)]
u_q_t_M[,c("submission_utc_ts")] <- NULL
require(data.table)
u_q_t_M <- data.table(u_q_t_M, key=c("masked_user_id","question_id"))
setorder(u_q_t_M, masked_user_id,question_id,-answer_datetime)
u_q_t_M = u_q_t_M[, head(.SD, 1), by=c("masked_user_id","question_id")]

####1)  To Create the Matrix for Questions ANswered by Users D.T = u_q_t_M
new_var <- "qns_ans"
u_q_t_M[,(new_var):=dplyr::case_when(
  qns_ans = points_earned == 10 ~ 1,
  qns_ans = points_earned == 5 ~ 1,
  qns_ans = points_earned == 0 ~ 0)]

## After Recommendation, need to pull out the Questions now: 
Questions_Recommended = merge(C_Q_Tag_M, Recommended_Tags, by= c("question_tags"),allow.cartesian=TRUE)

## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
Questions_Recommended$masked_user_id <- as.factor(Questions_Recommended$masked_user_id)
Questions_Recommended$question_id <- as.factor(Questions_Recommended$question_id)
u_q_t_M$question_id <- as.factor(u_q_t_M$question_id)
Questions_Recommended_w_qns_ans = unique(merge(Questions_Recommended,u_q_t_M,  by= c("masked_user_id","question_id"),all.x = TRUE))
Questions_Recommended_w_qns_ans[,c("no_of_trials","points_earned")] <- NULL
Questions_Recommended_w_qns_ans = unique(Questions_Recommended_w_qns_ans)

#filter(u_q_t_M, qns_ans == "1")
#filter(pulsescore_Master_GB_O28_DO, score != "0")
Questions_Recommended_w_qns_ans_GB_Users = filter(Questions_Recommended_w_qns_ans, country == "GB")
setorder(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id,question_id,-qns_ans)


#finding Qns Recommended for Users on Day 1  
    Qns_Reco_Day = filter(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id %in% Demo_UsersDay0) %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)
    Qns_Reco_Day$qns_ans[Qns_Reco_Day$qns_ans == 1] <- 3 # questions already answered 
    Qns_Reco_Day$qns_ans[is.na(Qns_Reco_Day$qns_ans)] <- 2 # Questions not yet Seen by User 
    Qns_Reco_Day$qns_ans[Qns_Reco_Day$qns_ans == 0] <- 1 # Questions Received but not Attempted 
    Qns_Reco_Day =   Qns_Reco_Day %>% group_by(masked_user_id)%>%arrange(qns_ans) # Arranging the Questions so that RECO is in the order of 1 , 2, 3 
    
    # Remove Duplicate Questions for same user 
    Qns_Reco_Day = Qns_Reco_Day  %>% distinct(question_id, masked_user_id , .keep_all = TRUE)
    
    #View(Qns_Reco_Day)
    #unique(Qns_Reco_Day$masked_user_id)
    
    ## Finding out the top 50 Questions for each User 
    ## Picking up the first 50 lines for each user 
    # ==> https://stackoverflow.com/questions/14800161/select-the-top-n-values-by-group
    
    #To be used for Final Recommendation 
    #To be used Question and Tag recommendations and Evaluation ==> 2 questions per User <== 
    require(data.table)
    d <- data.table(Qns_Reco_Day, key="masked_user_id")
    Qns_Reco_Day_50Q = d[, head(.SD, 50), by=masked_user_id]
    
    
## making Stream view Master long again : 
    
    ## Creating the Question, COuntry, Tag Master
    C_S_Tag_M = unique(views_dt_7 %>% select("country","deck_id","tag1","tag2","tag3","tag4","tag5","tag6","tag7"))
    
    ## Reshaping and creating a Long Strewam Tag Master table 
    C_S_Tag_M<- melt(C_S_Tag_M, id=c("country","deck_id"))              # long format
    C_S_Tag_M [,c("variable")] <- NULL                                  # Removed the Tags field variable names 
    names(C_S_Tag_M)[names(C_S_Tag_M) == "value"] <- "question_tags"    # Renamed the Actual Tag Field
    C_S_Tag_M = unique(C_S_Tag_M)                                       # Removed duplicates
    C_S_Tag_M = C_S_Tag_M %>% drop_na(question_tags)                    # Removed the NA columns 
    setorder(C_S_Tag_M, country, deck_id, question_tags)                # Reordering the fields 
    
    # Creating Recommended Stream output
    
    ## After Recommendation, need to pull out the Streams now: 
    Streams_Recommended = merge(C_S_Tag_M, Recommended_Tags, by= c("question_tags"),allow.cartesian=TRUE)
    
    ## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
    Streams_Recommended_w_views = unique(merge(Streams_Recommended,u_d_a_M,  by= c("masked_user_id","deck_id"),all.x = TRUE))
    Streams_Recommended_w_views[,c("action")] <- NULL
    Streams_Recommended_w_views = unique(Streams_Recommended_w_views)
    
    #filter(u_d_a_M, completed == "1")
    #filter(pulsescore_Master_GB, score != "0")
    Streams_Recommended_w_views_GB_Users = filter(Streams_Recommended_w_views, country == "GB")
    setorder(Streams_Recommended_w_views_GB_Users, masked_user_id,deck_id,-completed)
    
    # Remove Duplicate Questions for same user 
    Stream_Reco_Day = Streams_Recommended_w_views_GB_Users  %>% distinct(deck_id, masked_user_id , .keep_all = TRUE)
    
    d <- data.table(Stream_Reco_Day, key="masked_user_id")
    Stream_Reco_Day_50S = d[, head(.SD, 50), by=masked_user_id]
    
    #Removing fields  not Required for Challenge of the Day. 
    Qns_Reco_Day_50Q[,c("question_tags","country","answer_datetime","answer_date","qns_ans")] <- NULL
    Stream_Reco_Day_50S[,c("question_tags","country","completed")] <- NULL
    
    

# Creation of Synthetic Data After Day 1         
    
    SYNTHETIC_activity_data <- create_activity_data_from_recomm_questions(Qns_Reco_Day_50Q,recDate)
    
    names(SYNTHETIC_activity_data)[names(SYNTHETIC_activity_data) == "question_tags"] <- "tag1"
    colnames_df <- t(t(colnames(ASSES_PS_EVAL_GB_28)))
    setcolorder(SYNTHETIC_activity_data, colnames_df)
    SYNTHETIC_activity_data[,c("rating","qns_ans","answer_datetime","answer_date")] <- NULL
    
    #Adding observations using rbind() function  
    ASSESS_W_RECO_ANS<- rbind(ASSES_PS_EVAL_GB_28,SYNTHETIC_activity_data)
    